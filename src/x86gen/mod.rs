use std::io::{Write, Result};
use std::collections::{HashMap, HashSet};

use koopa::ir::*;
use koopa::back::NameManager;

use super::asm::{AsmEmit, peephole2};

// ─── x86-64 Structured Instruction ──────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum X64Inst {
    // Arithmetic (op src, dst) — AT&T syntax: op src, dst
    Addq { src: String, dst: String },
    Subq { src: String, dst: String },
    Imulq { src: String, dst: String },
    Idivq { divisor: String },
    Cqto,
    Andq { src: String, dst: String },
    Orq  { src: String, dst: String },
    // Compare + setcc
    Cmpq { a: String, b: String },
    Sete  { dst: String },
    Setne { dst: String },
    Setl  { dst: String },
    Setg  { dst: String },
    Setle { dst: String },
    Setge { dst: String },
    Movzbq { src: String, dst: String },
    // Data movement
    Movq  { src: String, dst: String },
    Leaq  { src: String, dst: String },   // leaq symbol(%rip), dst
    LoadMem { base: String, offset: i32, dst: String }, // movq offset(base), dst
    StoreMem { src: String, base: String, offset: i32 }, // movq src, offset(base)
    LoadGlobal { symbol: String, dst: String },
    StoreGlobal { src: String, symbol: String },
    // Stack
    Pushq(String),
    Popq(String),
    SubqImm { dst: String, imm: i32 },
    AddqImm { dst: String, imm: i32 },
    MovqImm { dst: String, imm: i32 },
    // Branch
    Je(String),
    Jne(String),
    Jmp(String),
    Call(String),
    Ret,
    // Labels / Directives
    Label(String),
    Directive(String),
}

impl AsmEmit for X64Inst {
    fn emit(&self, writer: &mut dyn Write) -> Result<()> {
        match self {
            X64Inst::Addq { src, dst }   => writeln!(writer, "\taddq {}, {}", src, dst),
            X64Inst::Subq { src, dst }   => writeln!(writer, "\tsubq {}, {}", src, dst),
            X64Inst::Imulq { src, dst }  => writeln!(writer, "\timulq {}, {}", src, dst),
            X64Inst::Idivq { divisor }    => writeln!(writer, "\tidivq {}", divisor),
            X64Inst::Cqto                => writeln!(writer, "\tcqto"),
            X64Inst::Andq { src, dst }   => writeln!(writer, "\tandq {}, {}", src, dst),
            X64Inst::Orq  { src, dst }   => writeln!(writer, "\torq {}, {}", src, dst),
            X64Inst::Cmpq { a, b }       => writeln!(writer, "\tcmpq {}, {}", a, b),
            X64Inst::Sete  { dst }       => writeln!(writer, "\tsete {}", dst),
            X64Inst::Setne { dst }       => writeln!(writer, "\tsetne {}", dst),
            X64Inst::Setl  { dst }       => writeln!(writer, "\tsetl {}", dst),
            X64Inst::Setg  { dst }       => writeln!(writer, "\tsetg {}", dst),
            X64Inst::Setle { dst }       => writeln!(writer, "\tsetle {}", dst),
            X64Inst::Setge { dst }       => writeln!(writer, "\tsetge {}", dst),
            X64Inst::Movzbq { src, dst } => writeln!(writer, "\tmovzbq {}, {}", src, dst),
            X64Inst::Movq  { src, dst }  => writeln!(writer, "\tmovq {}, {}", src, dst),
            X64Inst::Leaq  { src, dst }  => writeln!(writer, "\tleaq {}(%rip), {}", src, dst),
            X64Inst::LoadMem { base, offset, dst } => writeln!(writer, "\tmovq {}({}), {}", offset, base, dst),
            X64Inst::StoreMem { src, base, offset } => writeln!(writer, "\tmovq {}, {}({})", src, offset, base),
            X64Inst::LoadGlobal { symbol, dst } => writeln!(writer, "\tleaq {}(%rip), {}", symbol, dst),
            X64Inst::StoreGlobal { src, symbol } => {
                writeln!(writer, "\tleaq {}(%rip), %rcx", symbol)?;
                writeln!(writer, "\tmovq {}, (%rcx)", src)
            },
            X64Inst::Pushq(r)    => writeln!(writer, "\tpushq {}", r),
            X64Inst::Popq(r)     => writeln!(writer, "\tpopq {}", r),
            X64Inst::SubqImm { dst, imm } => writeln!(writer, "\tsubq ${}, {}", imm, dst),
            X64Inst::AddqImm { dst, imm } => writeln!(writer, "\taddq ${}, {}", imm, dst),
            X64Inst::MovqImm { dst, imm } => writeln!(writer, "\tmovq ${}, {}", imm, dst),
            X64Inst::Je(l)    => writeln!(writer, "\tje {}", l),
            X64Inst::Jne(l)   => writeln!(writer, "\tjne {}", l),
            X64Inst::Jmp(l)   => writeln!(writer, "\tjmp {}", l),
            X64Inst::Call(s)  => writeln!(writer, "\tcall {}", s),
            X64Inst::Ret      => writeln!(writer, "\tret"),
            X64Inst::Label(s) => writeln!(writer, "{}:", s),
            X64Inst::Directive(s) => writeln!(writer, "{}", s),
        }
    }
}

// ─── Peephole Optimizations ──────────────────────────────────────────────────

fn optimize_x64(insts: &mut Vec<X64Inst>) {
    // Rule 1: Dead store elimination — consecutive stores to same rsp offset
    peephole2(insts, |a, b| {
        if let (X64Inst::StoreMem { base: b1, offset: o1, .. }, X64Inst::StoreMem { base: b2, offset: o2, .. }) = (a, b) {
            if b1 == "%rsp" && b2 == "%rsp" && o1 == o2 { return Some(vec![b.clone()]); }
        }
        None
    });
    // Rule 2: Redundant load after store to same location
    peephole2(insts, |a, b| {
        if let (X64Inst::StoreMem { src, base: b1, offset: o1 }, X64Inst::LoadMem { dst, base: b2, offset: o2 }) = (a, b) {
            if b1 == b2 && o1 == o2 && src == dst { return Some(vec![a.clone()]); }
        }
        None
    });
    // Rule 3: MovqImm $0, dst; Addq dst, src → (remove mov, keep add)
    peephole2(insts, |a, b| {
        if let (X64Inst::MovqImm { dst: d1, imm: 0 }, X64Inst::Addq { src: s2, dst: d2 }) = (a, b) {
            if d1 == d2 && d2 == s2 { return Some(vec![b.clone()]); }
        }
        None
    });
    // Rule 4: Remove redundant movq: movq %rax, %rax
    peephole2(insts, |a, _b| {
        if let X64Inst::Movq { src, dst } = a {
            if src == dst { return Some(vec![]); }
        }
        None
    });
    // Rule 5: Pushq %rbp; Movq %rsp, %rbp → pushq %rbp (Movq already expected)
    // (Skip mov to self: handled by rule 4)
}

// ─── Register allocation state ───────────────────────────────────────────────

const CALLER_SAVED_REGS: &[&str] = &["r8", "r9", "r10", "r11"];
const CALLEE_SAVED_REGS: &[&str] = &["r12", "r13", "r14", "r15", "rbx"];
const ARG_REGS: &[&str] = &["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

#[derive(Debug, Clone)]
struct LiveInterval {
    value: Value, start: usize, end: usize, reg: Option<String>, spilled: bool,
}

struct GenX64Info {
    stackmem: usize, stackmap: HashMap<Value, usize>, allocmap: HashMap<Value, usize>,
    globalmap: HashMap<Value, usize>, namemgr: NameManager,
    reg_map: HashMap<Value, String>, spill_map: HashMap<Value, usize>,
    used_callee_regs: Vec<String>, used_caller_regs: Vec<String>,
    caller_saved_set: HashSet<String>, next_spill_offset: usize,
}

impl GenX64Info {
    fn new() -> Self {
        let cs: HashSet<_> = CALLER_SAVED_REGS.iter().map(|s| s.to_string()).collect();
        GenX64Info {
            stackmem: 0, stackmap: HashMap::new(), allocmap: HashMap::new(),
            globalmap: HashMap::new(), namemgr: NameManager::new(),
            reg_map: HashMap::new(), spill_map: HashMap::new(),
            used_callee_regs: Vec::new(), used_caller_regs: Vec::new(),
            caller_saved_set: cs, next_spill_offset: 0,
        }
    }
    fn clear_local_info(&mut self) {
        self.stackmap.clear(); self.allocmap.clear(); self.stackmem = 0;
        self.reg_map.clear(); self.spill_map.clear();
        self.used_callee_regs.clear(); self.used_caller_regs.clear();
        self.next_spill_offset = 0;
    }
}

// ─── Liveness + Linear scan ──────────────────────────────────────────────────

fn compute_live_intervals(func_data: &FunctionData) -> Vec<LiveInterval> {
    let dfg = func_data.dfg();
    let mut intervals: HashMap<Value, LiveInterval> = HashMap::new();
    let mark_use = |m: &mut HashMap<Value, LiveInterval>, v: Value, idx: usize| {
        let e = m.entry(v).or_insert(LiveInterval { value: v, start: usize::MAX, end: 0, reg: None, spilled: false });
        if e.start == usize::MAX { e.start = idx; } e.end = idx;
    };
    let mark_def = |m: &mut HashMap<Value, LiveInterval>, v: Value, idx: usize| {
        let e = m.entry(v).or_insert(LiveInterval { value: v, start: usize::MAX, end: 0, reg: None, spilled: false });
        if e.start == usize::MAX { e.start = idx; } e.end = e.end.max(idx);
    };
    let mut inst_idx = 0usize;
    for param in func_data.params() { mark_def(&mut intervals, *param, 0); }
    for (_bb, node) in func_data.layout().bbs() {
        for inst_val in node.insts().keys() {
            let id = dfg.value(*inst_val);
            mark_def(&mut intervals, *inst_val, inst_idx);
            match id.kind() {
                ValueKind::Integer(_) | ValueKind::Aggregate(_) | ValueKind::ZeroInit(_) => {},
                ValueKind::Alloc(_) | ValueKind::GlobalAlloc(_) => {},
                ValueKind::Load(v) => { mark_use(&mut intervals, v.src(), inst_idx); },
                ValueKind::Store(v) => { mark_use(&mut intervals, v.value(), inst_idx); mark_use(&mut intervals, v.dest(), inst_idx); },
                ValueKind::GetPtr(v) => { mark_use(&mut intervals, v.src(), inst_idx); mark_use(&mut intervals, v.index(), inst_idx); },
                ValueKind::GetElemPtr(v) => { mark_use(&mut intervals, v.src(), inst_idx); mark_use(&mut intervals, v.index(), inst_idx); },
                ValueKind::Binary(v) => { mark_use(&mut intervals, v.lhs(), inst_idx); mark_use(&mut intervals, v.rhs(), inst_idx); },
                ValueKind::Branch(v) => { mark_use(&mut intervals, v.cond(), inst_idx); },
                ValueKind::Jump(_) => {},
                ValueKind::Call(v) => { for arg in v.args() { mark_use(&mut intervals, *arg, inst_idx); } },
                ValueKind::Return(v) => { if let Some(r) = v.value() { mark_use(&mut intervals, r, inst_idx); } },
                ValueKind::Undef(_) | ValueKind::FuncArgRef(_) | ValueKind::BlockArgRef(_) => {}
            };
            inst_idx += 1;
        }
    }
    intervals.into_values().filter(|i| i.start != usize::MAX).collect()
}

fn linear_scan(func_data: &FunctionData, info: &mut GenX64Info) {
    let mut intervals = compute_live_intervals(func_data);
    if intervals.is_empty() { return; }
    intervals.sort_by_key(|i| i.start);
    let mut free_regs: Vec<String> = Vec::new();
    for reg in CALLEE_SAVED_REGS.iter().rev() { free_regs.push(reg.to_string()); }
    for reg in CALLER_SAVED_REGS.iter().rev() { free_regs.push(reg.to_string()); }
    let mut active: Vec<LiveInterval> = Vec::new();
    for mut interval in intervals {
        let mut i = 0;
        while i < active.len() {
            if active[i].end < interval.start {
                let expired = active.remove(i);
                if let Some(ref reg) = expired.reg { free_regs.push(reg.clone()); }
            } else { i += 1; }
        }
        if !free_regs.is_empty() {
            let reg = free_regs.pop().unwrap();
            interval.reg = Some(reg.clone());
            info.reg_map.insert(interval.value, reg.clone());
            if info.caller_saved_set.contains(&reg) {
                if !info.used_caller_regs.contains(&reg) { info.used_caller_regs.push(reg.clone()); }
            } else {
                if !info.used_callee_regs.contains(&reg) { info.used_callee_regs.push(reg.clone()); }
            }
        } else {
            let spill_idx = active.iter().enumerate().max_by_key(|(_, a)| a.end).unwrap().0;
            if active[spill_idx].end > interval.end {
                let mut spilled = active.remove(spill_idx);
                let reg = spilled.reg.take().unwrap();
                let slot = info.next_spill_offset; info.next_spill_offset += 4;
                info.spill_map.insert(spilled.value, slot);
                spilled.spilled = true;
                interval.reg = Some(reg.clone());
                info.reg_map.insert(interval.value, reg.clone());
                active.push(spilled);
            } else {
                let slot = info.next_spill_offset; info.next_spill_offset += 4;
                info.spill_map.insert(interval.value, slot);
                interval.spilled = true;
            }
        }
        active.push(interval);
        active.sort_by_key(|a| a.end);
    }
}

// ─── Codegen (IR → Vec<X64Inst>) ────────────────────────────────────────────

trait GenerateAsm {
    fn generate(&self, info: &mut GenX64Info, prog: &Program, buf: &mut Vec<X64Inst>);
}

impl GenerateAsm for koopa::ir::Program {
    fn generate(&self, info: &mut GenX64Info, prog: &Program, buf: &mut Vec<X64Inst>) {
        buf.push(X64Inst::Directive("\t.data".into()));
        for value in self.inst_layout() {
            let vd = &*self.borrow_value(value.clone());
            let name = info.namemgr.value_name(vd)[1..].to_string();
            if let ValueKind::GlobalAlloc(v) = vd.kind() {
                info.globalmap.insert(value.clone(), 0);
                buf.push(X64Inst::Directive(format!("\t.globl {}", name)));
                buf.push(X64Inst::Directive("\t.align 4".into()));
                buf.push(X64Inst::Label(name.clone()));
                match self.borrow_value(v.init()).kind() {
                    ValueKind::Integer(v) => buf.push(X64Inst::Directive(format!("\t.long {}", v.value()))),
                    ValueKind::ZeroInit(_) => {
                        let sz = match vd.ty().kind() { TypeKind::Pointer(ty) => ty.size(), _ => panic!() };
                        buf.push(X64Inst::Directive(format!("\t.zero {}", sz)));
                    },
                    ValueKind::Aggregate(v) => emit_agg(v, self, buf),
                    _ => todo!()
                }
            }
        }
        buf.push(X64Inst::Directive("\t.text".into()));
        for (_, fd) in self.funcs() {
            if fd.layout().entry_bb().is_none() { continue; }
            info.namemgr.enter_func_scope();
            fd.generate(info, prog, buf);
            info.namemgr.exit_func_scope();
            info.clear_local_info();
            buf.push(X64Inst::Directive("".into()));
        }
    }
}

fn emit_agg(agg: &values::Aggregate, prog: &Program, buf: &mut Vec<X64Inst>) {
    for v in agg.elems() {
        match prog.borrow_value(v.clone()).kind() {
            ValueKind::Integer(v) => buf.push(X64Inst::Directive(format!("\t.long {}", v.value()))),
            ValueKind::Aggregate(v) => emit_agg(v, prog, buf),
            ValueKind::ZeroInit(_) => buf.push(X64Inst::Directive(format!("\t.zero {}", prog.borrow_value(v.clone()).ty().size()))),
            _ => todo!()
        }
    }
}

impl GenerateAsm for koopa::ir::FunctionData {
    fn generate(&self, info: &mut GenX64Info, prog: &Program, buf: &mut Vec<X64Inst>) {
        let fname = self.name()[1..].to_string();
        buf.push(X64Inst::Directive(format!("\t.globl {}", fname)));
        buf.push(X64Inst::Label(fname.clone()));

        let mut offset = 0;
        for (v, data) in self.dfg().values().iter() {
            match data.kind() {
                ValueKind::Integer(_) | ValueKind::Aggregate(_) => continue,
                ValueKind::Alloc(_) => {
                    if let TypeKind::Pointer(ty) = data.ty().kind() { info.allocmap.insert(v.clone(), offset); offset += ty.size(); }
                },
                _ => (),
            }
            info.stackmap.insert(v.clone(), offset);
            offset += data.ty().size();
        }
        info.stackmem = offset;
        linear_scan(self, info);
        info.next_spill_offset += info.stackmem;

        // Prologue
        buf.push(X64Inst::Pushq("%rbp".into()));
        buf.push(X64Inst::Movq { src: "%rsp".into(), dst: "%rbp".into() });
        for reg in &info.used_callee_regs {
            buf.push(X64Inst::Pushq(format!("%{}", reg)));
        }
        let total = info.stackmem + info.next_spill_offset - info.stackmem;
        let aligned = ((total + 15) & !15) as i32;
        if aligned > 0 { buf.push(X64Inst::SubqImm { dst: "%rsp".into(), imm: aligned }); }

        for (i, param) in self.params().iter().enumerate() {
            let arg = if i < 6 { format!("%{}", ARG_REGS[i]) } else {
                format!("{}(%rbp)", 16 + (i - 6) * 8)
            };
            buf.push(X64Inst::Movq { src: arg, dst: "%rax".into() });
            buf.push(X64Inst::StoreMem { src: "%rax".into(), base: "%rsp".into(), offset: info.stackmap[param] as i32 });
        }

        for (bb, node) in self.layout().bbs() {
            let bb_name = info.namemgr.bb_name(self.dfg().bb(*bb));
            buf.push(X64Inst::Label(format!("\t.L{}_{}", fname, &bb_name[1..])));
            for inst_val in node.insts().keys() {
                inst_val.generate(self, info, prog, buf);
            }
        }
    }
}

trait _Gen {
    fn generate(&self, funcdata: &FunctionData, info: &mut GenX64Info, prog: &Program, buf: &mut Vec<X64Inst>);
}

impl _Gen for Value {
    fn generate(&self, funcdata: &FunctionData, info: &mut GenX64Info, prog: &Program, buf: &mut Vec<X64Inst>) {
        let inst = funcdata.dfg().value(self.clone());
        match inst.kind() {
            ValueKind::Integer(_) | ValueKind::Aggregate(_) => {},
            ValueKind::Return(v) => {
                if let Some(v) = v.value() { load_operand(v, funcdata, prog, info, buf, "rax"); }
                let total = info.stackmem + info.next_spill_offset - info.stackmem;
                let aligned = ((total + 15) & !15) as i32;
                if aligned > 0 { buf.push(X64Inst::AddqImm { dst: "%rsp".into(), imm: aligned }); }
                for reg in info.used_callee_regs.iter().rev() {
                    buf.push(X64Inst::Popq(format!("%{}", reg)));
                }
                buf.push(X64Inst::Popq("%rbp".into()));
                buf.push(X64Inst::Ret);
            },
            ValueKind::Alloc(_) => {
                buf.push(X64Inst::Leaq { src: format!("{}(%rsp)", info.allocmap[self]), dst: "%rax".into() });
                store_result(*self, info, buf, "rax");
            },
            ValueKind::Load(v) => {
                if info.globalmap.get(&v.src()).is_some() {
                    let sym = prog.borrow_value(v.src()).name().as_ref().unwrap()[1..].to_string();
                    buf.push(X64Inst::LoadGlobal { symbol: sym, dst: "%rax".into() });
                    buf.push(X64Inst::LoadMem { base: "%rax".into(), offset: 0, dst: "%rax".into() });
                } else {
                    load_operand(v.src(), funcdata, prog, info, buf, "rax");
                    buf.push(X64Inst::LoadMem { base: "%rax".into(), offset: 0, dst: "%rax".into() });
                }
                store_result(*self, info, buf, "rax");
            },
            ValueKind::Store(v) => {
                let mut is_agg = false;
                match funcdata.dfg().value(v.value()).kind() {
                    ValueKind::Aggregate(agg) => { is_agg = true; emit_agg_store2(funcdata, info, prog, buf, agg, info.allocmap[&v.dest()]); },
                    ValueKind::Integer(vi) => buf.push(X64Inst::MovqImm { dst: "%rax".into(), imm: vi.value() as i32 }),
                    _ => load_operand(v.value(), funcdata, prog, info, buf, "rax"),
                }
                if info.globalmap.get(&v.dest()).is_some() {
                    let sym = prog.borrow_value(v.dest()).name().as_ref().unwrap()[1..].to_string();
                    buf.push(X64Inst::StoreGlobal { src: "%rax".into(), symbol: sym });
                } else if !is_agg {
                    load_operand(v.dest(), funcdata, prog, info, buf, "rcx");
                    buf.push(X64Inst::StoreMem { src: "%rax".into(), base: "%rcx".into(), offset: 0 });
                }
            },
            ValueKind::Binary(v) => {
                load_operand(v.lhs(), funcdata, prog, info, buf, "rax");
                load_operand(v.rhs(), funcdata, prog, info, buf, "rcx");
                match v.op() {
                    BinaryOp::Add => buf.push(X64Inst::Addq { src: "%rcx".into(), dst: "%rax".into() }),
                    BinaryOp::Sub => buf.push(X64Inst::Subq { src: "%rcx".into(), dst: "%rax".into() }),
                    BinaryOp::Mul => buf.push(X64Inst::Imulq { src: "%rcx".into(), dst: "%rax".into() }),
                    BinaryOp::Div => { buf.push(X64Inst::Cqto); buf.push(X64Inst::Idivq { divisor: "%rcx".into() }); },
                    BinaryOp::Mod => { buf.push(X64Inst::Cqto); buf.push(X64Inst::Idivq { divisor: "%rcx".into() }); buf.push(X64Inst::Movq { src: "%rdx".into(), dst: "%rax".into() }); },
                    BinaryOp::And => buf.push(X64Inst::Andq { src: "%rcx".into(), dst: "%rax".into() }),
                    BinaryOp::Or  => buf.push(X64Inst::Orq  { src: "%rcx".into(), dst: "%rax".into() }),
                    BinaryOp::Eq | BinaryOp::NotEq | BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Le | BinaryOp::Ge => {
                        buf.push(X64Inst::Cmpq { a: "%rcx".into(), b: "%rax".into() });
                        match v.op() {
                            BinaryOp::Eq => buf.push(X64Inst::Sete { dst: "%al".into() }),
                            BinaryOp::NotEq => buf.push(X64Inst::Setne { dst: "%al".into() }),
                            BinaryOp::Lt => buf.push(X64Inst::Setl { dst: "%al".into() }),
                            BinaryOp::Gt => buf.push(X64Inst::Setg { dst: "%al".into() }),
                            BinaryOp::Le => buf.push(X64Inst::Setle { dst: "%al".into() }),
                            BinaryOp::Ge => buf.push(X64Inst::Setge { dst: "%al".into() }),
                            _ => unreachable!(),
                        }
                        buf.push(X64Inst::Movzbq { src: "%al".into(), dst: "%rax".into() });
                    },
                    _ => todo!()
                }
                store_result(*self, info, buf, "rax");
            },
            ValueKind::Branch(v) => {
                load_operand(v.cond(), funcdata, prog, info, buf, "rax");
                buf.push(X64Inst::Cmpq { a: "$0".into(), b: "%rax".into() });
                let tb = info.namemgr.bb_name(funcdata.dfg().bb(v.true_bb()));
                let fb = info.namemgr.bb_name(funcdata.dfg().bb(v.false_bb()));
                let fname = funcdata.name()[1..].to_string();
                buf.push(X64Inst::Jne(format!(".L{}_{}", fname, &tb[1..])));
                buf.push(X64Inst::Jmp(format!(".L{}_{}", fname, &fb[1..])));
            },
            ValueKind::Jump(v) => {
                let t = info.namemgr.bb_name(funcdata.dfg().bb(v.target()));
                buf.push(X64Inst::Jmp(format!(".L{}_{}", funcdata.name()[1..].to_string(), &t[1..])));
            },
            ValueKind::Call(v) => {
                for (value, reg) in &info.reg_map {
                    if info.caller_saved_set.contains(reg) {
                        if let Some(off) = info.stackmap.get(value) {
                            buf.push(X64Inst::StoreMem { src: format!("%{}", reg), base: "%rsp".into(), offset: *off as i32 });
                        }
                    }
                }
                let mut stack_args = 0i32;
                for (i, arg) in v.args().iter().enumerate() {
                    if i < 6 {
                        load_operand(*arg, funcdata, prog, info, buf, ARG_REGS[i]);
                    } else {
                        load_operand(*arg, funcdata, prog, info, buf, "rax");
                        buf.push(X64Inst::Pushq("%rax".into()));
                        stack_args += 1;
                    }
                }
                let push_bytes = stack_args * 8;
                if push_bytes % 16 != 0 { buf.push(X64Inst::SubqImm { dst: "%rsp".into(), imm: 8 }); }
                buf.push(X64Inst::Call(prog.func(v.callee()).name()[1..].to_string()));
                let clean = push_bytes + if push_bytes % 16 != 0 { 8 } else { 0 };
                if clean > 0 { buf.push(X64Inst::AddqImm { dst: "%rsp".into(), imm: clean }); }
                if !funcdata.dfg().value(self.clone()).ty().is_unit() { store_result(*self, info, buf, "rax"); }
            },
            ValueKind::GetElemPtr(v) => {
                load_operand(v.index(), funcdata, prog, info, buf, "rax");
                let step = if info.globalmap.get(&v.src()).is_some() {
                    match prog.borrow_value(v.src()).ty().kind() { TypeKind::Pointer(ty) => match ty.kind() { TypeKind::Array(ty, _) => ty.size(), _ => panic!() }, _ => panic!() }
                } else {
                    match funcdata.dfg().value(v.src()).ty().kind() { TypeKind::Pointer(ty) => match ty.kind() { TypeKind::Array(ty, _) => ty.size(), _ => panic!() }, _ => panic!() }
                };
                buf.push(X64Inst::Imulq { src: format!("${}", step), dst: "%rax".into() });
                load_operand(v.src(), funcdata, prog, info, buf, "rcx");
                buf.push(X64Inst::Addq { src: "%rcx".into(), dst: "%rax".into() });
                store_result(*self, info, buf, "rax");
            },
            ValueKind::GetPtr(v) => {
                load_operand(v.index(), funcdata, prog, info, buf, "rax");
                match funcdata.dfg().value(v.src()).ty().kind() {
                    TypeKind::Pointer(ty) => { buf.push(X64Inst::Imulq { src: format!("${}", ty.size()), dst: "%rax".into() }); },
                    _ => panic!(),
                }
                load_operand(v.src(), funcdata, prog, info, buf, "rcx");
                buf.push(X64Inst::Addq { src: "%rcx".into(), dst: "%rax".into() });
                store_result(*self, info, buf, "rax");
            },
            _ => todo!(),
        }
    }
}

fn emit_agg_store2(funcdata: &FunctionData, info: &mut GenX64Info, prog: &Program, buf: &mut Vec<X64Inst>, agg: &values::Aggregate, mut base: usize) {
    for v in agg.elems() {
        match funcdata.dfg().value(v.clone()).kind() {
            ValueKind::Aggregate(v) => emit_agg_store2(funcdata, info, prog, buf, v, base),
            ValueKind::Integer(v) => {
                buf.push(X64Inst::MovqImm { dst: "%rax".into(), imm: v.value() as i32 });
                buf.push(X64Inst::StoreMem { src: "%rax".into(), base: "%rsp".into(), offset: base as i32 });
                base += 4;
            },
            _ => {
                load_operand(*v, funcdata, prog, info, buf, "rax");
                buf.push(X64Inst::StoreMem { src: "%rax".into(), base: "%rsp".into(), offset: base as i32 });
                base += 4;
            },
        }
    }
}

// ─── Helpers ─────────────────────────────────────────────────────────────────

fn load_operand(value: Value, funcdata: &FunctionData, _prog: &Program, info: &GenX64Info, buf: &mut Vec<X64Inst>, dest: &str) {
    if let ValueKind::Integer(v) = funcdata.dfg().value(value).kind() {
        buf.push(X64Inst::MovqImm { dst: format!("%{}", dest), imm: v.value() as i32 }); return;
    }
    if let Some(reg) = info.reg_map.get(&value) {
        buf.push(X64Inst::Movq { src: format!("%{}", reg), dst: format!("%{}", dest) });
    } else if let Some(off) = info.spill_map.get(&value) {
        buf.push(X64Inst::LoadMem { base: "%rsp".into(), offset: (*off + info.stackmem) as i32, dst: format!("%{}", dest) });
    } else if let Some(off) = info.stackmap.get(&value) {
        buf.push(X64Inst::LoadMem { base: "%rsp".into(), offset: *off as i32, dst: format!("%{}", dest) });
    } else {
        panic!("Cannot load value");
    }
}

fn store_result(value: Value, info: &mut GenX64Info, buf: &mut Vec<X64Inst>, src: &str) {
    if let Some(off) = info.stackmap.get(&value) {
        buf.push(X64Inst::StoreMem { src: format!("%{}", src), base: "%rsp".into(), offset: *off as i32 });
    }
    if let Some(reg) = info.reg_map.get(&value) {
        buf.push(X64Inst::Movq { src: format!("%{}", src), dst: format!("%{}", reg) });
    }
}

// ─── Public API ──────────────────────────────────────────────────────────────

pub fn generator_x64(prog: &Program, writer: &mut impl Write) -> Result<()> {
    let mut info = GenX64Info::new();
    let mut buf: Vec<X64Inst> = Vec::new();
    prog.generate(&mut info, prog, &mut buf);
    optimize_x64(&mut buf);
    for inst in &buf {
        inst.emit(writer)?;
    }
    Ok(())
}
