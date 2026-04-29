use std::io::{Write, Result};
use std::collections::{HashMap, HashSet};

use koopa::ir::*;
use koopa::back::NameManager;

use super::asm::{AsmEmit, peephole2, peephole3};

// ─── RISC-V Structured Instruction ───────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum RiscvInst {
    // Arithmetic (rd = rs1 op rs2)
    Add  { rd: String, rs1: String, rs2: String },
    Sub  { rd: String, rs1: String, rs2: String },
    Mul  { rd: String, rs1: String, rs2: String },
    Div  { rd: String, rs1: String, rs2: String },
    Rem  { rd: String, rs1: String, rs2: String },
    And  { rd: String, rs1: String, rs2: String },
    Or   { rd: String, rs1: String, rs2: String },
    Slt  { rd: String, rs1: String, rs2: String },
    Seqz { rd: String, rs: String },
    Snez { rd: String, rs: String },
    // Arithmetic with immediate
    Addi { rd: String, rs: String, imm: i32 },
    Li   { rd: String, imm: i32 },
    // Move
    Mv { rd: String, rs: String },
    // Memory
    Lw { rd: String, base: String, offset: i32 },
    Sw { rs2: String, base: String, offset: i32 },
    // Address load
    La { rd: String, symbol: String },
    // Branch / Jump
    Beqz  { rs: String, label: String },
    Bnez  { rs: String, label: String },
    J     { label: String },
    // Function
    Call { symbol: String },
    Ret,
    // Pseudo — prologue/epilogue
    StackAlloc(i32),  // addi sp, sp, -N
    StackFree(i32),   // addi sp, sp, N
    SaveRa(i32),      // sw ra, offset(sp)
    LoadRa(i32),      // lw ra, offset(sp)
    SaveFp(i32),      // sw fp/s0, offset(sp)
    LoadFp(i32),      // lw fp/s0, offset(sp)
    MvFpSp,           // mv fp, sp
    MvSpFp,           // mv sp, fp
    // Pseudo — assembly directives and labels
    Label(String),
    Directive(String),
}

impl AsmEmit for RiscvInst {
    fn emit(&self, writer: &mut dyn Write) -> Result<()> {
        match self {
            RiscvInst::Add { rd, rs1, rs2 } => writeln!(writer, "add {}, {}, {}", rd, rs1, rs2),
            RiscvInst::Sub { rd, rs1, rs2 } => writeln!(writer, "sub {}, {}, {}", rd, rs1, rs2),
            RiscvInst::Mul { rd, rs1, rs2 } => writeln!(writer, "mul {}, {}, {}", rd, rs1, rs2),
            RiscvInst::Div { rd, rs1, rs2 } => writeln!(writer, "div {}, {}, {}", rd, rs1, rs2),
            RiscvInst::Rem { rd, rs1, rs2 } => writeln!(writer, "rem {}, {}, {}", rd, rs1, rs2),
            RiscvInst::And { rd, rs1, rs2 } => writeln!(writer, "and {}, {}, {}", rd, rs1, rs2),
            RiscvInst::Or  { rd, rs1, rs2 } => writeln!(writer, "or {}, {}, {}", rd, rs1, rs2),
            RiscvInst::Slt { rd, rs1, rs2 } => writeln!(writer, "slt {}, {}, {}", rd, rs1, rs2),
            RiscvInst::Seqz { rd, rs }       => writeln!(writer, "seqz {}, {}", rd, rs),
            RiscvInst::Snez { rd, rs }       => writeln!(writer, "snez {}, {}", rd, rs),
            RiscvInst::Addi { rd, rs, imm }  => {
                if *imm < 0 { writeln!(writer, "addi {}, {}, {}", rd, rs, imm) }
                else        { writeln!(writer, "addi {}, {}, {}", rd, rs, imm) }
            },
            RiscvInst::Li { rd, imm } => writeln!(writer, "li {}, {}", rd, imm),
            RiscvInst::Mv { rd, rs }  => writeln!(writer, "mv {}, {}", rd, rs),
            RiscvInst::Lw { rd, base, offset } => writeln!(writer, "lw {}, {}({})", rd, offset, base),
            RiscvInst::Sw { rs2, base, offset } => writeln!(writer, "sw {}, {}({})", rs2, offset, base),
            RiscvInst::La { rd, symbol } => writeln!(writer, "la {}, {}", rd, symbol),
            RiscvInst::Beqz { rs, label } => writeln!(writer, "beqz {}, {}", rs, label),
            RiscvInst::Bnez { rs, label } => writeln!(writer, "bnez {}, {}", rs, label),
            RiscvInst::J { label } => writeln!(writer, "j {}", label),
            RiscvInst::Call { symbol } => writeln!(writer, "call {}", symbol),
            RiscvInst::Ret => writeln!(writer, "ret"),
            RiscvInst::StackAlloc(n) => writeln!(writer, "addi sp, sp, -{}", n),
            RiscvInst::StackFree(n)  => writeln!(writer, "addi sp, sp, {}", n),
            RiscvInst::SaveRa(off) => writeln!(writer, "sw ra, {}(sp)", off),
            RiscvInst::LoadRa(off) => writeln!(writer, "lw ra, {}(sp)", off),
            RiscvInst::SaveFp(off) => writeln!(writer, "sw fp, {}(sp)", off),
            RiscvInst::LoadFp(off) => writeln!(writer, "lw fp, {}(sp)", off),
            RiscvInst::MvFpSp => writeln!(writer, "mv fp, sp"),
            RiscvInst::MvSpFp => writeln!(writer, "mv sp, fp"),
            RiscvInst::Label(s) => writeln!(writer, "{}:", s),
            RiscvInst::Directive(s) => writeln!(writer, "{}", s),
        }
    }
}

// ─── Peephole Optimizations ──────────────────────────────────────────────────

fn optimize_rv(insts: &mut Vec<RiscvInst>) {
    // Rule 1: Redundant mv chain elimination. mv a, b; mv c, a  →  mv a, b; mv c, b
    peephole2(insts, |a, b| {
        if let (RiscvInst::Mv { rd: rd1, rs: rs1 }, RiscvInst::Mv { rd: rd2, rs: rs2 }) = (a, b) {
            if rd1 == rs2 && rd1 != rd2 {
                return Some(vec![a.clone(), RiscvInst::Mv { rd: rd2.clone(), rs: rs1.clone() }]);
            }
        }
        None
    });

    // Rule 2: Remove load after store to same location with same value.
    // sw a0, off(sp); lw a0, off(sp) → sw a0, off(sp)
    peephole2(insts, |a, b| {
        if let (RiscvInst::Sw { rs2: s2, base: b1, offset: o1 }, RiscvInst::Lw { rd, base: b2, offset: o2 }) = (a, b) {
            if b1 == b2 && o1 == o2 && b1 == "sp" && s2 == rd {
                return Some(vec![a.clone()]);
            }
        }
        None
    });

    // Rule 3: Constant folding — li Rd, 0; add Rd, Rs, Rd → mv Rd, Rs
    peephole2(insts, |a, b| {
        if let (RiscvInst::Li { rd: rd1, imm: 0 }, RiscvInst::Add { rd: rd2, rs1, rs2 }) = (a, b) {
            if rd1 == rd2 && rd2 == rs2 {
                return Some(vec![RiscvInst::Mv { rd: rd1.clone(), rs: rs1.clone() }]);
            }
        }
        None
    });
    peephole2(insts, |a, b| {
        if let (RiscvInst::Li { rd: rd1, imm: 0 }, RiscvInst::Add { rd: rd2, rs1, rs2 }) = (a, b) {
            if rd1 == rd2 && rd2 == rs1 {
                return Some(vec![RiscvInst::Mv { rd: rd1.clone(), rs: rs2.clone() }]);
            }
        }
        None
    });

    // Rule 4: li Rd, 0; sub Rd, Rs, Rd → neg-like (sub Rd, x0, Rs → not directly available; keep sub)
    // Rule 5: Remove dead mv-to-self: mv a0, a0
    peephole2(insts, |a, _b| {
        if let RiscvInst::Mv { rd, rs } = a {
            if rd == rs {
                return Some(vec![]); // remove entirely
            }
        }
        None
    });

    // Rule 6: Dead store elimination — consecutive stores to same sp offset, keep last.
    peephole2(insts, |a, b| {
        if let (RiscvInst::Sw { base: b1, offset: o1, .. }, RiscvInst::Sw { base: b2, offset: o2, .. }) = (a, b) {
            if b1 == "sp" && b2 == "sp" && o1 == o2 {
                return Some(vec![b.clone()]);
            }
        }
        None
    });

    // Rule 7: 3-instruction: sw a0, off(sp); mv t1, a0; lw a0, off(sp)
    //   → sw a0, off(sp); mv t1, a0  (redundant reload)
    peephole3(insts, |a, b, c| {
        if let (
            RiscvInst::Sw { rs2, base: b1, offset: o1 },
            RiscvInst::Mv { rs: _, .. },
            RiscvInst::Lw { rd: lrd, base: b2, offset: o2 },
        ) = (a, b, c) {
            if b1 == b2 && o1 == o2 && b1 == "sp" && rs2 == lrd {
                return Some(vec![a.clone(), b.clone()]);
            }
        }
        None
    });

    // Rule 8: Seqz Rd, Rs; Li Rt, 1; Sub → if the Seqz result is negated... etc.
    // (Add more rules as needed)
}

// ─── Register-allocation & codegen state ─────────────────────────────────────

const CALLER_SAVED_REGS: &[&str] = &["t1", "t2", "t3", "t4", "t5", "t6"];
const CALLEE_SAVED_REGS: &[&str] = &[
    "s1", "s2", "s3", "s4", "s5", "s6",
    "s7", "s8", "s9", "s10", "s11",
];

#[derive(Debug, Clone)]
struct LiveInterval {
    value: Value,
    start: usize,
    end: usize,
    reg: Option<String>,
    spilled: bool,
}

struct GenRiscvInfo {
    stackmem: usize,
    stackmap: HashMap<Value, usize>,
    allocmap: HashMap<Value, usize>,
    globalmap: HashMap<Value, usize>,
    namemgr: NameManager,
    reg_map: HashMap<Value, String>,
    spill_map: HashMap<Value, usize>,
    used_callee_regs: Vec<String>,
    used_caller_regs: Vec<String>,
    caller_saved_set: HashSet<String>,
    next_spill_offset: usize,
}

impl GenRiscvInfo {
    fn new() -> Self {
        let caller_saved_set: HashSet<String> = CALLER_SAVED_REGS.iter().map(|s| s.to_string()).collect();
        GenRiscvInfo {
            stackmem: 0, stackmap: HashMap::new(), allocmap: HashMap::new(),
            globalmap: HashMap::new(), namemgr: NameManager::new(),
            reg_map: HashMap::new(), spill_map: HashMap::new(),
            used_callee_regs: Vec::new(), used_caller_regs: Vec::new(),
            caller_saved_set, next_spill_offset: 0,
        }
    }
    fn clear_local_info(&mut self) {
        self.stackmap.clear(); self.allocmap.clear(); self.stackmem = 0;
        self.reg_map.clear(); self.spill_map.clear();
        self.used_callee_regs.clear(); self.used_caller_regs.clear();
        self.next_spill_offset = 0;
    }
}

// ─── Liveness + Linear scan (shared with previous version) ───────────────────

fn compute_live_intervals(func_data: &FunctionData) -> Vec<LiveInterval> {
    let dfg = func_data.dfg();
    let mut intervals: HashMap<Value, LiveInterval> = HashMap::new();
    let mark_use = |intervals: &mut HashMap<Value, LiveInterval>, v: Value, idx: usize| {
        let e = intervals.entry(v).or_insert_with(|| LiveInterval {
            value: v, start: usize::MAX, end: 0, reg: None, spilled: false,
        });
        if e.start == usize::MAX { e.start = idx; }
        e.end = idx;
    };
    let mark_def = |intervals: &mut HashMap<Value, LiveInterval>, v: Value, idx: usize| {
        let e = intervals.entry(v).or_insert_with(|| LiveInterval {
            value: v, start: usize::MAX, end: 0, reg: None, spilled: false,
        });
        if e.start == usize::MAX { e.start = idx; }
        e.end = e.end.max(idx);
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
                ValueKind::Store(v) => {
                    mark_use(&mut intervals, v.value(), inst_idx);
                    mark_use(&mut intervals, v.dest(), inst_idx);
                },
                ValueKind::GetPtr(v) => {
                    mark_use(&mut intervals, v.src(), inst_idx);
                    mark_use(&mut intervals, v.index(), inst_idx);
                },
                ValueKind::GetElemPtr(v) => {
                    mark_use(&mut intervals, v.src(), inst_idx);
                    mark_use(&mut intervals, v.index(), inst_idx);
                },
                ValueKind::Binary(v) => {
                    mark_use(&mut intervals, v.lhs(), inst_idx);
                    mark_use(&mut intervals, v.rhs(), inst_idx);
                },
                ValueKind::Branch(v) => { mark_use(&mut intervals, v.cond(), inst_idx); },
                ValueKind::Jump(_) => {},
                ValueKind::Call(v) => {
                    for arg in v.args() { mark_use(&mut intervals, *arg, inst_idx); }
                },
                ValueKind::Return(v) => {
                    if let Some(ret_val) = v.value() { mark_use(&mut intervals, ret_val, inst_idx); }
                },
                ValueKind::Undef(_) | ValueKind::FuncArgRef(_) | ValueKind::BlockArgRef(_) => {}
            };
            inst_idx += 1;
        }
    }
    intervals.into_values().filter(|i| i.start != usize::MAX).collect()
}

fn linear_scan(func_data: &FunctionData, info: &mut GenRiscvInfo) {
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
                interval.reg = None;
            }
        }
        active.push(interval);
        active.sort_by_key(|a| a.end);
    }
}

// ─── Code generator (IR → Vec<RiscvInst>) ────────────────────────────────────

trait GenerateAsm {
    fn generate(&self, info: &mut GenRiscvInfo, prog: &Program, buf: &mut Vec<RiscvInst>);
}

impl GenerateAsm for koopa::ir::Program {
    fn generate(&self, info: &mut GenRiscvInfo, prog: &Program, buf: &mut Vec<RiscvInst>) {
        buf.push(RiscvInst::Directive(".data".into()));
        for value in self.inst_layout() {
            let value_data = &*self.borrow_value(value.clone());
            let name = info.namemgr.value_name(value_data)[1..].to_string();
            if let ValueKind::GlobalAlloc(v) = value_data.kind() {
                info.globalmap.insert(value.clone(), 0);
                let initval = v.init();
                buf.push(RiscvInst::Directive(format!(".global {}", name)));
                buf.push(RiscvInst::Label(name.clone()));
                match self.borrow_value(initval).kind() {
                    ValueKind::Integer(v) => buf.push(RiscvInst::Directive(format!(".word {}", v.value()))),
                    ValueKind::ZeroInit(_) => {
                        let sz = match value_data.ty().kind() {
                            TypeKind::Pointer(ty) => ty.size(),
                            _ => panic!(),
                        };
                        buf.push(RiscvInst::Directive(format!(".zero {}", sz)));
                    },
                    ValueKind::Aggregate(v) => emit_aggregate(v, self, buf),
                    _ => todo!()
                }
            }
        }
        buf.push(RiscvInst::Directive(".text".into()));
        for (_, funcdata) in self.funcs() {
            if funcdata.layout().entry_bb().is_none() { continue; }
            info.namemgr.enter_func_scope();
            funcdata.generate(info, prog, buf);
            info.namemgr.exit_func_scope();
            info.clear_local_info();
            buf.push(RiscvInst::Directive("".into())); // blank line
        }
    }
}

fn emit_aggregate(agg: &values::Aggregate, prog: &Program, buf: &mut Vec<RiscvInst>) {
    for value in agg.elems() {
        match prog.borrow_value(value.clone()).kind() {
            ValueKind::Integer(v) => buf.push(RiscvInst::Directive(format!(".word {}", v.value()))),
            ValueKind::Aggregate(v) => emit_aggregate(v, prog, buf),
            ValueKind::ZeroInit(_) => {
                buf.push(RiscvInst::Directive(format!(".zero {}", prog.borrow_value(value.clone()).ty().size())));
            },
            _ => todo!()
        }
    }
}

impl GenerateAsm for koopa::ir::FunctionData {
    fn generate(&self, info: &mut GenRiscvInfo, prog: &Program, buf: &mut Vec<RiscvInst>) {
        let fname = self.name()[1..].to_string();
        buf.push(RiscvInst::Directive(format!(".global {}", fname)));
        buf.push(RiscvInst::Label(fname.clone()));

        // Stack allocation
        let mut offset = 0;
        for (v, data) in self.dfg().values().iter() {
            match data.kind() {
                ValueKind::Integer(_) | ValueKind::Aggregate(_) => continue,
                ValueKind::Alloc(_) => {
                    if let TypeKind::Pointer(ty) = data.ty().kind() {
                        info.allocmap.insert(v.clone(), offset); offset += ty.size();
                    }
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
        let total_stack = info.stackmem + info.next_spill_offset - info.stackmem;
        buf.push(RiscvInst::StackAlloc(4));
        buf.push(RiscvInst::SaveRa(0));
        buf.push(RiscvInst::StackAlloc(4));
        buf.push(RiscvInst::SaveFp(0));
        buf.push(RiscvInst::MvFpSp);
        for reg in &info.used_callee_regs {
            buf.push(RiscvInst::StackAlloc(4));
            buf.push(RiscvInst::Sw { rs2: reg.clone(), base: "sp".into(), offset: 0 });
        }
        let sp_adj = total_stack as i32;
        if sp_adj > 0 { buf.push(RiscvInst::StackAlloc(sp_adj)); }

        // Save params
        for (i, param) in self.params().iter().enumerate() {
            let off = info.stackmap[param] as i32;
            let a_reg = format!("a{}", i);
            if i < 8 {
                buf.push(RiscvInst::Sw { rs2: a_reg, base: "sp".into(), offset: off });
            } else {
                let param_off = (total_stack + (i - 8 + 2) * 4) as i32;
                buf.push(RiscvInst::Lw { rd: "a0".into(), base: "sp".into(), offset: param_off });
                buf.push(RiscvInst::Sw { rs2: "a0".into(), base: "sp".into(), offset: off });
            }
        }

        // Basic blocks
        for (bb, node) in self.layout().bbs() {
            let bb_name = info.namemgr.bb_name(self.dfg().bb(*bb));
            let label = format!("label_{}_{}", fname, &bb_name[1..]);
            buf.push(RiscvInst::Label(label));
            for inst_val in node.insts().keys() {
                inst_val.generate(self, info, prog, buf);
            }
        }
    }
}

trait _Gen {
    fn generate(&self, funcdata: &FunctionData, info: &mut GenRiscvInfo, prog: &Program, buf: &mut Vec<RiscvInst>);
}

impl _Gen for Value {
    fn generate(&self, funcdata: &FunctionData, info: &mut GenRiscvInfo, prog: &Program, buf: &mut Vec<RiscvInst>) {
        let inst = funcdata.dfg().value(self.clone());
        match inst.kind() {
            ValueKind::Integer(_) | ValueKind::Aggregate(_) => {},
            ValueKind::Return(v) => {
                if let Some(v) = v.value() {
                    load_operand(v, funcdata, prog, info, buf, "a0");
                }
                // Epilogue
                for reg in info.used_callee_regs.iter().rev() {
                    buf.push(RiscvInst::Lw { rd: reg.clone(), base: "sp".into(), offset: 0 });
                    buf.push(RiscvInst::StackFree(4));
                }
                buf.push(RiscvInst::MvSpFp);
                buf.push(RiscvInst::LoadFp(0));
                buf.push(RiscvInst::StackFree(4));
                buf.push(RiscvInst::LoadRa(0));
                buf.push(RiscvInst::StackFree(4));
                buf.push(RiscvInst::Ret);
            },
            ValueKind::Alloc(_) => {
                buf.push(RiscvInst::Li { rd: "a0".into(), imm: info.allocmap[self] as i32 });
                buf.push(RiscvInst::Add { rd: "a0".into(), rs1: "a0".into(), rs2: "sp".into() });
                store_result(*self, info, buf, "a0");
            },
            ValueKind::Load(v) => {
                if info.globalmap.get(&v.src()).is_some() {
                    let sym = prog.borrow_value(v.src()).name().as_ref().unwrap()[1..].to_string();
                    buf.push(RiscvInst::La { rd: "a0".into(), symbol: sym });
                    buf.push(RiscvInst::Lw { rd: "a0".into(), base: "a0".into(), offset: 0 });
                } else {
                    load_operand(v.src(), funcdata, prog, info, buf, "a0");
                    buf.push(RiscvInst::Lw { rd: "a0".into(), base: "a0".into(), offset: 0 });
                }
                store_result(*self, info, buf, "a0");
            },
            ValueKind::Store(v) => {
                let mut is_agg = false;
                match funcdata.dfg().value(v.value()).kind() {
                    ValueKind::Aggregate(agg) => {
                        is_agg = true;
                        emit_agg_store(funcdata, info, prog, buf, agg, info.allocmap[&v.dest()]);
                    },
                    ValueKind::Integer(vi) => buf.push(RiscvInst::Li { rd: "a0".into(), imm: vi.value() }),
                    _ => load_operand(v.value(), funcdata, prog, info, buf, "a0"),
                }
                if info.globalmap.get(&v.dest()).is_some() {
                    let sym = prog.borrow_value(v.dest()).name().as_ref().unwrap()[1..].to_string();
                    buf.push(RiscvInst::La { rd: "t0".into(), symbol: sym });
                    buf.push(RiscvInst::Sw { rs2: "a0".into(), base: "t0".into(), offset: 0 });
                } else if !is_agg {
                    load_operand(v.dest(), funcdata, prog, info, buf, "a1");
                    buf.push(RiscvInst::Sw { rs2: "a0".into(), base: "a1".into(), offset: 0 });
                }
            },
            ValueKind::Binary(v) => {
                load_operand(v.lhs(), funcdata, prog, info, buf, "a0");
                load_operand(v.rhs(), funcdata, prog, info, buf, "a1");
                match v.op() {
                    BinaryOp::Add => buf.push(RiscvInst::Add { rd: "a0".into(), rs1: "a0".into(), rs2: "a1".into() }),
                    BinaryOp::Sub => buf.push(RiscvInst::Sub { rd: "a0".into(), rs1: "a0".into(), rs2: "a1".into() }),
                    BinaryOp::Mul => buf.push(RiscvInst::Mul { rd: "a0".into(), rs1: "a0".into(), rs2: "a1".into() }),
                    BinaryOp::Div => buf.push(RiscvInst::Div { rd: "a0".into(), rs1: "a0".into(), rs2: "a1".into() }),
                    BinaryOp::Mod => buf.push(RiscvInst::Rem { rd: "a0".into(), rs1: "a0".into(), rs2: "a1".into() }),
                    BinaryOp::And => buf.push(RiscvInst::And { rd: "a0".into(), rs1: "a0".into(), rs2: "a1".into() }),
                    BinaryOp::Or  => buf.push(RiscvInst::Or  { rd: "a0".into(), rs1: "a0".into(), rs2: "a1".into() }),
                    BinaryOp::Eq => {
                        buf.push(RiscvInst::Sub { rd: "a0".into(), rs1: "a0".into(), rs2: "a1".into() });
                        buf.push(RiscvInst::Seqz { rd: "a0".into(), rs: "a0".into() });
                    },
                    BinaryOp::NotEq => {
                        buf.push(RiscvInst::Sub { rd: "a0".into(), rs1: "a0".into(), rs2: "a1".into() });
                        buf.push(RiscvInst::Snez { rd: "a0".into(), rs: "a0".into() });
                    },
                    BinaryOp::Lt => buf.push(RiscvInst::Slt { rd: "a0".into(), rs1: "a0".into(), rs2: "a1".into() }),
                    BinaryOp::Gt => buf.push(RiscvInst::Slt { rd: "a0".into(), rs1: "a1".into(), rs2: "a0".into() }),
                    BinaryOp::Le => {
                        buf.push(RiscvInst::Slt { rd: "a0".into(), rs1: "a1".into(), rs2: "a0".into() });
                        buf.push(RiscvInst::Li { rd: "a1".into(), imm: 1 });
                        buf.push(RiscvInst::Sub { rd: "a0".into(), rs1: "a1".into(), rs2: "a0".into() });
                    },
                    BinaryOp::Ge => {
                        buf.push(RiscvInst::Slt { rd: "a0".into(), rs1: "a0".into(), rs2: "a1".into() });
                        buf.push(RiscvInst::Li { rd: "a1".into(), imm: 1 });
                        buf.push(RiscvInst::Sub { rd: "a0".into(), rs1: "a1".into(), rs2: "a0".into() });
                    },
                    _ => todo!()
                }
                store_result(*self, info, buf, "a0");
            },
            ValueKind::Branch(v) => {
                load_operand(v.cond(), funcdata, prog, info, buf, "a0");
                let tb = info.namemgr.bb_name(funcdata.dfg().bb(v.true_bb()));
                let fb = info.namemgr.bb_name(funcdata.dfg().bb(v.false_bb()));
                let fname = funcdata.name()[1..].to_string();
                buf.push(RiscvInst::Bnez { rs: "a0".into(), label: format!("label_{}_{}", fname, &tb[1..]) });
                buf.push(RiscvInst::J { label: format!("label_{}_{}", fname, &fb[1..]) });
            },
            ValueKind::Jump(v) => {
                let t = info.namemgr.bb_name(funcdata.dfg().bb(v.target()));
                let fname = funcdata.name()[1..].to_string();
                buf.push(RiscvInst::J { label: format!("label_{}_{}", fname, &t[1..]) });
            },
            ValueKind::Call(v) => {
                let argcnt = v.args().iter().count();
                // Flush caller-saved
                for (value, reg) in &info.reg_map {
                    if info.caller_saved_set.contains(reg) {
                        if let Some(off) = info.stackmap.get(value) {
                            buf.push(RiscvInst::Sw { rs2: reg.clone(), base: "sp".into(), offset: *off as i32 });
                        }
                    }
                }
                let mut stack_size = 0i32;
                for (i, arg) in v.args().iter().rev().enumerate() {
                    if i + 8 >= argcnt && i < argcnt {
                        let idx = argcnt - 1 - i;
                        load_operand(*arg, funcdata, prog, info, buf, &format!("a{}", idx));
                    } else {
                        load_operand(*arg, funcdata, prog, info, buf, "a0");
                        buf.push(RiscvInst::Sw { rs2: "a0".into(), base: "sp".into(), offset: -4 - 4 * i as i32 });
                    }
                }
                if argcnt > 8 { stack_size += ((argcnt - 8) * 4) as i32; }
                if stack_size > 0 { buf.push(RiscvInst::StackAlloc(stack_size)); }
                buf.push(RiscvInst::Call { symbol: prog.func(v.callee()).name()[1..].to_string() });
                if stack_size > 0 { buf.push(RiscvInst::StackFree(stack_size)); }
                if !funcdata.dfg().value(self.clone()).ty().is_unit() {
                    store_result(*self, info, buf, "a0");
                }
            },
            ValueKind::GetElemPtr(v) => {
                load_operand(v.index(), funcdata, prog, info, buf, "a0");
                let step = if info.globalmap.get(&v.src()).is_some() {
                    match prog.borrow_value(v.src()).ty().kind() {
                        TypeKind::Pointer(ty) => match ty.kind() { TypeKind::Array(ty, _) => ty.size(), _ => panic!() },
                        _ => panic!(),
                    }
                } else {
                    match funcdata.dfg().value(v.src()).ty().kind() {
                        TypeKind::Pointer(ty) => match ty.kind() { TypeKind::Array(ty, _) => ty.size(), _ => panic!() },
                        _ => panic!(),
                    }
                };
                buf.push(RiscvInst::Li { rd: "a1".into(), imm: step as i32 });
                buf.push(RiscvInst::Mul { rd: "a1".into(), rs1: "a0".into(), rs2: "a1".into() });
                load_operand(v.src(), funcdata, prog, info, buf, "a0");
                buf.push(RiscvInst::Add { rd: "a0".into(), rs1: "a0".into(), rs2: "a1".into() });
                store_result(*self, info, buf, "a0");
            },
            ValueKind::GetPtr(v) => {
                load_operand(v.index(), funcdata, prog, info, buf, "a0");
                match funcdata.dfg().value(v.src()).ty().kind() {
                    TypeKind::Pointer(ty) => {
                        buf.push(RiscvInst::Li { rd: "a1".into(), imm: ty.size() as i32 });
                        buf.push(RiscvInst::Mul { rd: "a1".into(), rs1: "a1".into(), rs2: "a0".into() });
                    },
                    _ => panic!(),
                }
                load_operand(v.src(), funcdata, prog, info, buf, "a1");
                buf.push(RiscvInst::Add { rd: "a0".into(), rs1: "a0".into(), rs2: "a1".into() });
                store_result(*self, info, buf, "a0");
            },
            _ => todo!(),
        }
    }
}

fn emit_agg_store(funcdata: &FunctionData, info: &mut GenRiscvInfo, prog: &Program, buf: &mut Vec<RiscvInst>, agg: &values::Aggregate, mut base: usize) {
    for value in agg.elems() {
        match funcdata.dfg().value(value.clone()).kind() {
            ValueKind::Aggregate(v) => emit_agg_store(funcdata, info, prog, buf, v, base),
            ValueKind::Integer(v) => {
                buf.push(RiscvInst::Li { rd: "a0".into(), imm: v.value() });
                buf.push(RiscvInst::Sw { rs2: "a0".into(), base: "sp".into(), offset: base as i32 });
                base += 4;
            },
            _ => {
                load_operand(*value, funcdata, prog, info, buf, "a0");
                buf.push(RiscvInst::Sw { rs2: "a0".into(), base: "sp".into(), offset: base as i32 });
                base += 4;
            },
        }
    }
}

// ─── Operand helpers ─────────────────────────────────────────────────────────

fn load_operand(value: Value, funcdata: &FunctionData, prog: &Program, info: &GenRiscvInfo, buf: &mut Vec<RiscvInst>, dest: &str) {
    if let ValueKind::Integer(v) = funcdata.dfg().value(value).kind() {
        buf.push(RiscvInst::Li { rd: dest.into(), imm: v.value() });
        return;
    }
    if let Some(reg) = info.reg_map.get(&value) {
        buf.push(RiscvInst::Mv { rd: dest.into(), rs: reg.clone() });
    } else if let Some(off) = info.spill_map.get(&value) {
        buf.push(RiscvInst::Lw { rd: dest.into(), base: "sp".into(), offset: (*off + info.stackmem) as i32 });
    } else if info.globalmap.get(&value).is_some() {
        let sym = prog.borrow_value(value).name().as_ref().unwrap()[1..].to_string();
        buf.push(RiscvInst::La { rd: dest.into(), symbol: sym });
    } else if let Some(off) = info.stackmap.get(&value) {
        buf.push(RiscvInst::Lw { rd: dest.into(), base: "sp".into(), offset: *off as i32 });
    } else {
        panic!("Cannot load value");
    }
}

fn store_result(value: Value, info: &mut GenRiscvInfo, buf: &mut Vec<RiscvInst>, src: &str) {
    if let Some(off) = info.stackmap.get(&value) {
        buf.push(RiscvInst::Sw { rs2: src.into(), base: "sp".into(), offset: *off as i32 });
    }
    if let Some(reg) = info.reg_map.get(&value) {
        buf.push(RiscvInst::Mv { rd: reg.clone(), rs: src.into() });
    }
}

// ─── Public API ──────────────────────────────────────────────────────────────

pub fn generator_rv(prog: &Program, writer: &mut impl Write) -> Result<()> {
    let mut info = GenRiscvInfo::new();
    let mut buf: Vec<RiscvInst> = Vec::new();
    prog.generate(&mut info, prog, &mut buf);
    optimize_rv(&mut buf);
    for inst in &buf {
        inst.emit(writer)?;
    }
    Ok(())
}
