use std::io::{Write, Result};
use std::collections::{HashMap, HashSet};

use koopa::ir::*;
use koopa::back::NameManager;

use super::asm::{AsmEmit, peephole2, peephole3};

// ─── LoongArch Structured Instruction ────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum LAInst {
    // Arithmetic (word = 32-bit)
    AddW  { rd: String, rj: String, rk: String },
    SubW  { rd: String, rj: String, rk: String },
    MulW  { rd: String, rj: String, rk: String },
    DivW  { rd: String, rj: String, rk: String },
    ModW  { rd: String, rj: String, rk: String },
    And   { rd: String, rj: String, rk: String },
    Or    { rd: String, rj: String, rk: String },
    Xor   { rd: String, rj: String, rk: String },
    Slt   { rd: String, rj: String, rk: String },
    Sltu  { rd: String, rj: String, rk: String },
    Sltui { rd: String, rj: String, imm: u16 },
    Xori  { rd: String, rj: String, imm: u16 },
    // Immediate
    Ori    { rd: String, rj: String, imm: u16 },
    Lu12iW { rd: String, imm: i32 },
    AddiW  { rd: String, rj: String, imm: i32 },
    AddiD  { rd: String, rj: String, imm: i32 },
    // Memory
    LdW { rd: String, rj: String, offset: i32 },
    StW { rd: String, rj: String, offset: i32 },
    LdD { rd: String, rj: String, offset: i32 },
    StD { rd: String, rj: String, offset: i32 },
    // PC-relative
    Pcalau12i { rd: String, symbol: String },
    // Branch
    Beq  { rj: String, rk: String, label: String },
    Bne  { rj: String, rk: String, label: String },
    Beqz { rj: String, label: String },
    Bnez { rj: String, label: String },
    Blt  { rj: String, rk: String, label: String },
    Bge  { rj: String, rk: String, label: String },
    B    { label: String },
    Bl   { symbol: String },
    Jirl { rd: String, rj: String, offset: i32 },
    // Labels / directives
    Label(String),
    Directive(String),
}

impl AsmEmit for LAInst {
    fn emit(&self, writer: &mut dyn Write) -> Result<()> {
        match self {
            LAInst::AddW  { rd, rj, rk } => writeln!(writer, "\tadd.w {}, {}, {}", rd, rj, rk),
            LAInst::SubW  { rd, rj, rk } => writeln!(writer, "\tsub.w {}, {}, {}", rd, rj, rk),
            LAInst::MulW  { rd, rj, rk } => writeln!(writer, "\tmul.w {}, {}, {}", rd, rj, rk),
            LAInst::DivW  { rd, rj, rk } => writeln!(writer, "\tdiv.w {}, {}, {}", rd, rj, rk),
            LAInst::ModW  { rd, rj, rk } => writeln!(writer, "\tmod.w {}, {}, {}", rd, rj, rk),
            LAInst::And   { rd, rj, rk } => writeln!(writer, "\tand {}, {}, {}", rd, rj, rk),
            LAInst::Or    { rd, rj, rk } => writeln!(writer, "\tor {}, {}, {}", rd, rj, rk),
            LAInst::Xor   { rd, rj, rk } => writeln!(writer, "\txor {}, {}, {}", rd, rj, rk),
            LAInst::Slt   { rd, rj, rk } => writeln!(writer, "\tslt {}, {}, {}", rd, rj, rk),
            LAInst::Sltu  { rd, rj, rk } => writeln!(writer, "\tsltu {}, {}, {}", rd, rj, rk),
            LAInst::Sltui { rd, rj, imm } => writeln!(writer, "\tsltui {}, {}, {}", rd, rj, imm),
            LAInst::Xori  { rd, rj, imm } => writeln!(writer, "\txori {}, {}, {}", rd, rj, imm),
            LAInst::Ori    { rd, rj, imm } => writeln!(writer, "\tori {}, {}, {}", rd, rj, imm),
            LAInst::Lu12iW { rd, imm } => writeln!(writer, "\tlu12i.w {}, {}", rd, imm),
            LAInst::AddiW  { rd, rj, imm } => writeln!(writer, "\taddi.w {}, {}, {}", rd, rj, imm),
            LAInst::AddiD  { rd, rj, imm } => writeln!(writer, "\taddi.d {}, {}, {}", rd, rj, imm),
            LAInst::LdW { rd, rj, offset } => writeln!(writer, "\tld.w {}, {}, {}", rd, rj, offset),
            LAInst::StW { rd, rj, offset } => writeln!(writer, "\tst.w {}, {}, {}", rd, rj, offset),
            LAInst::LdD { rd, rj, offset } => writeln!(writer, "\tld.d {}, {}, {}", rd, rj, offset),
            LAInst::StD { rd, rj, offset } => writeln!(writer, "\tst.d {}, {}, {}", rd, rj, offset),
            LAInst::Pcalau12i { rd, symbol } => writeln!(writer, "\tpcalau12i {}, %pc_hi20({})", rd, symbol),
            LAInst::Beq  { rj, rk, label } => writeln!(writer, "\tbeq {}, {}, {}", rj, rk, label),
            LAInst::Bne  { rj, rk, label } => writeln!(writer, "\tbne {}, {}, {}", rj, rk, label),
            LAInst::Beqz { rj, label } => writeln!(writer, "\tbeqz {}, {}", rj, label),
            LAInst::Bnez { rj, label } => writeln!(writer, "\tbnez {}, {}", rj, label),
            LAInst::Blt  { rj, rk, label } => writeln!(writer, "\tblt {}, {}, {}", rj, rk, label),
            LAInst::Bge  { rj, rk, label } => writeln!(writer, "\tbge {}, {}, {}", rj, rk, label),
            LAInst::B    { label } => writeln!(writer, "\tb {}", label),
            LAInst::Bl   { symbol } => writeln!(writer, "\tbl {}", symbol),
            LAInst::Jirl { rd, rj, offset } => writeln!(writer, "\tjirl {}, {}, {}", rd, rj, offset),
            LAInst::Label(s) => writeln!(writer, "{}:", s),
            LAInst::Directive(s) => writeln!(writer, "{}", s),
        }
    }
}

// ─── Peephole Optimizations ──────────────────────────────────────────────────

fn optimize_la(insts: &mut Vec<LAInst>) {
    // Rule 1: Dead store elimination — consecutive st.w to same sp offset
    peephole2(insts, |a, b| {
        if let (LAInst::StW { rj: b1, offset: o1, .. }, LAInst::StW { rj: b2, offset: o2, .. }) = (a, b) {
            if b1 == "$sp" && b2 == "$sp" && o1 == o2 { return Some(vec![b.clone()]); }
        }
        None
    });
    // Rule 2: Redundant load after store: st.w $a0, $sp, N; ld.w $a0, $sp, N → st.w
    peephole2(insts, |a, b| {
        if let (LAInst::StW { rd, rj: b1, offset: o1 }, LAInst::LdW { rd: lrd, rj: b2, offset: o2 }) = (a, b) {
            if b1 == b2 && o1 == o2 && b1 == "$sp" && rd == lrd { return Some(vec![a.clone()]); }
        }
        None
    });
    // Rule 3: Constant folding — ori rd, zero, 0; add.w rd, rs, rd → or rd, rs, zero
    peephole2(insts, |a, b| {
        if let (LAInst::Ori { rd: d1, rj, imm: 0 }, LAInst::AddW { rd: d2, rj: rj2, rk }) = (a, b) {
            if rj == "$zero" && d1 == d2 && d2 == rk { return Some(vec![LAInst::Or { rd: d1.clone(), rj: rj2.clone(), rk: "$zero".into() }]); }
        }
        None
    });
    // Rule 4: Remove mov-to-self
    peephole2(insts, |a, _b| {
        if let LAInst::Or { rd, rj, rk } = a {
            if (rj == rd && rk == "$zero") || (rk == rd && rj == "$zero") {
                return Some(vec![]);
            }
        }
        None
    });
    // Rule 5: ld.w after st.w with intermediate mov → can be optimized
    peephole3(insts, |a, b, c| {
        if let (LAInst::StW { rd: srd, rj: b1, offset: o1 }, LAInst::Or { rd: mrd, rj: mj, rk: mk }, LAInst::LdW { rd: lrd, rj: b2, offset: o2 }) = (a, b, c) {
            if b1 == b2 && o1 == o2 && b1 == "$sp" && srd == lrd && mrd == srd {
                if (mj == srd && mk == "$zero") || (mk == srd && mj == "$zero") {
                    return Some(vec![a.clone(), b.clone()]);
                }
            }
        }
        None
    });
}

// ─── Register allocation state ───────────────────────────────────────────────

const CALLER_SAVED_REGS: &[&str] = &["t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7"];
const CALLEE_SAVED_REGS: &[&str] = &["s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8"];
const ARG_REGS: &[&str] = &["a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"];

#[derive(Debug, Clone)]
struct LiveInterval { value: Value, start: usize, end: usize, reg: Option<String>, spilled: bool }

struct GenLAInfo {
    stackmem: usize, stackmap: HashMap<Value, usize>, allocmap: HashMap<Value, usize>,
    globalmap: HashMap<Value, usize>, namemgr: NameManager,
    reg_map: HashMap<Value, String>, spill_map: HashMap<Value, usize>,
    used_callee_regs: Vec<String>, used_caller_regs: Vec<String>,
    caller_saved_set: HashSet<String>, next_spill_offset: usize,
}

impl GenLAInfo {
    fn new() -> Self {
        let cs: HashSet<_> = CALLER_SAVED_REGS.iter().map(|s| s.to_string()).collect();
        GenLAInfo { stackmem: 0, stackmap: HashMap::new(), allocmap: HashMap::new(),
            globalmap: HashMap::new(), namemgr: NameManager::new(),
            reg_map: HashMap::new(), spill_map: HashMap::new(),
            used_callee_regs: Vec::new(), used_caller_regs: Vec::new(),
            caller_saved_set: cs, next_spill_offset: 0 }
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
    let mut m: HashMap<Value, LiveInterval> = HashMap::new();
    let mark_use = |m: &mut HashMap<Value, LiveInterval>, v: Value, idx: usize| {
        let e = m.entry(v).or_insert(LiveInterval { value: v, start: usize::MAX, end: 0, reg: None, spilled: false });
        if e.start == usize::MAX { e.start = idx; } e.end = idx;
    };
    let mark_def = |m: &mut HashMap<Value, LiveInterval>, v: Value, idx: usize| {
        let e = m.entry(v).or_insert(LiveInterval { value: v, start: usize::MAX, end: 0, reg: None, spilled: false });
        if e.start == usize::MAX { e.start = idx; } e.end = e.end.max(idx);
    };
    let mut inst_idx = 0usize;
    for param in func_data.params() { mark_def(&mut m, *param, 0); }
    for (_bb, node) in func_data.layout().bbs() {
        for inst_val in node.insts().keys() {
            let id = dfg.value(*inst_val);
            mark_def(&mut m, *inst_val, inst_idx);
            match id.kind() {
                ValueKind::Integer(_) | ValueKind::Aggregate(_) | ValueKind::ZeroInit(_) => {},
                ValueKind::Alloc(_) | ValueKind::GlobalAlloc(_) => {},
                ValueKind::Load(v) => { mark_use(&mut m, v.src(), inst_idx); },
                ValueKind::Store(v) => { mark_use(&mut m, v.value(), inst_idx); mark_use(&mut m, v.dest(), inst_idx); },
                ValueKind::GetPtr(v) => { mark_use(&mut m, v.src(), inst_idx); mark_use(&mut m, v.index(), inst_idx); },
                ValueKind::GetElemPtr(v) => { mark_use(&mut m, v.src(), inst_idx); mark_use(&mut m, v.index(), inst_idx); },
                ValueKind::Binary(v) => { mark_use(&mut m, v.lhs(), inst_idx); mark_use(&mut m, v.rhs(), inst_idx); },
                ValueKind::Branch(v) => { mark_use(&mut m, v.cond(), inst_idx); },
                ValueKind::Jump(_) => {},
                ValueKind::Call(v) => { for arg in v.args() { mark_use(&mut m, *arg, inst_idx); } },
                ValueKind::Return(v) => { if let Some(r) = v.value() { mark_use(&mut m, r, inst_idx); } },
                ValueKind::Undef(_) | ValueKind::FuncArgRef(_) | ValueKind::BlockArgRef(_) => {}
            };
            inst_idx += 1;
        }
    }
    m.into_values().filter(|i| i.start != usize::MAX).collect()
}

fn linear_scan(func_data: &FunctionData, info: &mut GenLAInfo) {
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
            if active[i].end < interval.start { if let Some(reg) = active.remove(i).reg { free_regs.push(reg); } } else { i += 1; }
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
                info.spill_map.insert(spilled.value, slot); spilled.spilled = true;
                interval.reg = Some(reg); info.reg_map.insert(interval.value, interval.reg.clone().unwrap());
                active.push(spilled);
            } else {
                let slot = info.next_spill_offset; info.next_spill_offset += 4;
                info.spill_map.insert(interval.value, slot); interval.spilled = true;
            }
        }
        active.push(interval);
        active.sort_by_key(|a| a.end);
    }
}

// ─── Codegen ─────────────────────────────────────────────────────────────────

fn load_imm32(buf: &mut Vec<LAInst>, rd: &str, val: i32) {
    let r = format!("${}", rd);
    let v = val as u32;
    let hi = (v >> 12) as i32;
    let lo = (v & 0xfff) as i32;
    if hi == 0 {
        buf.push(LAInst::Ori { rd: r, rj: "$zero".into(), imm: lo as u16 });
    } else if lo == 0 {
        buf.push(LAInst::Lu12iW { rd: r, imm: hi });
    } else {
        let r2 = format!("${}", rd);
        buf.push(LAInst::Lu12iW { rd: r, imm: hi });
        buf.push(LAInst::Ori { rd: r2.clone(), rj: r2, imm: lo as u16 });
    }
}

fn load_operand(value: Value, funcdata: &FunctionData, prog: &Program, info: &GenLAInfo, buf: &mut Vec<LAInst>, dest: &str) {
    let d = |s: &str| format!("${}", s);
    if let ValueKind::Integer(v) = funcdata.dfg().value(value).kind() {
        load_imm32(buf, dest, v.value()); return;
    }
    if let Some(reg) = info.reg_map.get(&value) {
        buf.push(LAInst::Or { rd: d(dest), rj: d(reg), rk: "$zero".into() });
    } else if let Some(off) = info.spill_map.get(&value) {
        buf.push(LAInst::LdW { rd: d(dest), rj: "$sp".into(), offset: (*off + info.stackmem) as i32 });
    } else if info.globalmap.get(&value).is_some() {
        let sym = prog.borrow_value(value).name().as_ref().unwrap()[1..].to_string();
        buf.push(LAInst::Pcalau12i { rd: d(dest), symbol: sym.clone() });
        buf.push(LAInst::Directive(format!("\taddi.d ${}, ${}, %pc_lo12({})", dest, dest, sym)));
    } else if let Some(off) = info.stackmap.get(&value) {
        buf.push(LAInst::LdW { rd: d(dest), rj: "$sp".into(), offset: *off as i32 });
    } else {
        panic!("Cannot load value");
    }
}

fn store_result(value: Value, info: &mut GenLAInfo, buf: &mut Vec<LAInst>, src: &str) {
    let s = |name: &str| format!("${}", name);
    if let Some(off) = info.stackmap.get(&value) {
        buf.push(LAInst::StW { rd: s(src), rj: "$sp".into(), offset: *off as i32 });
    }
    if let Some(reg) = info.reg_map.get(&value) {
        buf.push(LAInst::Or { rd: s(reg), rj: s(src), rk: "$zero".into() });
    }
}

trait GenerateAsm {
    fn generate(&self, info: &mut GenLAInfo, prog: &Program, buf: &mut Vec<LAInst>);
}

impl GenerateAsm for koopa::ir::Program {
    fn generate(&self, info: &mut GenLAInfo, prog: &Program, buf: &mut Vec<LAInst>) {
        buf.push(LAInst::Directive("\t.data".into()));
        for value in self.inst_layout() {
            let vd = &*self.borrow_value(value.clone());
            let name = info.namemgr.value_name(vd)[1..].to_string();
            if let ValueKind::GlobalAlloc(v) = vd.kind() {
                info.globalmap.insert(value.clone(), 0);
                buf.push(LAInst::Directive(format!("\t.globl {}", name)));
                buf.push(LAInst::Directive("\t.align 2".into()));
                buf.push(LAInst::Label(name.clone()));
                match self.borrow_value(v.init()).kind() {
                    ValueKind::Integer(v) => buf.push(LAInst::Directive(format!("\t.4byte {}", v.value()))),
                    ValueKind::ZeroInit(_) => {
                        let sz = match vd.ty().kind() { TypeKind::Pointer(ty) => ty.size(), _ => panic!() };
                        buf.push(LAInst::Directive(format!("\t.zero {}", sz)));
                    },
                    ValueKind::Aggregate(v) => emit_agg(v, self, buf),
                    _ => todo!()
                }
            }
        }
        buf.push(LAInst::Directive("\t.text".into()));
        for (_, fd) in self.funcs() {
            if fd.layout().entry_bb().is_none() { continue; }
            info.namemgr.enter_func_scope();
            fd.generate(info, prog, buf);
            info.namemgr.exit_func_scope();
            info.clear_local_info();
            buf.push(LAInst::Directive("".into()));
        }
    }
}

fn emit_agg(agg: &values::Aggregate, prog: &Program, buf: &mut Vec<LAInst>) {
    for v in agg.elems() {
        match prog.borrow_value(v.clone()).kind() {
            ValueKind::Integer(v) => buf.push(LAInst::Directive(format!("\t.4byte {}", v.value()))),
            ValueKind::Aggregate(v) => emit_agg(v, prog, buf),
            ValueKind::ZeroInit(_) => buf.push(LAInst::Directive(format!("\t.zero {}", prog.borrow_value(v.clone()).ty().size()))),
            _ => todo!()
        }
    }
}

impl GenerateAsm for koopa::ir::FunctionData {
    fn generate(&self, info: &mut GenLAInfo, prog: &Program, buf: &mut Vec<LAInst>) {
        let fname = self.name()[1..].to_string();
        buf.push(LAInst::Directive(format!("\t.globl {}", fname)));
        buf.push(LAInst::Label(fname.clone()));

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

        let total = info.stackmem + info.next_spill_offset - info.stackmem;
        let callee_saves = info.used_callee_regs.len() * 8;
        let aligned = ((total + callee_saves + 16) + 15) & !15;
        let frame = aligned as i32;
        let ra_off = frame - 8;
        let fp_off = frame - 16;

        // Prologue
        buf.push(LAInst::AddiD { rd: "$sp".into(), rj: "$sp".into(), imm: -frame });
        buf.push(LAInst::StD { rd: "$ra".into(), rj: "$sp".into(), offset: ra_off });
        buf.push(LAInst::StD { rd: "$fp".into(), rj: "$sp".into(), offset: fp_off });
        let mut csr_off = fp_off - 8;
        for reg in &info.used_callee_regs {
            buf.push(LAInst::StD { rd: format!("${}", reg), rj: "$sp".into(), offset: csr_off });
            csr_off -= 8;
        }
        buf.push(LAInst::Or { rd: "$fp".into(), rj: "$sp".into(), rk: "$zero".into() });

        // Save params
        for (i, param) in self.params().iter().enumerate() {
            if i < 8 {
                buf.push(LAInst::StW { rd: format!("${}", ARG_REGS[i]), rj: "$sp".into(), offset: info.stackmap[param] as i32 });
            } else {
                let poff = (frame + (i as i32 - 8) * 8) as i32;
                buf.push(LAInst::LdD { rd: "$a0".into(), rj: "$sp".into(), offset: poff });
                buf.push(LAInst::StW { rd: "$a0".into(), rj: "$sp".into(), offset: info.stackmap[param] as i32 });
            }
        }

        for (bb, node) in self.layout().bbs() {
            let bb_name = info.namemgr.bb_name(self.dfg().bb(*bb));
            buf.push(LAInst::Label(format!("\t.L{}_{}", fname, &bb_name[1..])));
            for inst_val in node.insts().keys() {
                inst_val.generate(self, info, prog, buf);
            }
        }
    }
}

trait _Gen {
    fn generate(&self, funcdata: &FunctionData, info: &mut GenLAInfo, prog: &Program, buf: &mut Vec<LAInst>);
}

impl _Gen for Value {
    fn generate(&self, funcdata: &FunctionData, info: &mut GenLAInfo, prog: &Program, buf: &mut Vec<LAInst>) {
        let inst = funcdata.dfg().value(self.clone());
        match inst.kind() {
            ValueKind::Integer(_) | ValueKind::Aggregate(_) => {},
            ValueKind::Return(v) => {
                if let Some(v) = v.value() { load_operand(v, funcdata, prog, info, buf, "a0"); }
                let total = info.stackmem + info.next_spill_offset - info.stackmem;
                let callee_saves = info.used_callee_regs.len() * 8;
                let aligned = ((total + callee_saves + 16) + 15) & !15;
                let frame = aligned as i32;
                let ra_off = frame - 8;
                let fp_off = frame - 16;
                let mut csr_off = fp_off - 8 - ((info.used_callee_regs.len() as i32 - 1) * 8);
                for reg in info.used_callee_regs.iter().rev() {
                    buf.push(LAInst::LdD { rd: format!("${}", reg), rj: "$sp".into(), offset: csr_off });
                    csr_off += 8;
                }
                buf.push(LAInst::LdD { rd: "$fp".into(), rj: "$sp".into(), offset: fp_off });
                buf.push(LAInst::LdD { rd: "$ra".into(), rj: "$sp".into(), offset: ra_off });
                buf.push(LAInst::AddiD { rd: "$sp".into(), rj: "$sp".into(), imm: frame });
                buf.push(LAInst::Jirl { rd: "$r0".into(), rj: "$r1".into(), offset: 0 });
            },
            ValueKind::Alloc(_) => {
                load_imm32(buf, "a0", info.allocmap[self] as i32);
                buf.push(LAInst::AddiD { rd: "$a0".into(), rj: "$a0".into(), imm: 0 }); // actually add.d $a0, $a0, $sp
                // Replace with: add.d $a0, $a0, $sp
                buf.pop();
                buf.push(LAInst::Directive("\tadd.d $a0, $a0, $sp".into()));
                store_result(*self, info, buf, "a0");
            },
            ValueKind::Load(v) => {
                if info.globalmap.get(&v.src()).is_some() {
                    let sym = prog.borrow_value(v.src()).name().as_ref().unwrap()[1..].to_string();
                    buf.push(LAInst::Pcalau12i { rd: "$a0".into(), symbol: sym.clone() });
                    buf.push(LAInst::Directive(format!("\taddi.d $a0, $a0, %pc_lo12({})", sym)));
                    buf.push(LAInst::LdW { rd: "$a0".into(), rj: "$a0".into(), offset: 0 });
                } else {
                    load_operand(v.src(), funcdata, prog, info, buf, "a0");
                    buf.push(LAInst::LdW { rd: "$a0".into(), rj: "$a0".into(), offset: 0 });
                }
                store_result(*self, info, buf, "a0");
            },
            ValueKind::Store(v) => {
                let mut is_agg = false;
                match funcdata.dfg().value(v.value()).kind() {
                    ValueKind::Aggregate(agg) => { is_agg = true; emit_agg_store(funcdata, info, prog, buf, agg, info.allocmap[&v.dest()]); },
                    ValueKind::Integer(vi) => load_imm32(buf, "a0", vi.value()),
                    _ => load_operand(v.value(), funcdata, prog, info, buf, "a0"),
                }
                if info.globalmap.get(&v.dest()).is_some() {
                    let sym = prog.borrow_value(v.dest()).name().as_ref().unwrap()[1..].to_string();
                    buf.push(LAInst::Pcalau12i { rd: "$t8".into(), symbol: sym.clone() });
                    buf.push(LAInst::Directive(format!("\taddi.d $t8, $t8, %pc_lo12({})", sym)));
                    buf.push(LAInst::StW { rd: "$a0".into(), rj: "$t8".into(), offset: 0 });
                } else if !is_agg {
                    load_operand(v.dest(), funcdata, prog, info, buf, "t8");
                    buf.push(LAInst::StW { rd: "$a0".into(), rj: "$t8".into(), offset: 0 });
                }
            },
            ValueKind::Binary(v) => {
                load_operand(v.lhs(), funcdata, prog, info, buf, "a0");
                load_operand(v.rhs(), funcdata, prog, info, buf, "a1");
                match v.op() {
                    BinaryOp::Add => buf.push(LAInst::AddW { rd: "$a0".into(), rj: "$a0".into(), rk: "$a1".into() }),
                    BinaryOp::Sub => buf.push(LAInst::SubW { rd: "$a0".into(), rj: "$a0".into(), rk: "$a1".into() }),
                    BinaryOp::Mul => buf.push(LAInst::MulW { rd: "$a0".into(), rj: "$a0".into(), rk: "$a1".into() }),
                    BinaryOp::Div => buf.push(LAInst::DivW { rd: "$a0".into(), rj: "$a0".into(), rk: "$a1".into() }),
                    BinaryOp::Mod => buf.push(LAInst::ModW { rd: "$a0".into(), rj: "$a0".into(), rk: "$a1".into() }),
                    BinaryOp::And => buf.push(LAInst::And  { rd: "$a0".into(), rj: "$a0".into(), rk: "$a1".into() }),
                    BinaryOp::Or  => buf.push(LAInst::Or   { rd: "$a0".into(), rj: "$a0".into(), rk: "$a1".into() }),
                    BinaryOp::Eq => { buf.push(LAInst::Xor { rd: "$a0".into(), rj: "$a0".into(), rk: "$a1".into() }); buf.push(LAInst::Sltui { rd: "$a0".into(), rj: "$a0".into(), imm: 1 }); },
                    BinaryOp::NotEq => { buf.push(LAInst::Xor { rd: "$a0".into(), rj: "$a0".into(), rk: "$a1".into() }); buf.push(LAInst::Sltu { rd: "$a0".into(), rj: "$zero".into(), rk: "$a0".into() }); },
                    BinaryOp::Lt => buf.push(LAInst::Slt { rd: "$a0".into(), rj: "$a0".into(), rk: "$a1".into() }),
                    BinaryOp::Gt => buf.push(LAInst::Slt { rd: "$a0".into(), rj: "$a1".into(), rk: "$a0".into() }),
                    BinaryOp::Le => { buf.push(LAInst::Slt { rd: "$a0".into(), rj: "$a1".into(), rk: "$a0".into() }); buf.push(LAInst::Xori { rd: "$a0".into(), rj: "$a0".into(), imm: 1 }); },
                    BinaryOp::Ge => { buf.push(LAInst::Slt { rd: "$a0".into(), rj: "$a0".into(), rk: "$a1".into() }); buf.push(LAInst::Xori { rd: "$a0".into(), rj: "$a0".into(), imm: 1 }); },
                    _ => todo!()
                }
                store_result(*self, info, buf, "a0");
            },
            ValueKind::Branch(v) => {
                load_operand(v.cond(), funcdata, prog, info, buf, "a0");
                let tb = info.namemgr.bb_name(funcdata.dfg().bb(v.true_bb()));
                let fb = info.namemgr.bb_name(funcdata.dfg().bb(v.false_bb()));
                let fname = funcdata.name()[1..].to_string();
                buf.push(LAInst::Bnez { rj: "$a0".into(), label: format!(".L{}_{}", fname, &tb[1..]) });
                buf.push(LAInst::B { label: format!(".L{}_{}", fname, &fb[1..]) });
            },
            ValueKind::Jump(v) => {
                let t = info.namemgr.bb_name(funcdata.dfg().bb(v.target()));
                buf.push(LAInst::B { label: format!(".L{}_{}", funcdata.name()[1..].to_string(), &t[1..]) });
            },
            ValueKind::Call(v) => {
                for (value, reg) in &info.reg_map {
                    if info.caller_saved_set.contains(reg) {
                        if let Some(off) = info.stackmap.get(value) {
                            buf.push(LAInst::StW { rd: format!("${}", reg), rj: "$sp".into(), offset: *off as i32 });
                        }
                    }
                }
                let mut stack_args = 0i32;
                for (i, arg) in v.args().iter().enumerate() {
                    if i < 8 {
                        load_operand(*arg, funcdata, prog, info, buf, ARG_REGS[i]);
                    } else {
                        load_operand(*arg, funcdata, prog, info, buf, "a0");
                        buf.push(LAInst::AddiD { rd: "$sp".into(), rj: "$sp".into(), imm: -8 });
                        buf.push(LAInst::StD { rd: "$a0".into(), rj: "$sp".into(), offset: 0 });
                        stack_args += 1;
                    }
                }
                let push_bytes = stack_args * 8;
                if push_bytes % 16 != 0 { buf.push(LAInst::AddiD { rd: "$sp".into(), rj: "$sp".into(), imm: -8 }); }
                buf.push(LAInst::Bl { symbol: prog.func(v.callee()).name()[1..].to_string() });
                let clean = push_bytes + if push_bytes % 16 != 0 { 8 } else { 0 };
                if clean > 0 { buf.push(LAInst::AddiD { rd: "$sp".into(), rj: "$sp".into(), imm: clean }); }
                if !funcdata.dfg().value(self.clone()).ty().is_unit() { store_result(*self, info, buf, "a0"); }
            },
            ValueKind::GetElemPtr(v) => {
                load_operand(v.index(), funcdata, prog, info, buf, "a0");
                let step = if info.globalmap.get(&v.src()).is_some() {
                    match prog.borrow_value(v.src()).ty().kind() { TypeKind::Pointer(ty) => match ty.kind() { TypeKind::Array(ty, _) => ty.size(), _ => panic!() }, _ => panic!() }
                } else {
                    match funcdata.dfg().value(v.src()).ty().kind() { TypeKind::Pointer(ty) => match ty.kind() { TypeKind::Array(ty, _) => ty.size(), _ => panic!() }, _ => panic!() }
                };
                load_imm32(buf, "a1", step as i32);
                buf.push(LAInst::MulW { rd: "$a0".into(), rj: "$a0".into(), rk: "$a1".into() });
                load_operand(v.src(), funcdata, prog, info, buf, "a1");
                buf.push(LAInst::AddiD { rd: "$a0".into(), rj: "$a0".into(), imm: 0 }); // placeholder: add.d
                buf.pop();
                buf.push(LAInst::Directive("\tadd.d $a0, $a0, $a1".into()));
                store_result(*self, info, buf, "a0");
            },
            ValueKind::GetPtr(v) => {
                load_operand(v.index(), funcdata, prog, info, buf, "a0");
                match funcdata.dfg().value(v.src()).ty().kind() {
                    TypeKind::Pointer(ty) => { load_imm32(buf, "a1", ty.size() as i32); buf.push(LAInst::MulW { rd: "$a0".into(), rj: "$a0".into(), rk: "$a1".into() }); },
                    _ => panic!(),
                }
                load_operand(v.src(), funcdata, prog, info, buf, "a1");
                buf.push(LAInst::Directive("\tadd.d $a0, $a0, $a1".into()));
                store_result(*self, info, buf, "a0");
            },
            _ => todo!(),
        }
    }
}

fn emit_agg_store(funcdata: &FunctionData, info: &mut GenLAInfo, prog: &Program, buf: &mut Vec<LAInst>, agg: &values::Aggregate, mut base: usize) {
    for v in agg.elems() {
        match funcdata.dfg().value(v.clone()).kind() {
            ValueKind::Aggregate(v) => emit_agg_store(funcdata, info, prog, buf, v, base),
            ValueKind::Integer(v) => { load_imm32(buf, "a0", v.value()); buf.push(LAInst::StW { rd: "$a0".into(), rj: "$sp".into(), offset: base as i32 }); base += 4; },
            _ => { load_operand(*v, funcdata, prog, info, buf, "a0"); buf.push(LAInst::StW { rd: "$a0".into(), rj: "$sp".into(), offset: base as i32 }); base += 4; },
        }
    }
}

// ─── Public API ──────────────────────────────────────────────────────────────

pub fn generator_la(prog: &Program, writer: &mut impl Write) -> Result<()> {
    let mut info = GenLAInfo::new();
    let mut buf: Vec<LAInst> = Vec::new();
    prog.generate(&mut info, prog, &mut buf);
    optimize_la(&mut buf);
    for inst in &buf {
        inst.emit(writer)?;
    }
    Ok(())
}
