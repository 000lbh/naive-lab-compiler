use std::io::{Write, Result};
use std::collections::{HashMap, HashSet};

use koopa::ir::*;
use koopa::back::NameManager;

/// LoongArch64 caller-saved registers (allocated first, flushed before calls)
/// t0-t7 = $r12-$r19  (8 regs)
const CALLER_SAVED_REGS: &[&str] = &["t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7"];
/// LoongArch64 callee-saved registers (allocated when caller-saved exhausted)
/// s0-s8 = $r23-$r31  (9 regs), $fp = $r22 is reserved as frame pointer
const CALLEE_SAVED_REGS: &[&str] = &["s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8"];
/// LoongArch argument registers (System V ABI: a0-a7 = $r4-$r11)
const ARG_REGS: &[&str] = &["a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"];

#[derive(Debug, Clone)]
struct LiveInterval {
    value: Value,
    start: usize,
    end: usize,
    reg: Option<String>,
    spilled: bool,
}

struct GenLAInfo {
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

impl GenLAInfo {
    fn new() -> Self {
        let caller_saved_set: HashSet<String> = CALLER_SAVED_REGS.iter().map(|s| s.to_string()).collect();
        GenLAInfo {
            stackmem: 0,
            stackmap: HashMap::new(),
            allocmap: HashMap::new(),
            globalmap: HashMap::new(),
            namemgr: NameManager::new(),
            reg_map: HashMap::new(),
            spill_map: HashMap::new(),
            used_callee_regs: Vec::new(),
            used_caller_regs: Vec::new(),
            caller_saved_set,
            next_spill_offset: 0,
        }
    }
    fn clear_local_info(&mut self) {
        self.stackmap = HashMap::new();
        self.allocmap = HashMap::new();
        self.stackmem = 0;
        self.reg_map = HashMap::new();
        self.spill_map = HashMap::new();
        self.used_callee_regs = Vec::new();
        self.used_caller_regs = Vec::new();
        self.next_spill_offset = 0;
    }
}

fn compute_live_intervals(func_data: &FunctionData) -> Vec<LiveInterval> {
    let dfg = func_data.dfg();
    let mut intervals: HashMap<Value, LiveInterval> = HashMap::new();

    let mark_use = |intervals: &mut HashMap<Value, LiveInterval>, v: Value, idx: usize| {
        let interval = intervals.entry(v).or_insert_with(|| LiveInterval {
            value: v, start: usize::MAX, end: 0, reg: None, spilled: false,
        });
        if interval.start == usize::MAX { interval.start = idx; }
        interval.end = idx;
    };

    let mark_def = |intervals: &mut HashMap<Value, LiveInterval>, v: Value, idx: usize| {
        let interval = intervals.entry(v).or_insert_with(|| LiveInterval {
            value: v, start: usize::MAX, end: 0, reg: None, spilled: false,
        });
        if interval.start == usize::MAX { interval.start = idx; }
        interval.end = interval.end.max(idx);
    };

    let mut inst_idx = 0usize;
    for param in func_data.params() { mark_def(&mut intervals, *param, 0); }
    for (_bb, node) in func_data.layout().bbs() {
        for inst_val in node.insts().keys() {
            let inst_data = dfg.value(*inst_val);
            mark_def(&mut intervals, *inst_val, inst_idx);
            match inst_data.kind() {
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

fn linear_scan(func_data: &FunctionData, info: &mut GenLAInfo) {
    let mut intervals = compute_live_intervals(func_data);
    if intervals.is_empty() { return; }
    intervals.sort_by_key(|i| i.start);

    // Build register pool: caller-saved first
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
            let spill_idx = active.iter().enumerate()
                .max_by_key(|(_, a)| a.end).unwrap().0;
            if active[spill_idx].end > interval.end {
                let mut spilled = active.remove(spill_idx);
                let reg = spilled.reg.take().unwrap();
                let slot = info.next_spill_offset;
                info.next_spill_offset += 4;
                info.spill_map.insert(spilled.value, slot);
                spilled.spilled = true;
                interval.reg = Some(reg.clone());
                info.reg_map.insert(interval.value, reg.clone());
                active.push(spilled);
            } else {
                let slot = info.next_spill_offset;
                info.next_spill_offset += 4;
                info.spill_map.insert(interval.value, slot);
                interval.spilled = true;
                interval.reg = None;
            }
        }
        active.push(interval);
        active.sort_by_key(|a| a.end);
    }
}

/// Write `st.w $src, $sp, $offset` or use scratch if offset too large
fn store_w_sp(writer: &mut impl Write, src: &str, offset: i32) -> Result<()> {
    if offset >= -2048 && offset < 2048 {
        writeln!(writer, "\tst.w ${}, $sp, {}", src, offset)
    } else {
        writeln!(writer, "\tlu12i.w $t8, {}", (offset as u32 >> 12) as i32)?;
        writeln!(writer, "\tori $t8, $t8, {}", offset & 0xfff)?;
        writeln!(writer, "\tadd.d $t8, $t8, $sp")?;
        writeln!(writer, "\tst.w ${}, $t8, 0", src)
    }
}

/// Write `ld.w $dest, $sp, $offset` or use scratch if offset too large
fn load_w_sp(writer: &mut impl Write, dest: &str, offset: i32) -> Result<()> {
    if offset >= -2048 && offset < 2048 {
        writeln!(writer, "\tld.w ${}, $sp, {}", dest, offset)
    } else {
        writeln!(writer, "\tlu12i.w $t8, {}", (offset as u32 >> 12) as i32)?;
        writeln!(writer, "\tori $t8, $t8, {}", offset & 0xfff)?;
        writeln!(writer, "\tadd.d $t8, $t8, $sp")?;
        writeln!(writer, "\tld.w ${}, $t8, 0", dest)
    }
}

/// Write `st.d $src, $sp, $offset` or use scratch if offset too large
fn store_d_sp(writer: &mut impl Write, src: &str, offset: i32) -> Result<()> {
    if offset >= -2048 && offset < 2048 {
        writeln!(writer, "\tst.d ${}, $sp, {}", src, offset)
    } else {
        writeln!(writer, "\tlu12i.w $t8, {}", (offset as u32 >> 12) as i32)?;
        writeln!(writer, "\tori $t8, $t8, {}", offset & 0xfff)?;
        writeln!(writer, "\tadd.d $t8, $t8, $sp")?;
        writeln!(writer, "\tst.d ${}, $t8, 0", src)
    }
}

/// Write `ld.d $dest, $sp, $offset` or use scratch if offset too large
fn load_d_sp(writer: &mut impl Write, dest: &str, offset: i32) -> Result<()> {
    if offset >= -2048 && offset < 2048 {
        writeln!(writer, "\tld.d ${}, $sp, {}", dest, offset)
    } else {
        writeln!(writer, "\tlu12i.w $t8, {}", (offset as u32 >> 12) as i32)?;
        writeln!(writer, "\tori $t8, $t8, {}", offset & 0xfff)?;
        writeln!(writer, "\tadd.d $t8, $t8, $sp")?;
        writeln!(writer, "\tld.d ${}, $t8, 0", dest)
    }
}

/// Load an immediate 32-bit value into a register
fn load_imm32(writer: &mut impl Write, dest: &str, val: i32) -> Result<()> {
    let v = val as u32;
    let hi20 = (v >> 12) as i32;
    let lo12 = (v & 0xfff) as i32;
    if hi20 == 0 && lo12 < 2048 {
        writeln!(writer, "\tori ${}, $zero, {}", dest, lo12)
    } else if lo12 == 0 {
        writeln!(writer, "\tlu12i.w ${}, {}", dest, hi20)
    } else {
        writeln!(writer, "\tlu12i.w ${}, {}", dest, hi20)?;
        writeln!(writer, "\tori ${}, ${}, {}", dest, dest, lo12)
    }
}

/// Move $src to $dest
fn move_reg(writer: &mut impl Write, dest: &str, src: &str) -> Result<()> {
    writeln!(writer, "\tor ${}, ${}, $zero", dest, src)
}

/// Load a value into $a0 (primary working register)
fn load_operand(writer: &mut impl Write, value: Value, funcdata: &FunctionData, prog: &Program, info: &GenLAInfo, dest: &str) -> Result<()> {
    if let ValueKind::Integer(v) = funcdata.dfg().value(value).kind() {
        return load_imm32(writer, dest, v.value());
    }
    if let Some(reg) = info.reg_map.get(&value) {
        move_reg(writer, dest, reg)?;
    } else if let Some(offset) = info.spill_map.get(&value) {
        load_w_sp(writer, dest, (*offset + info.stackmem) as i32)?;
    } else if info.globalmap.get(&value).is_some() {
        let sym = prog.borrow_value(value).name().as_ref().unwrap()[1..].to_string();
        writeln!(writer, "\tpcalau12i ${}, %pc_hi20({})", dest, sym)?;
        writeln!(writer, "\taddi.d ${}, ${}, %pc_lo12({})", dest, dest, sym)?;
    } else if let Some(offset) = info.stackmap.get(&value) {
        load_w_sp(writer, dest, *offset as i32)?;
    } else {
        panic!("Cannot load value");
    }
    Ok(())
}

/// Store result from $a0 to destination value's location
fn store_result(writer: &mut impl Write, dest: Value, info: &mut GenLAInfo, src: &str) -> Result<()> {
    if let Some(offset) = info.stackmap.get(&dest) {
        store_w_sp(writer, src, *offset as i32)?;
    }
    if let Some(reg) = info.reg_map.get(&dest) {
        move_reg(writer, reg, src)?;
    }
    Ok(())
}

trait GenerateAsm {
    fn generate(&self, writer: &mut impl Write, info: &mut GenLAInfo, prog: &Program) -> Result<()>;
}

impl GenerateAsm for koopa::ir::Program {
    fn generate(&self, writer: &mut impl Write, info: &mut GenLAInfo, prog: &Program) -> Result<()> {
        writeln!(writer, "\t.data")?;
        for value in self.inst_layout() {
            let value_data = &*self.borrow_value(value.clone());
            let name = info.namemgr.value_name(value_data)[1..].to_string();
            match value_data.kind() {
                ValueKind::GlobalAlloc(v) => {
                    info.globalmap.insert(value.clone(), 0);
                    let initval = v.init();
                    writeln!(writer, "\t.globl {}", name)?;
                    if prog.borrow_value(initval).ty().size() > 4 {
                        writeln!(writer, "\t.align 3")?;
                    } else {
                        writeln!(writer, "\t.align 2")?;
                    }
                    writeln!(writer, "{}:", name)?;
                    match self.borrow_value(initval).kind() {
                        ValueKind::Integer(v) => {
                            writeln!(writer, "\t.4byte {}", v.value())?;
                        },
                        ValueKind::ZeroInit(_) => {
                            let size = match value_data.ty().kind() {
                                TypeKind::Pointer(ty) => ty.size(),
                                _ => panic!(),
                            };
                            writeln!(writer, "\t.zero {}", size)?;
                        },
                        ValueKind::Aggregate(v) => {
                            fn process_aggregate(writer: &mut impl Write, agg: &values::Aggregate, prog: &Program) -> Result<()> {
                                for value in agg.elems() {
                                    match prog.borrow_value(value.clone()).kind() {
                                        ValueKind::Integer(v) => { writeln!(writer, "\t.4byte {}", v.value())?; },
                                        ValueKind::Aggregate(v) => { process_aggregate(writer, v, prog)?; },
                                        ValueKind::ZeroInit(_) => {
                                            writeln!(writer, "\t.zero {}", prog.borrow_value(value.clone()).ty().size())?;
                                        },
                                        _ => todo!()
                                    }
                                }
                                Ok(())
                            }
                            process_aggregate(writer, v, self)?;
                        },
                        _ => todo!()
                    }
                },
                _ => todo!()
            }
        }
        writeln!(writer, "\t.text")?;
        for (_, funcdata) in self.funcs() {
            if funcdata.layout().entry_bb().is_none() { continue; }
            info.namemgr.enter_func_scope();
            funcdata.generate(writer, info, prog)?;
            info.namemgr.exit_func_scope();
            info.clear_local_info();
            writeln!(writer, "")?;
        }
        Ok(())
    }
}

impl GenerateAsm for koopa::ir::FunctionData {
    fn generate(&self, writer: &mut impl Write, info: &mut GenLAInfo, prog: &Program) -> Result<()> {
        writeln!(writer, "\t.globl {}", self.name()[1..].to_string())?;
        writeln!(writer, "{}:", self.name()[1..].to_string())?;

        // Stack allocation
        let mut offset = 0;
        for (v, data) in self.dfg().values().iter() {
            match data.kind() {
                ValueKind::Integer(_) | ValueKind::Aggregate(_) => continue,
                ValueKind::Alloc(_) => {
                    match data.ty().kind() {
                        TypeKind::Pointer(ty) => { info.allocmap.insert(v.clone(), offset); offset += ty.size(); },
                        _ => panic!(),
                    }
                },
                _ => (),
            }
            info.stackmap.insert(v.clone(), offset);
            offset += data.ty().size();
        }
        info.stackmem = offset;

        // Register allocation
        linear_scan(self, info);
        info.next_spill_offset += info.stackmem;

        // Prologue: allocate frame, save $ra and $fp
        let total_stack = info.stackmem + info.next_spill_offset - info.stackmem;
        let callee_saves = info.used_callee_regs.len() * 8;
        // 16-byte alignment for stack frame
        let aligned_stack = ((total_stack + callee_saves + 16) + 15) & !15;
        let frame_size = aligned_stack as i32;

        // Build prologue
        // save $ra, $fp, callee-saved regs, then allocate locals
        let ra_offset = frame_size - 8;
        let fp_offset = frame_size - 16;
        writeln!(writer, "\taddi.d $sp, $sp, -{}", frame_size)?;
        store_d_sp(writer, "ra", ra_offset)?;
        store_d_sp(writer, "fp", fp_offset)?;
        // Save callee-saved registers
        let mut csr_offset = fp_offset - 8;
        for reg in &info.used_callee_regs {
            store_d_sp(writer, reg, csr_offset)?;
            csr_offset -= 8;
        }
        writeln!(writer, "\tor $fp, $sp, $zero")?;

        // Save function parameters to local stack slots
        for (i, param) in self.params().iter().enumerate() {
            if i < 8 {
                store_w_sp(writer, ARG_REGS[i], info.stackmap[param] as i32)?;
            } else {
                let param_offset = (frame_size + (i as i32 - 8) * 8) as i32;
                load_d_sp(writer, "a0", param_offset)?;
                store_w_sp(writer, "a0", info.stackmap[param] as i32)?;
            }
        }

        // Basic blocks
        for (bb, node) in self.layout().bbs() {
            let bb_name = info.namemgr.bb_name(self.dfg().bb(*bb));
            writeln!(writer, "\t.L{}_{}:", self.name()[1..].to_string(), bb_name[1..].to_string())?;
            for inst_val in node.insts().keys() {
                inst_val.generate(writer, self, info, prog)?;
            }
        }
        Ok(())
    }
}

trait _Gen {
    fn generate(&self, writer: &mut impl Write, funcdata: &FunctionData, info: &mut GenLAInfo, prog: &Program) -> Result<()>;
}

impl _Gen for Value {
    fn generate(&self, writer: &mut impl Write, funcdata: &FunctionData, info: &mut GenLAInfo, prog: &Program) -> Result<()> {
        match funcdata.dfg().value(self.clone()).kind() {
            ValueKind::Integer(_v) => panic!(),
            ValueKind::Aggregate(_v) => panic!(),
            ValueKind::Return(v) => {
                if let Some(v) = v.value() {
                    load_operand(writer, v, funcdata, prog, info, "a0")?;
                }
                // Epilogue: restore callee-saved, $fp, $ra, deallocate, return
                let total_stack = info.stackmem + info.next_spill_offset - info.stackmem;
                let callee_saves = info.used_callee_regs.len() * 8;
                let aligned_stack = ((total_stack + callee_saves + 16) + 15) & !15;
                let frame_size = aligned_stack as i32;
                let ra_offset = frame_size - 8;
                let fp_offset = frame_size - 16;

                // Restore callee-saved in reverse
                let mut csr_offset = fp_offset - 8 - ((info.used_callee_regs.len() as i32 - 1) * 8);
                for reg in info.used_callee_regs.iter().rev() {
                    load_d_sp(writer, reg, csr_offset)?;
                    csr_offset += 8;
                }

                load_d_sp(writer, "fp", fp_offset)?;
                load_d_sp(writer, "ra", ra_offset)?;
                writeln!(writer, "\taddi.d $sp, $sp, {}", frame_size)?;
                writeln!(writer, "\tjirl $r0, $r1, 0")?;
            }
            ValueKind::Alloc(_v) => {
                load_imm32(writer, "a0", info.allocmap[self] as i32)?;
                writeln!(writer, "\tadd.d $a0, $a0, $sp")?;
                store_w_sp(writer, "a0", info.stackmap[self] as i32)?;
            }
            ValueKind::Load(v) => {
                if info.globalmap.get(&v.src()).is_some() {
                    let sym = prog.borrow_value(v.src()).name().as_ref().unwrap()[1..].to_string();
                    writeln!(writer, "\tpcalau12i $a0, %pc_hi20({})", sym)?;
                    writeln!(writer, "\taddi.d $a0, $a0, %pc_lo12({})", sym)?;
                    writeln!(writer, "\tld.w $a0, $a0, 0")?;
                } else {
                    load_operand(writer, v.src(), funcdata, prog, info, "a0")?;
                    writeln!(writer, "\tld.w $a0, $a0, 0")?;
                }
                store_result(writer, *self, info, "a0")?;
            }
            ValueKind::Store(v) => {
                let mut is_agg = false;
                match funcdata.dfg().value(v.value()).kind() {
                    ValueKind::Aggregate(agg) => {
                        is_agg = true;
                        fn proc_agg(w: &mut impl Write, fd: &FunctionData, inf: &mut GenLAInfo, p: &Program, agg: &values::Aggregate, mut base: usize) -> Result<usize> {
                            let old_base = base;
                            for value in agg.elems() {
                                let mut is_agg = false;
                                match fd.dfg().value(value.clone()).kind() {
                                    ValueKind::Aggregate(v) => { base += proc_agg(w, fd, inf, p, v, base)?; is_agg = true; },
                                    ValueKind::Integer(v) => { load_imm32(w, "a0", v.value())?; },
                                    _ => load_operand(w, *value, fd, p, inf, "a0")?,
                                }
                                if !is_agg { store_w_sp(w, "a0", base as i32)?; base += 4; }
                            }
                            Ok(base - old_base)
                        }
                        proc_agg(writer, funcdata, info, prog, agg, info.allocmap[&v.dest()])?;
                    }
                    ValueKind::Integer(v) => load_imm32(writer, "a0", v.value())?,
                    _ => load_operand(writer, v.value(), funcdata, prog, info, "a0")?,
                }
                if info.globalmap.get(&v.dest()).is_some() {
                    assert!(!is_agg);
                    let sym = prog.borrow_value(v.dest()).name().as_ref().unwrap()[1..].to_string();
                    writeln!(writer, "\tpcalau12i $t8, %pc_hi20({})", sym)?;
                    writeln!(writer, "\taddi.d $t8, $t8, %pc_lo12({})", sym)?;
                    writeln!(writer, "\tst.w $a0, $t8, 0")?;
                } else if !is_agg {
                    load_operand(writer, v.dest(), funcdata, prog, info, "t8")?;
                    writeln!(writer, "\tst.w $a0, $t8, 0")?;
                }
            }
            ValueKind::Binary(v) => {
                load_operand(writer, v.lhs(), funcdata, prog, info, "a0")?;
                load_operand(writer, v.rhs(), funcdata, prog, info, "a1")?;
                match v.op() {
                    BinaryOp::Add => writeln!(writer, "\tadd.w $a0, $a0, $a1")?,
                    BinaryOp::Sub => writeln!(writer, "\tsub.w $a0, $a0, $a1")?,
                    BinaryOp::Mul => writeln!(writer, "\tmul.w $a0, $a0, $a1")?,
                    BinaryOp::Div => writeln!(writer, "\tdiv.w $a0, $a0, $a1")?,
                    BinaryOp::Mod => writeln!(writer, "\tmod.w $a0, $a0, $a1")?,
                    BinaryOp::And => writeln!(writer, "\tand $a0, $a0, $a1")?,
                    BinaryOp::Or => writeln!(writer, "\tor $a0, $a0, $a1")?,
                    BinaryOp::Eq => {
                        writeln!(writer, "\txor $a0, $a0, $a1")?;
                        writeln!(writer, "\tsltui $a0, $a0, 1")?;
                    },
                    BinaryOp::NotEq => {
                        writeln!(writer, "\txor $a0, $a0, $a1")?;
                        writeln!(writer, "\tsltu $a0, $zero, $a0")?;
                    },
                    BinaryOp::Lt => writeln!(writer, "\tslt $a0, $a0, $a1")?,
                    BinaryOp::Gt => writeln!(writer, "\tslt $a0, $a1, $a0")?,
                    BinaryOp::Le => {
                        writeln!(writer, "\tslt $a0, $a1, $a0")?;
                        writeln!(writer, "\txori $a0, $a0, 1")?;
                    },
                    BinaryOp::Ge => {
                        writeln!(writer, "\tslt $a0, $a0, $a1")?;
                        writeln!(writer, "\txori $a0, $a0, 1")?;
                    },
                    _ => todo!()
                }
                store_result(writer, *self, info, "a0")?;
            }
            ValueKind::Branch(v) => {
                load_operand(writer, v.cond(), funcdata, prog, info, "a0")?;
                let true_br = info.namemgr.bb_name(funcdata.dfg().bb(v.true_bb()));
                let false_br = info.namemgr.bb_name(funcdata.dfg().bb(v.false_bb()));
                writeln!(writer, "\tbnez $a0, .L{}_{}", funcdata.name()[1..].to_string(), true_br[1..].to_string())?;
                writeln!(writer, "\tb .L{}_{}", funcdata.name()[1..].to_string(), false_br[1..].to_string())?;
            }
            ValueKind::Jump(v) => {
                let target = info.namemgr.bb_name(funcdata.dfg().bb(v.target()));
                writeln!(writer, "\tb .L{}_{}", funcdata.name()[1..].to_string(), target[1..].to_string())?;
            }
            ValueKind::Call(v) => {
                let argcnt = v.args().iter().count();
                let mut stack_args = 0i32;

                // Flush caller-saved registers to stack before call
                for (value, reg) in &info.reg_map {
                    if info.caller_saved_set.contains(reg) {
                        if let Some(offset) = info.stackmap.get(value) {
                            store_w_sp(writer, reg, *offset as i32)?;
                        }
                    }
                }

                // Pass arguments
                for (i, arg) in v.args().iter().enumerate() {
                    if i < 8 {
                        load_operand(writer, *arg, funcdata, prog, info, ARG_REGS[i])?;
                    } else {
                        load_operand(writer, *arg, funcdata, prog, info, "a0")?;
                        writeln!(writer, "\taddi.d $sp, $sp, -8")?;
                        writeln!(writer, "\tst.d $a0, $sp, 0")?;
                        stack_args += 1;
                    }
                }

                // 16-byte alignment
                let push_bytes = stack_args * 8;
                if push_bytes % 16 != 0 {
                    writeln!(writer, "\taddi.d $sp, $sp, -8")?;
                }

                writeln!(writer, "\tbl {}", prog.func(v.callee()).name()[1..].to_string())?;

                // Clean up stack
                let clean_bytes = push_bytes + if push_bytes % 16 != 0 { 8 } else { 0 };
                if clean_bytes > 0 {
                    writeln!(writer, "\taddi.d $sp, $sp, {}", clean_bytes)?;
                }

                if !funcdata.dfg().value(self.clone()).ty().is_unit() {
                    store_result(writer, *self, info, "a0")?;
                }
            }
            ValueKind::GetElemPtr(v) => {
                load_operand(writer, v.index(), funcdata, prog, info, "a0")?;
                let step = if info.globalmap.get(&v.src()).is_some() {
                    match prog.borrow_value(v.src()).ty().kind() {
                        TypeKind::Pointer(ty) => match ty.kind() {
                            TypeKind::Array(ty, _len) => ty.size(),
                            _ => panic!(),
                        },
                        _ => panic!(),
                    }
                } else {
                    match funcdata.dfg().value(v.src()).ty().kind() {
                        TypeKind::Pointer(ty) => match ty.kind() {
                            TypeKind::Array(ty, _len) => ty.size(),
                            _ => panic!(),
                        },
                        _ => panic!(),
                    }
                };
                load_imm32(writer, "a1", step as i32)?;
                writeln!(writer, "\tmul.w $a0, $a0, $a1")?;
                load_operand(writer, v.src(), funcdata, prog, info, "a1")?;
                writeln!(writer, "\tadd.d $a0, $a0, $a1")?;
                store_result(writer, *self, info, "a0")?;
            }
            ValueKind::GetPtr(v) => {
                load_operand(writer, v.index(), funcdata, prog, info, "a0")?;
                match funcdata.dfg().value(v.src()).ty().kind() {
                    TypeKind::Pointer(ty) => {
                        load_imm32(writer, "a1", ty.size() as i32)?;
                        writeln!(writer, "\tmul.w $a0, $a0, $a1")?;
                    },
                    _ => panic!(),
                }
                load_operand(writer, v.src(), funcdata, prog, info, "a1")?;
                writeln!(writer, "\tadd.d $a0, $a0, $a1")?;
                store_result(writer, *self, info, "a0")?;
            }
            _ => { println!("{:#?}", funcdata.dfg().value(self.clone()).kind()); todo!(); }
        }
        Ok(())
    }
}

pub fn generator_la(prog: &Program, writer: &mut impl Write) -> Result<()> {
    let mut info = GenLAInfo::new();
    prog.generate(writer, &mut info, &prog)?;
    Ok(())
}
