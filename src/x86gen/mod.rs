use std::io::{Write, Result};
use std::collections::{HashMap, HashSet};

use koopa::ir::*;
use koopa::back::NameManager;

/// x86-64 caller-saved registers (allocated first, flushed before calls)
const CALLER_SAVED_REGS: &[&str] = &["r8", "r9", "r10", "r11"];
/// x86-64 callee-saved registers (allocated when caller-saved exhausted, saved in prologue)
const CALLEE_SAVED_REGS: &[&str] = &["r12", "r13", "r14", "r15", "rbx"];

/// Live interval for linear scan register allocation
#[derive(Debug, Clone)]
struct LiveInterval {
    value: Value,
    start: usize,
    end: usize,
    reg: Option<String>,
    spilled: bool,
}

struct GenX64Info {
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

impl GenX64Info {
    fn new() -> Self {
        let caller_saved_set: HashSet<String> = CALLER_SAVED_REGS.iter().map(|s| s.to_string()).collect();
        GenX64Info {
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

    let mut mark_use = |intervals: &mut HashMap<Value, LiveInterval>, v: Value, idx: usize| {
        let interval = intervals.entry(v).or_insert_with(|| LiveInterval {
            value: v,
            start: usize::MAX,
            end: 0,
            reg: None,
            spilled: false,
        });
        if interval.start == usize::MAX {
            interval.start = idx;
        }
        interval.end = idx;
    };

    let mut mark_def = |intervals: &mut HashMap<Value, LiveInterval>, v: Value, idx: usize| {
        let interval = intervals.entry(v).or_insert_with(|| LiveInterval {
            value: v,
            start: usize::MAX,
            end: 0,
            reg: None,
            spilled: false,
        });
        if interval.start == usize::MAX {
            interval.start = idx;
        }
        interval.end = interval.end.max(idx);
    };

    let mut inst_idx = 0usize;

    for param in func_data.params() {
        mark_def(&mut intervals, *param, 0);
    }

    for (_bb, node) in func_data.layout().bbs() {
        for inst_val in node.insts().keys() {
            let inst_data = dfg.value(*inst_val);
            mark_def(&mut intervals, *inst_val, inst_idx);

            match inst_data.kind() {
                ValueKind::Integer(_) | ValueKind::Aggregate(_) | ValueKind::ZeroInit(_) => {},
                ValueKind::Alloc(_) => {},
                ValueKind::GlobalAlloc(_) => {},
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
                    for arg in v.args() {
                        mark_use(&mut intervals, *arg, inst_idx);
                    }
                },
                ValueKind::Return(v) => {
                    if let Some(ret_val) = v.value() {
                        mark_use(&mut intervals, ret_val, inst_idx);
                    }
                },
                ValueKind::Undef(_) | ValueKind::FuncArgRef(_) | ValueKind::BlockArgRef(_) => {}
            };

            inst_idx += 1;
        }
    }

    intervals.into_values()
        .filter(|i| i.start != usize::MAX)
        .collect()
}

fn linear_scan(func_data: &FunctionData, info: &mut GenX64Info) {
    let mut intervals = compute_live_intervals(func_data);
    if intervals.is_empty() {
        return;
    }

    intervals.sort_by_key(|i| i.start);

    // Build register pool: caller-saved first, then callee-saved
    let mut free_regs: Vec<String> = Vec::new();
    for reg in CALLEE_SAVED_REGS.iter().rev() {
        free_regs.push(reg.to_string());
    }
    for reg in CALLER_SAVED_REGS.iter().rev() {
        free_regs.push(reg.to_string());
    }

    let mut active: Vec<LiveInterval> = Vec::new();

    for mut interval in intervals {
        let mut i = 0;
        while i < active.len() {
            if active[i].end < interval.start {
                let expired = active.remove(i);
                if let Some(ref reg) = expired.reg {
                    free_regs.push(reg.clone());
                }
            } else {
                i += 1;
            }
        }

        if !free_regs.is_empty() {
            let reg = free_regs.pop().unwrap();
            interval.reg = Some(reg.clone());
            info.reg_map.insert(interval.value, reg.clone());
            if info.caller_saved_set.contains(&reg) {
                if !info.used_caller_regs.contains(&reg) {
                    info.used_caller_regs.push(reg.clone());
                }
            } else {
                if !info.used_callee_regs.contains(&reg) {
                    info.used_callee_regs.push(reg.clone());
                }
            }
        } else {
            let spill_idx = active.iter()
                .enumerate()
                .max_by_key(|(_, a)| a.end)
                .unwrap().0;

            if active[spill_idx].end > interval.end {
                let mut spilled = active.remove(spill_idx);
                let reg = spilled.reg.take().unwrap();

                let spill_slot = info.next_spill_offset;
                info.next_spill_offset += 4;
                info.spill_map.insert(spilled.value, spill_slot);
                spilled.spilled = true;

                interval.reg = Some(reg.clone());
                info.reg_map.insert(interval.value, reg.clone());
                active.push(spilled);
            } else {
                let spill_slot = info.next_spill_offset;
                info.next_spill_offset += 4;
                info.spill_map.insert(interval.value, spill_slot);
                interval.spilled = true;
                interval.reg = None;
            }
        }

        active.push(interval);
        active.sort_by_key(|a| a.end);
    }
}

/// Write an x86-64 memory operand: offset(%base)
fn mem_op(offset: i32, base: &str) -> String {
    if offset == 0 {
        format!("(%{})", base)
    } else {
        format!("{}(%{})", offset, base)
    }
}

/// Load a value into rax (or specified register)
fn load_operand(writer: &mut impl Write, value: Value, funcdata: &FunctionData, prog: &Program, info: &GenX64Info, dest: &str) -> Result<()> {
    if let ValueKind::Integer(v) = funcdata.dfg().value(value).kind() {
        return writeln!(writer, "\tmovq ${}, %{}", v.value(), dest);
    }
    if let Some(reg) = info.reg_map.get(&value) {
        writeln!(writer, "\tmovq %{}, %{}", reg, dest)?;
    } else if let Some(offset) = info.spill_map.get(&value) {
        writeln!(writer, "\tmovq {}(%rsp), %{}", offset + info.stackmem as usize, dest)?;
    } else if info.globalmap.get(&value).is_some() {
        writeln!(writer, "\tleaq {}(%rip), %{}", prog.borrow_value(value).name().as_ref().unwrap()[1..].to_string(), dest)?;
    } else if let Some(offset) = info.stackmap.get(&value) {
        writeln!(writer, "\tmovq {}(%rsp), %{}", offset, dest)?;
    } else {
        panic!("Cannot load value: {:?}", funcdata.dfg().value(value).kind());
    }
    Ok(())
}

/// Store a value from src register to the destination value's location
fn store_result(writer: &mut impl Write, dest: Value, info: &mut GenX64Info, src: &str) -> Result<()> {
    if let Some(offset) = info.stackmap.get(&dest) {
        writeln!(writer, "\tmovq %{}, {}(%rsp)", src, offset)?;
    }
    if let Some(reg) = info.reg_map.get(&dest) {
        writeln!(writer, "\tmovq %{}, %{}", src, reg)?;
    }
    Ok(())
}

/// x86-64 argument registers (System V AMD64 calling convention)
const ARG_REGS: &[&str] = &["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

trait GenerateAsm {
    fn generate(&self, writer: &mut impl Write, info: &mut GenX64Info, prog: &Program) -> Result<()>;
}

impl GenerateAsm for koopa::ir::Program {
    fn generate(&self, writer: &mut impl Write, info: &mut GenX64Info, prog: &Program) -> Result<()> {
        writeln!(writer, "\t.data")?;
        for value in self.inst_layout() {
            let value_data = &*self.borrow_value(value.clone());
            let name = info.namemgr.value_name(value_data)[1..].to_string();
            match value_data.kind() {
                ValueKind::GlobalAlloc(v) => {
                    info.globalmap.insert(value.clone(), 0);
                    let initval = v.init();
                    writeln!(writer, "\t.globl {}", name)?;
                    writeln!(writer, "\t.align 4")?;
                    writeln!(writer, "{}:", name)?;
                    match self.borrow_value(initval).kind() {
                        ValueKind::Integer(v) => {
                            writeln!(writer, "\t.long {}", v.value())?;
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
                                        ValueKind::Integer(v) => {
                                            writeln!(writer, "\t.long {}", v.value())?;
                                        },
                                        ValueKind::Aggregate(v) => {
                                            process_aggregate(writer, v, prog)?;
                                        },
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
            if funcdata.layout().entry_bb().is_none() {
                continue;
            }
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
    fn generate(&self, writer: &mut impl Write, info: &mut GenX64Info, prog: &Program) -> Result<()> {
        writeln!(writer, "\t.globl {}", self.name()[1..].to_string())?;
        writeln!(writer, "{}:", self.name()[1..].to_string())?;

        // Calculate stack offsets
        let mut offset = 0;
        for (v, data) in self.dfg().values().iter() {
            match data.kind() {
                ValueKind::Integer(_) => continue,
                ValueKind::Aggregate(_) => continue,
                ValueKind::Alloc(_) => {
                    match data.ty().kind() {
                        TypeKind::Pointer(ty) => {
                            info.allocmap.insert(v.clone(), offset);
                            offset += ty.size();
                        },
                        _ => panic!(),
                    }
                },
                _ => (),
            }
            info.stackmap.insert(v.clone(), offset);
            offset += data.ty().size();
        }
        info.stackmem = offset;

        // Run register allocation
        linear_scan(self, info);
        info.next_spill_offset += info.stackmem;

        // Emit prologue
        writeln!(writer, "\tpushq %rbp")?;
        writeln!(writer, "\tmovq %rsp, %rbp")?;

        // Save callee-saved registers
        for reg in &info.used_callee_regs {
            writeln!(writer, "\tpushq %{}", reg)?;
        }

        // Allocate stack space
        let total_stack = info.stackmem + info.next_spill_offset - info.stackmem;
        // 16-byte stack alignment
        let aligned_stack = (total_stack + 15) & !15;
        if aligned_stack > 0 {
            writeln!(writer, "\tsubq ${}, %rsp", aligned_stack)?;
        }

        // Process parameters: save to stack
        for (i, param) in self.params().iter().enumerate() {
            let arg_str = if i < 6 {
                format!("%{}", ARG_REGS[i])
            } else {
                let param_offset = (16 + (i - 6) * 8) as i32;
                format!("{}(%rbp)", param_offset)
            };
            writeln!(writer, "\tmovq {}, %rax", arg_str)?;
            writeln!(writer, "\tmovq %rax, {}(%rsp)", info.stackmap[param])?;
        }

        // Write basic blocks
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
    fn generate(&self, writer: &mut impl Write, funcdata: &FunctionData, info: &mut GenX64Info, prog: &Program) -> Result<()>;
}

impl _Gen for Value {
    fn generate(&self, writer: &mut impl Write, funcdata: &FunctionData, info: &mut GenX64Info, prog: &Program) -> Result<()> {
        match funcdata.dfg().value(self.clone()).kind() {
            ValueKind::Integer(_v) => panic!(),
            ValueKind::Aggregate(_v) => panic!(),
            ValueKind::Return(v) => {
                if let Some(v) = v.value() {
                    load_operand(writer, v, funcdata, prog, info, "rax")?;
                }
                // Restore stack and callee-saved registers
                let total_stack = info.stackmem + info.next_spill_offset - info.stackmem;
                let aligned_stack = (total_stack + 15) & !15;
                if aligned_stack > 0 {
                    writeln!(writer, "\taddq ${}, %rsp", aligned_stack)?;
                }
                for reg in info.used_callee_regs.iter().rev() {
                    writeln!(writer, "\tpopq %{}", reg)?;
                }
                writeln!(writer, "\tpopq %rbp")?;
                writeln!(writer, "\tret")?;
            }
            ValueKind::Alloc(v) => {
                writeln!(writer, "\tleaq {}(%rsp), %rax", info.allocmap[self])?;
                writeln!(writer, "\tmovq %rax, {}(%rsp)", info.stackmap[self])?;
            }
            ValueKind::Load(v) => {
                if info.globalmap.get(&v.src()).is_some() {
                    writeln!(writer, "\tleaq {}(%rip), %rax", prog.borrow_value(v.src()).name().as_ref().unwrap()[1..].to_string())?;
                    writeln!(writer, "\tmovq (%rax), %rax")?;
                } else {
                    load_operand(writer, v.src(), funcdata, prog, info, "rax")?;
                    writeln!(writer, "\tmovq (%rax), %rax")?;
                }
                store_result(writer, *self, info, "rax")?;
            }
            ValueKind::Store(v) => {
                let mut is_agg = false;
                match funcdata.dfg().value(v.value()).kind() {
                    ValueKind::Aggregate(agg) => {
                        is_agg = true;
                        fn process_aggregate(writer: &mut impl Write, funcdata: &FunctionData, info: &mut GenX64Info, prog: &Program, agg: &values::Aggregate, mut base: usize) -> Result<usize> {
                            let old_base = base;
                            for value in agg.elems() {
                                let mut is_agg = false;
                                match funcdata.dfg().value(value.clone()).kind() {
                                    ValueKind::Aggregate(v) => {
                                        base += process_aggregate(writer, funcdata, info, prog, v, base)?;
                                        is_agg = true;
                                    },
                                    ValueKind::Integer(v) => {
                                        writeln!(writer, "\tmovq ${}, %rax", v.value())?;
                                    },
                                    _ => load_operand(writer, *value, funcdata, prog, info, "rax")?,
                                }
                                if !is_agg {
                                    writeln!(writer, "\tmovq %rax, {}(%rsp)", base)?;
                                    base += 4;
                                }
                            }
                            Ok(base - old_base)
                        }
                        process_aggregate(writer, funcdata, info, prog, agg, info.allocmap[&v.dest()])?;
                    }
                    ValueKind::Integer(v) => writeln!(writer, "\tmovq ${}, %rax", v.value())?,
                    _ => load_operand(writer, v.value(), funcdata, prog, info, "rax")?,
                }
                if info.globalmap.get(&v.dest()).is_some() {
                    assert!(!is_agg);
                    writeln!(writer, "\tleaq {}(%rip), %rcx", prog.borrow_value(v.dest()).name().as_ref().unwrap()[1..].to_string())?;
                    writeln!(writer, "\tmovq %rax, (%rcx)")?;
                } else if !is_agg {
                    load_operand(writer, v.dest(), funcdata, prog, info, "rcx")?;
                    writeln!(writer, "\tmovq %rax, (%rcx)")?;
                }
            }
            ValueKind::Binary(v) => {
                load_operand(writer, v.lhs(), funcdata, prog, info, "rax")?;
                load_operand(writer, v.rhs(), funcdata, prog, info, "rcx")?;
                match v.op() {
                    BinaryOp::Add => writeln!(writer, "\taddq %rcx, %rax")?,
                    BinaryOp::Sub => writeln!(writer, "\tsubq %rcx, %rax")?,
                    BinaryOp::Mul => {
                        writeln!(writer, "\timulq %rcx, %rax")?;
                    },
                    BinaryOp::Div => {
                        writeln!(writer, "\tcqto")?;
                        writeln!(writer, "\tidivq %rcx")?;
                    },
                    BinaryOp::Mod => {
                        writeln!(writer, "\tcqto")?;
                        writeln!(writer, "\tidivq %rcx")?;
                        writeln!(writer, "\tmovq %rdx, %rax")?;
                    },
                    BinaryOp::And => writeln!(writer, "\tandq %rcx, %rax")?,
                    BinaryOp::Or => writeln!(writer, "\torq %rcx, %rax")?,
                    BinaryOp::Eq | BinaryOp::NotEq | BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Le | BinaryOp::Ge => {
                        writeln!(writer, "\tcmpq %rcx, %rax")?;
                        match v.op() {
                            BinaryOp::Eq => writeln!(writer, "\tsete %al")?,
                            BinaryOp::NotEq => writeln!(writer, "\tsetne %al")?,
                            BinaryOp::Lt => writeln!(writer, "\tsetl %al")?,
                            BinaryOp::Gt => writeln!(writer, "\tsetg %al")?,
                            BinaryOp::Le => writeln!(writer, "\tsetle %al")?,
                            BinaryOp::Ge => writeln!(writer, "\tsetge %al")?,
                            _ => unreachable!(),
                        }
                        writeln!(writer, "\tmovzbq %al, %rax")?;
                    },
                    _ => todo!()
                }
                store_result(writer, *self, info, "rax")?;
            }
            ValueKind::Branch(v) => {
                load_operand(writer, v.cond(), funcdata, prog, info, "rax")?;
                writeln!(writer, "\tcmpq $0, %rax")?;
                let true_branch = info.namemgr.bb_name(funcdata.dfg().bb(v.true_bb()));
                let false_branch = info.namemgr.bb_name(funcdata.dfg().bb(v.false_bb()));
                writeln!(writer, "\tjne .L{}_{}", funcdata.name()[1..].to_string(), true_branch[1..].to_string())?;
                writeln!(writer, "\tjmp .L{}_{}", funcdata.name()[1..].to_string(), false_branch[1..].to_string())?;
            }
            ValueKind::Jump(v) => {
                let target = info.namemgr.bb_name(funcdata.dfg().bb(v.target()));
                writeln!(writer, "\tjmp .L{}_{}", funcdata.name()[1..].to_string(), target[1..].to_string())?;
            }
            ValueKind::Call(v) => {
                let argcnt = v.args().iter().count();
                let mut stack_args = 0i32;

                // Flush caller-saved registers to stack before call
                for (value, reg) in &info.reg_map {
                    if info.caller_saved_set.contains(reg) {
                        if let Some(offset) = info.stackmap.get(value) {
                            writeln!(writer, "\tmovq %{}, {}(%rsp)", reg, offset)?;
                        }
                    }
                }

                // Process arguments
                for (i, arg) in v.args().iter().enumerate() {
                    if i < 6 {
                        load_operand(writer, *arg, funcdata, prog, info, ARG_REGS[i])?;
                    } else {
                        load_operand(writer, *arg, funcdata, prog, info, "rax")?;
                        writeln!(writer, "\tpushq %rax")?;
                        stack_args += 1;
                    }
                }

                // 16-byte alignment before call
                let push_bytes = stack_args * 8;
                if push_bytes % 16 != 0 {
                    writeln!(writer, "\tsubq $8, %rsp")?;
                }

                writeln!(writer, "\tcall {}", prog.func(v.callee()).name()[1..].to_string())?;

                // Clean up stack
                let clean_bytes = push_bytes + if push_bytes % 16 != 0 { 8 } else { 0 };
                if clean_bytes > 0 {
                    writeln!(writer, "\taddq ${}, %rsp", clean_bytes)?;
                }

                if !funcdata.dfg().value(self.clone()).ty().is_unit() {
                    store_result(writer, *self, info, "rax")?;
                }
            }
            ValueKind::GetElemPtr(v) => {
                load_operand(writer, v.index(), funcdata, prog, info, "rax")?;
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
                writeln!(writer, "\timulq ${}, %rax", step)?;
                load_operand(writer, v.src(), funcdata, prog, info, "rcx")?;
                writeln!(writer, "\taddq %rcx, %rax")?;
                store_result(writer, *self, info, "rax")?;
            }
            ValueKind::GetPtr(v) => {
                load_operand(writer, v.index(), funcdata, prog, info, "rax")?;
                match funcdata.dfg().value(v.src()).ty().kind() {
                    TypeKind::Pointer(ty) => {
                        writeln!(writer, "\timulq ${}, %rax", ty.size())?;
                    },
                    _ => panic!(),
                }
                load_operand(writer, v.src(), funcdata, prog, info, "rcx")?;
                writeln!(writer, "\taddq %rcx, %rax")?;
                store_result(writer, *self, info, "rax")?;
            }
            _ => {
                println!("{:#?}", funcdata.dfg().value(self.clone()).kind());
                todo!();
            }
        }
        Ok(())
    }
}

pub fn generator_x64(prog: &Program, writer: &mut impl Write) -> Result<()> {
    let mut info = GenX64Info::new();
    prog.generate(writer, &mut info, &prog)?;
    Ok(())
}
