use std::io::{Write, Result};
use std::collections::HashMap;

use koopa::ir::*;
use koopa::back::NameManager;

macro_rules! add_imm {
    ($writer: expr, $reg: expr, $imm: expr) => {
        if $imm < 2048 && $imm != 0 {
            writeln!($writer, "addi {}, {}, {}", $reg, $reg, $imm)?;
        }
        else if $imm != 0 {
            writeln!($writer, "li t0, {}", $imm)?;
            writeln!($writer, "add {}, {}, t0", $reg, $reg)?;
        }
    };
}

macro_rules! sub_imm {
    ($writer: expr, $reg: expr, $imm: expr) => {
        if $imm <= 2048 && $imm != 0 {
            writeln!($writer, "addi {}, {}, -{}", $reg, $reg, $imm)?;
        }
        else if $imm != 0 {
            writeln!($writer, "li t0, {}", $imm)?;
            writeln!($writer, "sub {}, {}, t0", $reg, $reg)?;
        }
    };
}

macro_rules! load_from_stack {
    ($writer: expr, $offset: expr, $dest: expr) => {
        if ($offset < 2048) {
            writeln!($writer, "lw {}, {}(sp)", $dest, $offset)?;
        }
        else {
            writeln!($writer, "li {}, {}", $dest, $offset)?;
            writeln!($writer, "add {}, {}, sp", $dest, $dest)?;
            writeln!($writer, "lw {}, ({})", $dest, $dest)?;
        }
    };
}

fn load(writer: &mut impl Write, src: Value, prog: &Program, info: &GenRiscvInfo, reg: &str) -> Result<()> {
    if info.globalmap.get(&src).is_some() {
        writeln!(writer, "la a0, {}", prog.borrow_value(src).name().as_ref().unwrap()[1..].to_string())?;
    } else {
        load_from_stack!(writer, info.stackmap[&src], reg);
    }
    Ok(())
}

macro_rules! store_to_stack {
    ($writer: expr, $offset: expr, $src: expr) => {
        if ($offset < 2048) {
            writeln!($writer, "sw {}, {}(sp)", $src, $offset)?;
        }
        else {
            writeln!($writer, "li t0, {}", $offset)?;
            writeln!($writer, "add t0, t0, sp")?;
            writeln!($writer, "sw {}, (t0)", $src)?;
        }
    };
}

fn store(writer: &mut impl Write, dest: Value, prog: &Program, info: &GenRiscvInfo, reg: &str) -> Result<()> {
    if info.globalmap.get(&dest).is_some() {
        writeln!(writer, "la t0, {}", prog.borrow_value(dest).name().as_ref().unwrap()[1..].to_string())?;
        writeln!(writer, "sw a0, (t0)")?;
    } else {
        store_to_stack!(writer, info.stackmap[&dest], reg);
    }
    Ok(())
}

struct GenRiscvInfo {
    stackmem: usize,
    stackmap: HashMap<Value, usize>,
    allocmap: HashMap<Value, usize>,
    globalmap: HashMap<Value, usize>,
    namemgr: NameManager,
}

impl GenRiscvInfo {
    fn new() -> Self {
        GenRiscvInfo {
            stackmem: 0,
            stackmap: HashMap::new(),
            allocmap: HashMap::new(),
            globalmap: HashMap::new(),
            namemgr: NameManager::new(),
        }
    }
    fn clear_local_info(&mut self) {
        self.stackmap = HashMap::new();
        self.stackmem = 0;
    }
}

trait GenerateAsm {
    fn generate(&self, writer: &mut impl Write, info: &mut GenRiscvInfo, prog: &Program) -> Result<()>;
}

impl GenerateAsm for koopa::ir::Program {
    fn generate(&self, writer: &mut impl Write, info: &mut GenRiscvInfo, prog: &Program) -> Result<()> {
        // write data seg
        writeln!(writer, ".data")?;
        let mut global_offset = 0usize;
        for value in self.inst_layout() {
            let value_data = &*self.borrow_value(value.clone());
            let name = info.namemgr.value_name(value_data)[1..].to_string();
            match value_data.kind() {
                ValueKind::GlobalAlloc(v) => {
                    info.globalmap.insert(value.clone(), global_offset);
                    let initval = v.init();
                    global_offset += prog.borrow_value(initval).ty().size();
                    writeln!(writer, ".global {}", name)?;
                    writeln!(writer, "{}:", name)?;
                    match self.borrow_value(initval).kind() {
                        ValueKind::Integer(v) => {
                            writeln!(writer, ".word {}", v.value())?;
                        },
                        ValueKind::ZeroInit(v) => {
                            let size = match value_data.ty().kind() {
                                TypeKind::Pointer(ty) => ty.size(),
                                _ => panic!(),
                            };
                            writeln!(writer, ".zero {}", size)?;
                        },
                        ValueKind::Aggregate(v) => {
                            fn process_aggregate(writer: &mut impl Write, agg: &values::Aggregate, prog: &Program) -> Result<()> {
                                for value in agg.elems() {
                                    match prog.borrow_value(value.clone()).kind() {
                                        ValueKind::Integer(v) => {
                                            writeln!(writer, ".word {}", v.value())?;
                                        },
                                        ValueKind::Aggregate(v) => {
                                            process_aggregate(writer, v, prog)?;
                                        },
                                        ValueKind::ZeroInit(v) => {
                                            writeln!(writer, ".zero {}", prog.borrow_value(value.clone()).ty().size())?;
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

        // write code seg
        writeln!(writer, ".text")?;

        // generate funcs
        for (_, funcdata) in self.funcs() {
            // Skip declarations
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
    fn generate(&self, writer: &mut impl Write, info: &mut GenRiscvInfo, prog: &Program) -> Result<()> {
        // write symbol info
        writeln!(writer, ".global {}", self.name()[1..].to_string())?;
        writeln!(writer, "{}:", self.name()[1..].to_string())?;
        // calculate stack info
        let mut offset = 0;
        for (v, data) in self.dfg().values().iter() {
            match data.kind() {
                ValueKind::Integer(_) => continue,
                ValueKind::Aggregate(_) => continue,
                ValueKind::Alloc(_) => {
                    match data.ty().kind() {
                        TypeKind::Pointer(ty) => {
                            info.allocmap.insert(v.clone(), offset);
                            println!("Allocated var {:#?} at offset {}", ty, offset);
                            offset += ty.size();
                        },
                        _ => panic!(),
                    }
                },
                _ => (),
            }
            info.stackmap.insert(v.clone(), offset);
            println!("Allocated tmpvar {:#?} at offset {}", data.ty(), offset);
            offset += data.ty().size();
        }
        info.stackmem = offset;
        writeln!(writer, "addi sp, sp, -4")?;
        writeln!(writer, "sw ra, (sp)")?;
        writeln!(writer, "addi sp, sp, -4")?;
        writeln!(writer, "sw fp, (sp)")?;
        writeln!(writer, "mv fp, sp")?;
        if info.stackmem <= 2048{
            writeln!(writer, "addi sp, sp, -{}", info.stackmem)?;
        } else {
            writeln!(writer, "li t0, -{}", info.stackmem)?;
            writeln!(writer, "add sp, t0, sp")?;
        }
        // process parameters
        for (i, param) in self.params().iter().enumerate() {
            if i < 8 {
                store_to_stack!(writer, info.stackmap[param], format!("a{}", i));
            } else {
                load_from_stack!(writer, info.stackmem + (i - 8 + 2) * 4, "a0");
                store_to_stack!(writer, info.stackmap[param], "a0");
            }
        }
        // write blocks
        for (bb, node) in self.layout().bbs() {
            let bb_name = info.namemgr.bb_name(self.dfg().bb(*bb));
            writeln!(writer, "label_{}_{}:", self.name()[1..].to_string(), bb_name[1..].to_string())?;
            for inst_val in node.insts().keys() {
                inst_val.generate(writer, self, info, prog)?;
            }
        }
        Ok(())
    }
}

trait _Gen {
    fn generate(&self, writer: &mut impl Write, funcdata: &FunctionData, info: &mut GenRiscvInfo, prog: &Program) -> Result<()>;
}

impl _Gen for Value {
    fn generate(&self, writer: &mut impl Write, funcdata: &FunctionData, info: &mut GenRiscvInfo, prog: &Program) -> Result<()> {
        match funcdata.dfg().value(self.clone()).kind() {
            ValueKind::Integer(_v) => {
                panic!()
            }
            ValueKind::Aggregate(_v) => {
                panic!()
            }
            ValueKind::Return(v) => {
                if let Some(v) = v.value() {
                    match funcdata.dfg().value(v).kind() {
                        ValueKind::Integer(v) => writeln!(writer, "li a0, {}", v.value())?,
                        _ => load_from_stack!(writer, info.stackmap[&v], "a0"),
                    }
                }
                writeln!(writer, "mv sp, fp")?;
                writeln!(writer, "lw fp, (sp)")?;
                writeln!(writer, "addi sp, sp, 4")?;
                writeln!(writer, "lw ra, (sp)")?;
                writeln!(writer, "addi sp, sp, 4")?;
                writeln!(writer, "ret")?;
            }
            ValueKind::Alloc(v) => {
                writeln!(writer, "li a0, {}", info.allocmap[self])?;
                writeln!(writer, "add a0, a0, sp")?;
                store_to_stack!(writer, info.stackmap[self], "a0");
            }
            ValueKind::Load(v) => {
                if let Some(i) = info.globalmap.get(&v.src()) {
                    writeln!(writer, "la a0, {}", prog.borrow_value(v.src()).name().as_ref().unwrap()[1..].to_string())?;
                    writeln!(writer, "lw a0, (a0)")?;
                } else {
                    load_from_stack!(writer, info.stackmap[&v.src()], "a0");
                    writeln!(writer, "lw a0, (a0)")?;
                }
                store_to_stack!(writer, info.stackmap[self], "a0");
            }
            ValueKind::Store(v) => {
                let mut is_agg = false;
                match funcdata.dfg().value(v.value()).kind() {
                    ValueKind::Aggregate(agg) => {
                        is_agg = true;
                        fn process_aggregate(writer: &mut impl Write, funcdata: &FunctionData, info: &mut GenRiscvInfo, prog: &Program, agg: &values::Aggregate, mut base: usize) -> Result<usize> {
                            let old_base = base;
                            for value in agg.elems() {
                                let mut is_agg = false;
                                match funcdata.dfg().value(value.clone()).kind() {
                                    ValueKind::Aggregate(v) => {
                                        base += process_aggregate(writer, funcdata, info, prog, v, base)?;
                                        is_agg = true;
                                    },
                                    ValueKind::Integer(v) => {
                                        writeln!(writer, "li a0, {}", v.value())?;
                                    },
                                    _ => load_from_stack!(writer, info.stackmap[value], "a0"),
                                }
                                if !is_agg {
                                    store_to_stack!(writer, base, "a0");
                                    base += 4;
                                }
                            }

                            Ok(base - old_base)
                        }
                        process_aggregate(writer, funcdata, info, prog, agg, info.allocmap[&v.dest()])?;
                    }
                    ValueKind::Integer(v) => writeln!(writer, "li a0, {}", v.value())?,
                    _ => load_from_stack!(writer, info.stackmap[&v.value()], "a0"),
                }
                // Aggregate value never save to global variables
                if let Some(i) = info.globalmap.get(&v.dest()) {
                    assert!(!is_agg);
                    writeln!(writer, "la t0, {}", prog.borrow_value(v.dest()).name().as_ref().unwrap()[1..].to_string())?;
                    writeln!(writer, "sw a0, (t0)")?;
                } else if !is_agg {
                    load_from_stack!(writer, info.stackmap[&v.dest()], "a1");
                    writeln!(writer, "sw a0, (a1)")?;
                }
            }
            ValueKind::Binary(v) => {
                match funcdata.dfg().value(v.lhs()).kind() {
                    ValueKind::Integer(v) => writeln!(writer, "li a0, {}", v.value())?,
                    _ => load_from_stack!(writer, info.stackmap[&v.lhs()], "a0"),
                }
                match funcdata.dfg().value(v.rhs()).kind() {
                    ValueKind::Integer(v) => writeln!(writer, "li a1, {}", v.value())?,
                    _ => load_from_stack!(writer, info.stackmap[&v.rhs()], "a1"),
                }
                match v.op() {
                    BinaryOp::Add => {
                        writeln!(writer, "add a0, a0, a1")?;
                    },
                    BinaryOp::Div => {
                        writeln!(writer, "div a0, a0, a1")?;
                    },
                    BinaryOp::And => {
                        writeln!(writer, "and a0, a0, a1")?;
                    },
                    BinaryOp::Eq => {
                        writeln!(writer, "sub a0, a0, a1")?;
                        writeln!(writer, "seqz a0, a0")?;
                    },
                    BinaryOp::Gt => {
                        writeln!(writer, "slt a0, a1, a0")?;
                    },
                    BinaryOp::Lt => {
                        writeln!(writer, "slt a0, a0, a1")?;
                    },
                    BinaryOp::Ge => {
                        writeln!(writer, "sub a2, a0, a1")?;
                        writeln!(writer, "seqz a2, a2")?;
                        writeln!(writer, "slt a0, a1, a0")?;
                        writeln!(writer, "or a0, a0, a2")?;
                    },
                    BinaryOp::Le => {
                        writeln!(writer, "sub a2, a0, a1")?;
                        writeln!(writer, "seqz a2, a2")?;
                        writeln!(writer, "slt a0, a0, a1")?;
                        writeln!(writer, "or a0, a0, a2")?;
                    },
                    BinaryOp::Mod => {
                        writeln!(writer, "rem a0, a0, a1")?;
                    },
                    BinaryOp::Mul => {
                        writeln!(writer, "mul a0, a0, a1")?;
                    },
                    BinaryOp::NotEq => {
                        writeln!(writer, "sub a0, a0, a1")?;
                        writeln!(writer, "snez a0, a0")?;
                    },
                    BinaryOp::Or => {
                        writeln!(writer, "or a0, a0, a1")?;
                    },
                    BinaryOp::Sub => {
                        writeln!(writer, "sub a0, a0, a1")?;
                    },
                    _ => {
                        todo!()
                    }
                }
                store_to_stack!(writer, info.stackmap[self], "a0");
            }
            ValueKind::Branch(v) => {
                match funcdata.dfg().value(v.cond()).kind() {
                    ValueKind::Integer(v) => writeln!(writer, "li a0, {}", v.value())?,
                    _ => load_from_stack!(writer, info.stackmap[&v.cond()], "a0"),
                }
                let true_branch = info.namemgr.bb_name(funcdata.dfg().bb(v.true_bb()));
                let false_branch = info.namemgr.bb_name(funcdata.dfg().bb(v.false_bb()));
                writeln!(writer, "bnez a0, label_{}_{}", funcdata.name()[1..].to_string(), true_branch[1..].to_string())?;
                writeln!(writer, "j label_{}_{}", funcdata.name()[1..].to_string(), false_branch[1..].to_string())?;
            }
            ValueKind::Jump(v) => {
                let target = info.namemgr.bb_name(funcdata.dfg().bb(v.target()));
                writeln!(writer, "j label_{}_{}", funcdata.name()[1..].to_string(), target[1..].to_string())?;
            }
            ValueKind::Call(v) => {
                let argcnt = v.args().iter().count();
                let mut stack_size = 0;
                if argcnt > 8 {
                    stack_size += (argcnt - 8) * 4;
                }
                for (i, arg) in v.args().iter().rev().enumerate() {
                    if i + 8 >= argcnt && i < argcnt {
                        let offset = argcnt - 1 - i;
                        match funcdata.dfg().value(*arg).kind() {
                            ValueKind::Integer(v) => writeln!(writer, "li a{}, {}", offset, v.value())?,
                            _ => load_from_stack!(writer, info.stackmap[arg], format!("a{}", offset)),
                        }
                    } else {
                        match funcdata.dfg().value(*arg).kind() {
                            ValueKind::Integer(v) => writeln!(writer, "li a0, {}", v.value())?,
                            _ => load_from_stack!(writer, info.stackmap[arg], "a0"),
                        }
                        store_to_stack!(writer, -4 - 4 * i as i32, "a0");
                    }
                }
                sub_imm!(writer, "sp", stack_size);
                writeln!(writer, "call {}", prog.func(v.callee()).name()[1..].to_string())?;
                add_imm!(writer, "sp", stack_size);
                if !funcdata.dfg().value(self.clone()).ty().is_unit() {
                    store_to_stack!(writer, info.stackmap[self], "a0");
                }
            }
            ValueKind::GetElemPtr(v) => {
                if let ValueKind::Integer(i) = funcdata.dfg().value(v.index()).kind() {
                    writeln!(writer, "li a0, {}", i.value())?;
                } else {
                    load(writer, v.index(), prog, info, "a0")?;
                }
                let step = if info.globalmap.get(&v.src()).is_some() {
                    match prog.borrow_value(v.src()).ty().kind() {
                        TypeKind::Pointer(ty) => {
                            match ty.kind() {
                                TypeKind::Array(ty, _len) => ty.size(),
                                _ => panic!(),
                            }
                        },
                        _ => panic!(),
                    }
                } else {
                    match funcdata.dfg().value(v.src()).ty().kind() {
                        TypeKind::Pointer(ty) => {
                            match ty.kind() {
                                TypeKind::Array(ty, _len) => ty.size(),
                                _ => panic!(),
                            }
                        },
                        _ => panic!(),
                    }
                };
                writeln!(writer, "li a1, {}", step)?;
                writeln!(writer, "mul a1, a0, a1")?;
                load(writer, v.src(), prog, info, "a0")?;
                writeln!(writer, "add a0, a0, a1")?;
                store_to_stack!(writer, info.stackmap[self], "a0");
            }
            ValueKind::GetPtr(v) => {
                if let ValueKind::Integer(i) = funcdata.dfg().value(v.index()).kind() {
                    writeln!(writer, "li a0, {}", i.value())?;
                } else {
                    load(writer, v.index(), prog, info, "a0")?;
                }
                match funcdata.dfg().value(v.src()).ty().kind() {
                    TypeKind::Pointer(ty) => {
                        writeln!(writer, "li a1, {}", ty.size())?;
                        writeln!(writer, "mul a0, a0, a1")?;
                    },
                    _ => panic!(),
                }
                load_from_stack!(writer, info.stackmap[&v.src()], "a1");
                writeln!(writer, "add a0, a0, a1")?;
                store_to_stack!(writer, info.stackmap[self], "a0");
            }
            _ => {
                println!("{:#?}", funcdata.dfg().value(self.clone()).kind());
                todo!();
            }
        }
        Ok(())
    }
}

pub fn generator_rv(prog: &Program, writer: &mut impl Write) -> Result<()> {
    let mut info = GenRiscvInfo::new();
    prog.generate(writer, &mut info, &prog)?;
    Ok(())
}