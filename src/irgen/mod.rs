use super::types::ast::*;
use koopa::ir::*;
use koopa::ir::builder_traits::*;

trait GenerateIR {
    fn generate(&self, info: &AstGlobalInfo, func: &mut FunctionData, bb: &mut BasicBlock) -> Value;
}

impl Into<BinaryOp> for Op {
    fn into(self) -> BinaryOp {
        match self {
            Self::Add => BinaryOp::Add, // 0 + x <=> +x
            Self::And => BinaryOp::And,
            Self::Div => BinaryOp::Div,
            Self::Eq => BinaryOp::Eq,
            Self::Ge => BinaryOp::Ge,
            Self::Gt => BinaryOp::Gt,
            Self::Le => BinaryOp::Le,
            Self::Lt => BinaryOp::Lt,
            Self::Mod => BinaryOp::Mod,
            Self::Mul => BinaryOp::Mul,
            Self::Ne => BinaryOp::NotEq,
            Self::Not => BinaryOp::Eq, // 0 == x <=> !x
            Self::Or => BinaryOp::Or,
            Self::Sub => BinaryOp::Sub,// 0 - x <=> -x
        }
    }
}

impl GenerateIR for Expression {
    fn generate(&self, info: &AstGlobalInfo, func: &mut FunctionData, bb: &mut BasicBlock) -> Value {
        self.binary_exp.generate(info, func, bb)
    }
}

impl GenerateIR for BinaryExp {
    fn generate(&self, info: &AstGlobalInfo, func: &mut FunctionData, bb: &mut BasicBlock) -> Value {
        match self {
            Self::T(lhs, op, rhs) => {
                match op {
                    Op::And | Op::Or => {
                        let lhs = lhs.generate(info, func, bb);
                        let tmp = func.dfg_mut().new_value().alloc(Type::get_i32());
                        func.layout_mut().bb_mut(bb.clone()).insts_mut().extend([tmp]);
                        let true_bb = func.dfg_mut().new_bb().basic_block(None);
                        let false_bb = func.dfg_mut().new_bb().basic_block(None);
                        let next_bb = func.dfg_mut().new_bb().basic_block(None);
                        func.layout_mut().bbs_mut().extend([true_bb, false_bb, next_bb]);
                        if *op == Op::And {
                            let br1 = func.dfg_mut().new_value().branch(lhs, true_bb, false_bb);
                            func.layout_mut().bb_mut(bb.clone()).insts_mut().extend([br1]);
                            *bb = true_bb;
                            let rhs = rhs.generate(info, func, bb);
                            let zero = func.dfg_mut().new_value().integer(0);
                            let cmp = func.dfg_mut().new_value().binary(BinaryOp::NotEq, zero, rhs);
                            let store = func.dfg_mut().new_value().store(cmp, tmp);
                            let jmp = func.dfg_mut().new_value().jump(next_bb);
                            func.layout_mut().bb_mut(bb.clone()).insts_mut().extend([cmp, store, jmp]);
                            *bb = false_bb;
                            let zero = func.dfg_mut().new_value().integer(0);
                            let store = func.dfg_mut().new_value().store(zero, tmp);
                            let jmp = func.dfg_mut().new_value().jump(next_bb);
                            func.layout_mut().bb_mut(bb.clone()).insts_mut().extend([store, jmp]);
                            *bb = next_bb;
                        } else {
                            let br1 = func.dfg_mut().new_value().branch(lhs, true_bb, false_bb);
                            func.layout_mut().bb_mut(bb.clone()).insts_mut().extend([br1]);
                            *bb = true_bb;
                            let one = func.dfg_mut().new_value().integer(1);
                            let store = func.dfg_mut().new_value().store(one, tmp);
                            let jmp = func.dfg_mut().new_value().jump(next_bb);
                            func.layout_mut().bb_mut(bb.clone()).insts_mut().extend([store, jmp]);
                            *bb = false_bb;
                            let rhs = rhs.generate(info, func, bb);
                            let zero = func.dfg_mut().new_value().integer(0);
                            let cmp = func.dfg_mut().new_value().binary(BinaryOp::NotEq, zero, rhs);
                            let store = func.dfg_mut().new_value().store(cmp, tmp);
                            let jmp = func.dfg_mut().new_value().jump(next_bb);
                            func.layout_mut().bb_mut(bb.clone()).insts_mut().extend([cmp, store, jmp]);
                            *bb = next_bb;
                        }
                        let result = func.dfg_mut().new_value().load(tmp);
                        func.layout_mut().bb_mut(bb.clone()).insts_mut().extend([result]);
                        result
                    }
                    _ => {
                        let lhs = lhs.generate(info, func, bb);
                        let rhs = rhs.generate(info, func, bb);
                        let result = func.dfg_mut().new_value().binary((*op).into(), lhs, rhs);
                        func.layout_mut().bb_mut(bb.clone()).insts_mut().extend([result]);
                        result
                    }
                }
            }
            Self::U(u) => u.generate(info, func, bb)
        }
    }
}

impl GenerateIR for UnaryExp {
    fn generate(&self, info: &AstGlobalInfo, func: &mut FunctionData, bb: &mut BasicBlock) -> Value {
        match self {
            Self::B(op, rhs) => {
                let lhs = func.dfg_mut().new_value().integer(0);
                let rhs = rhs.generate(info, func, bb);
                let result = func.dfg_mut().new_value().binary((*op).into(), lhs, rhs);
                func.layout_mut().bb_mut(bb.clone()).insts_mut().extend([result]);
                result
            }
            Self::C(callee, params) => {
                if let Some(callee) = info.find_func_symbol(callee) {
                    // TODO: calling type check
                    let call_values = params.iter().map(|param| {
                        param.generate(info, func, bb)
                    }).collect();
                    let call = func.dfg_mut().new_value().call(callee.clone(), call_values);
                    func.layout_mut().bb_mut(bb.clone()).insts_mut().extend([call]);
                    call
                } else {
                    panic!("Callee {} not found", callee);
                }
            }
            Self::I(arr, indexes) => {
                let is_ptr = match info.find_symbol(arr) {
                    Some((VarType::Var(SysyType::PTR(_)), _)) => true,
                    _ => false,
                };
                let is_arr = match info.find_symbol(arr) {
                    Some((VarType::Var(SysyType::ARR(_, _)), _)) => true,
                    _ => false,
                };
                assert!(is_ptr || is_arr);
                if let Some(arr) = info.find_var_symbol(arr) {
                    let mut arr = arr.clone();
                    let mut first_iter = true;
                    for index in indexes {
                        let index = index.generate(info, func, bb);
                        let value = if is_ptr && first_iter {
                            let value = func.dfg_mut().new_value().load(arr);
                            func.layout_mut().bb_mut(*bb).insts_mut().extend([value]);
                            func.dfg_mut().new_value().get_ptr(value, index)
                        } else {
                            func.dfg_mut().new_value().get_elem_ptr(arr, index)
                        };
                        first_iter = false;
                        func.layout_mut().bb_mut(*bb).insts_mut().extend([value]);
                        arr = value;
                    }
                    let value = match func.dfg().value(arr).ty().kind() {
                        TypeKind::Pointer(ty) => {
                            match ty.kind() {
                                TypeKind::Array(_, _) => {
                                    let zero = func.dfg_mut().new_value().integer(0);
                                    func.dfg_mut().new_value().get_elem_ptr(arr, zero)
                                },
                                TypeKind::Int32 => {
                                    func.dfg_mut().new_value().load(arr)
                                },
                                _ => todo!()
                            }
                        },
                        _ => panic!("type mismatch"),
                    };
                    func.layout_mut().bb_mut(*bb).insts_mut().extend([value]);
                    value
                } else {
                    panic!("Array {} not found", arr);
                }
            }
            Self::U(u) => u.generate(info, func, bb)
        }
    }
}

impl GenerateIR for PrimaryExp {
    fn generate(&self, info: &AstGlobalInfo, func: &mut FunctionData, bb: &mut BasicBlock) -> Value {
        match self {
            Self::Exp(exp) => exp.generate(info, func, bb),
            Self::Num(n) => func.dfg_mut().new_value().integer(*n),
            Self::Ident(id) => {
                let v = info.find_var_symbol(id).unwrap().clone();
                if let Some((VarType::Var(ty), _)) = info.find_symbol(id).clone() {
                    let result = match ty {
                        SysyType::I32 | SysyType::PTR(_) => {
                            func.dfg_mut().new_value().load(v)
                        },
                        SysyType::ARR(_, _) => {
                            let zero = func.dfg_mut().new_value().integer(0);
                            func.dfg_mut().new_value().get_elem_ptr(v, zero)
                        },
                        _ => todo!()
                    };
                    func.layout_mut().bb_mut(bb.clone()).insts_mut().extend([result]);
                    result
                } else {
                    panic!()
                }
            }
        }
    }
}

impl ConstInitVal {
    fn generate(&self, info: &AstGlobalInfo, func: &mut FunctionData, ty: SysyType) -> Value {
        fn ty_to_len(ty: SysyType) -> Vec<usize> {
            match ty {
                SysyType::ARR(ty, len) => {
                    let mut result = ty_to_len(*ty);
                    result.push(len);
                    result
                },
                _ => vec![],
            }
        }
        let lens = ty_to_len(ty);
        let size = lens.iter().fold(1, |x, y| x * y);
        let mut init_list = vec![None; size];
        fn process(init_list: &mut [Option<Value>], list: &Vec<ConstInitVal>, lens: &[usize], info: &AstGlobalInfo, func: &mut FunctionData) {
            let mut cnt = 0;
            let size = lens.iter().fold(1, |x, y| x * y);
            for i in list {
                match i {
                    ConstInitVal::Exp(exp) => {
                        let value = func.dfg_mut().new_value().integer(exp.eval(info));
                        init_list[cnt] = Some(value);
                        cnt += 1;
                    },
                    ConstInitVal::List(lst) => {
                        if cnt % lens[0] != 0 {
                            panic!("Initlist misaligned");
                        }
                        let mut lvl = 0;
                        let mut cnt2 = cnt;
                        for i in lens {
                            if cnt2 % i != 0 {
                                break;
                            }
                            lvl += 1;
                            cnt2 /= i;
                        }
                        if lvl == lens.len() {
                            lvl = lens.len() - 1;
                        }
                        let next_len = lens[0..lvl].into_iter().fold(1, |x, y| x * y);
                        process(&mut init_list[cnt..cnt + next_len], lst, &lens[0..lvl], info, func);
                        cnt += next_len;
                    }
                }
            }
            for i in cnt..size {
                init_list[i] = Some(func.dfg_mut().new_value().integer(0));
            }
        }
        match self {
            Self::Exp(exp) => func.dfg_mut().new_value().integer(exp.eval(info)),
            Self::List(lst) => {
                process(&mut init_list, lst, &lens, info, func);
                let init_list: Vec<Value> = init_list.into_iter().map(|x| x.unwrap()).collect();
                fn aggregate(init_list: &[Value], lens: &[usize], func: &mut FunctionData) -> Value {
                    let mut elems = Vec::new() ;
                    if lens.len() == 1 {
                        elems = init_list.to_vec();
                    } else {
                        let step = lens[..lens.len() - 1].into_iter().fold(1, |x, y| x * y);
                        let mut i = 0;
                        let size = lens.into_iter().fold(1, |x, y| x * y);
                        while i < size {
                            elems.push(aggregate(&init_list[i..i + step], &lens[..lens.len() - 1], func));
                            i += step;
                        }
                    }
                    func.dfg_mut().new_value().aggregate(elems)
                }
                aggregate(&init_list, &lens, func)
            }
        }
    }
    fn global_generate(&self, info: &AstGlobalInfo, prog: &mut Program, ty: SysyType) -> Value {
        fn ty_to_len(ty: SysyType) -> Vec<usize> {
            match ty {
                SysyType::ARR(ty, len) => {
                    let mut result = ty_to_len(*ty);
                    result.push(len);
                    result
                },
                _ => vec![],
            }
        }
        let lens = ty_to_len(ty);
        let size = lens.iter().fold(1, |x, y| x * y);
        let mut init_list = vec![None; size];
        fn process(init_list: &mut [Option<Value>], list: &Vec<ConstInitVal>, lens: &[usize], info: &AstGlobalInfo, prog: &mut Program) {
            let mut cnt = 0;
            let size = lens.into_iter().fold(1, |x, y| x * y);
            for i in list {
                match i {
                    ConstInitVal::Exp(exp) => {
                        let value = prog.new_value().integer(exp.eval(info));
                        init_list[cnt] = Some(value);
                        cnt += 1;
                    },
                    ConstInitVal::List(lst) => {
                        if cnt % lens[0] != 0 {
                            panic!("Initlist misaligned");
                        }
                        let mut lvl = 0;
                        let mut cnt2 = cnt;
                        for i in lens {
                            if cnt2 % i != 0 {
                                break;
                            }
                            lvl += 1;
                            cnt2 /= i;
                        }
                        if lvl == lens.len() {
                            lvl = lens.len() - 1;
                        }
                        let next_len = lens[0..lvl].into_iter().fold(1, |x, y| x * y);
                        process(&mut init_list[cnt..cnt + next_len], lst, &lens[0..lvl], info, prog);
                        cnt += next_len;
                    }
                }
            }
            for i in cnt..size {
                init_list[i] = Some(prog.new_value().integer(0));
            }
        }
        match self {
            Self::Exp(exp) => prog.new_value().integer(exp.eval(info)),
            Self::List(lst) => {
                process(&mut init_list, lst, &lens, info, prog);
                let init_list: Vec<Value> = init_list.into_iter().map(|x| x.unwrap()).collect();
                fn aggregate(init_list: &[Value], lens: &[usize], prog: &mut Program) -> Value {
                    let elems = if lens.len() == 1 {
                        init_list.to_vec()
                    } else {
                        let step = lens[..lens.len() - 1].into_iter().fold(1, |x, y| x * y);
                        let mut elems = vec![];
                        let mut i = 0;
                        let size = lens.into_iter().fold(1, |x, y| x * y);
                        while i < size {
                            elems.push(aggregate(&init_list[i..i + step], &lens[..lens.len() - 1], prog));
                            i += step;
                        }
                        elems
                    };
                    prog.new_value().aggregate(elems)
                }
                aggregate(&init_list, &lens, prog)
            }
        }
    }
}

impl InitVal {
    fn generate(&self, info: &AstGlobalInfo, func: &mut FunctionData, bb: &mut BasicBlock, ty: SysyType) -> Value {
        fn ty_to_len(ty: SysyType) -> Vec<usize> {
            match ty {
                SysyType::ARR(ty, len) => {
                    let mut result = ty_to_len(*ty);
                    result.push(len);
                    result
                },
                _ => vec![],
            }
        }
        let lens = ty_to_len(ty);
        let size = lens.iter().fold(1, |x, y| x * y);
        let mut init_list = vec![None; size];
        fn process(init_list: &mut [Option<Value>], list: &Vec<InitVal>, lens: &[usize], info: &AstGlobalInfo, func: &mut FunctionData, bb: &mut BasicBlock) {
            let mut cnt = 0;
            let size = lens.iter().fold(1, |x, y| x * y);
            for i in list {
                match i {
                    InitVal::Exp(exp) => {
                        let value = exp.generate(info, func, bb);
                        init_list[cnt] = Some(value);
                        cnt += 1;
                    },
                    InitVal::List(lst) => {
                        if cnt % lens[0] != 0 {
                            panic!("Initlist misaligned");
                        }
                        let mut lvl = 0;
                        let mut cnt2 = cnt;
                        for i in lens {
                            if cnt2 % i != 0 {
                                break;
                            }
                            lvl += 1;
                            cnt2 /= i;
                        }
                        if lvl == lens.len() {
                            lvl = lens.len() - 1;
                        }
                        let next_len = lens[0..lvl].iter().fold(1, |x, y| x * y);
                        process(&mut init_list[cnt..cnt + next_len], lst, &lens[0..lvl], info, func, bb);
                        cnt += next_len;
                    }
                }
            }
            for i in cnt..size {
                init_list[i] = Some(func.dfg_mut().new_value().integer(0));
            }
        }
        match self {
            Self::Exp(exp) => exp.generate(info, func, bb),
            Self::List(lst) => {
                process(&mut init_list, lst, &lens, info, func, bb);
                let init_list: Vec<Value> = init_list.into_iter().map(|x| x.unwrap()).collect();
                fn aggregate(init_list: &[Value], lens: &[usize], func: &mut FunctionData) -> Value {
                    let elems = if lens.len() == 1 {
                        init_list.to_vec()
                    } else {
                        let step = lens[..lens.len() - 1].into_iter().fold(1, |x, y| x * y);
                        let mut elems = vec![];
                        let mut i = 0;
                        let size = lens.into_iter().fold(1, |x, y| x * y);
                        while i < size {
                            elems.push(aggregate(&init_list[i..i + step], &lens[..lens.len() - 1], func));
                            i += step;
                        }
                        elems
                    };
                    func.dfg_mut().new_value().aggregate(elems)
                }
                aggregate(&init_list, &lens, func)
            }
        }
    }
}

fn optimize_block(input: &mut Block, outer_info: Option<* mut AstGlobalInfo>) {
    let mut info = AstGlobalInfo::new();
    info.set_outer(outer_info);
    for item in &mut input.items {
        match item {
            BlockItem::D(decl) => {
                match decl {
                    Decl::C(cdecl) => {
                        let mut doit = |x: &mut ConstDef| {
                            if info.has_symbol_conflict(x.ident.clone()) {
                                panic!("Redefined ident {}", x.ident);
                            }
                            x.init_val.optimize(&info);
                            if x.indexes.is_empty() {
                                match &x.init_val {
                                    ConstInitVal::Exp(exp) => {info.add_symbol(x.ident.clone(), (VarType::Const(cdecl.decl_type.clone()), exp.eval(&info)));},
                                    ConstInitVal::List(_) => panic!("InitList mismatch"),
                                }
                            } else {
                                let mut ty = cdecl.decl_type.clone();
                                for index in x.indexes.iter().rev() {
                                    ty = SysyType::ARR(Box::new(ty), index.eval(&info) as usize);
                                }
                                info.add_symbol(x.ident.clone(), (VarType::Var(ty), 0));
                            }
                        };
                        doit(&mut cdecl.def1);
                        let _: Vec<()> = cdecl.def_remain.iter_mut().map(doit).collect();
                    }
                    Decl::V(vdecl) => {
                        let mut doit = |x: &mut VarDef| {
                            if info.has_symbol_conflict(x.ident.clone()) {
                                panic!("Redefined ident {}", x.ident);
                            }
                            let mut ty = vdecl.decl_type.clone();
                            for index in x.indexes.iter_mut().rev() {
                                index.expression.optimize(&info);
                                ty = SysyType::ARR(Box::new(ty), index.eval(&info) as usize);
                            }
                            let _ = info.add_symbol(x.ident.clone(), (VarType::Var(ty), 0));
                            if let Some(init_val) = &mut x.init_val {
                                init_val.optimize(&info);
                            }
                        };
                        doit(&mut vdecl.def1);
                        let _: Vec<()> = vdecl.def_remain.iter_mut().map(doit).collect();
                    }
                }
            }
            BlockItem::S(stmt) => {
                match stmt {
                    Stmt::ASN{lval, exp} => {
                        for index in &mut lval.indexes {
                            index.optimize(&info);
                        }
                        exp.optimize(&info);
                    },
                    Stmt::RET(exp) => {
                        if let Some(exp) = exp {
                            exp.optimize(&info);
                        }
                    },
                    Stmt::EXP(Some(exp)) => {
                        exp.optimize(&info);
                    },
                    Stmt::EXP(None) => (),
                    Stmt::BLK(blk) => {
                        optimize_block(blk, Some(&mut info));
                    },
                    Stmt::CND { cond, stmt, else_stmt } => {
                        // cond opt
                        cond.optimize(&info);
                        // if opt
                        let mut blk = match &**stmt {
                            Stmt::BLK(blk) => (**blk).clone(),
                            other => Block::new_single_stmt(other.clone()),
                        };
                        optimize_block(&mut blk, Some(&mut info));
                        **stmt = Stmt::BLK(Box::new(blk));
                        // else opt
                        if let Some(stmt) = else_stmt {
                            let mut blk = match &**stmt {
                                Stmt::BLK(blk) => (**blk).clone(),
                                other => Block::new_single_stmt(other.clone()),
                            };
                            optimize_block(&mut blk, Some(&mut info));
                            **stmt = Stmt::BLK(Box::new(blk));
                        }
                    },
                    Stmt::WHILE { cond, stmt } => {
                        // cond opt
                        cond.optimize(&info);
                        // stmt opt
                        let mut blk = match &**stmt {
                            Stmt::BLK(blk) => (**blk).clone(),
                            other => Block::new_single_stmt(other.clone()),
                        };
                        optimize_block(&mut blk, Some(&mut info));
                        **stmt = Stmt::BLK(Box::new(blk));
                    },
                    Stmt::BREAK => (),
                    Stmt::CONT => (),
                }
            }
        }
    }
}

fn process_block(func_data: &mut FunctionData, block: &Block, bb: &BasicBlock, outer_info: Option<* mut AstGlobalInfo>) {
    let mut info = AstGlobalInfo::new();
    info.set_outer(outer_info);
    let mut bb = *bb;

    for item in &block.items {
        if info.returned {
            break;
        }
        match item {
            BlockItem::D(decl) => {
                match decl {
                    Decl::V(vdecl) => {
                        let mut doit = |x: &VarDef| {
                            let mut ty = vdecl.decl_type.clone();
                            for index in x.indexes.iter().rev() {
                                ty = SysyType::ARR(Box::new(ty), index.eval(&info) as usize);
                            }
                            let allocvar = func_data.dfg_mut().new_value().alloc(ty.get_koopa_type());
                            assert!(info.add_var_symbol(x.ident.clone(), allocvar).is_none());
                            info.add_symbol(x.ident.clone(), (VarType::Var(ty.clone()), 0));
                            func_data.dfg_mut().set_value_name(allocvar, format!("@{}", x.ident).into());
                            func_data.layout_mut().bb_mut(bb).insts_mut().extend([allocvar]);
                            if let Some(init_val) = &x.init_val {
                                let old_bb = bb;
                                let result = init_val.generate(&mut info, func_data, &mut bb, ty.clone());
                                if bb != old_bb {
                                    info.set_outer_should_update_bb(bb);
                                }
                                let inst = func_data.dfg_mut().new_value().store(result, allocvar);
                                func_data.layout_mut().bb_mut(bb).insts_mut().extend([inst]);
                            }
                        };
                        doit(&vdecl.def1);
                        let _: Vec<()> = vdecl.def_remain.iter().map(doit).collect();
                    }
                    Decl::C(cdecl) => {
                        let mut doit = |x: &ConstDef| {
                            if x.indexes.len() == 0 {
                                return;
                            }
                            let mut ty = cdecl.decl_type.clone();
                            for index in x.indexes.iter().rev() {
                                ty = SysyType::ARR(Box::new(ty), index.eval(&info) as usize);
                            }
                            let allocvar = func_data.dfg_mut().new_value().alloc(ty.get_koopa_type());
                            assert!(info.add_var_symbol(x.ident.clone(), allocvar).is_none());
                            info.add_symbol(x.ident.clone(), (VarType::Var(ty.clone()), 0));
                            func_data.dfg_mut().set_value_name(allocvar, format!("@{}", x.ident).into());
                            func_data.layout_mut().bb_mut(bb).insts_mut().extend([allocvar]);
                            let result = x.init_val.generate(&mut info, func_data, ty.clone());
                            let inst = func_data.dfg_mut().new_value().store(result, allocvar);
                            func_data.layout_mut().bb_mut(bb).insts_mut().extend([inst]);
                        };
                        doit(&cdecl.def1);
                        let _: Vec<()> = cdecl.def_remain.iter().map(doit).collect();
                    }
                }
            }
            BlockItem::S(stmt) => {
                match stmt {
                    Stmt::RET(exp) => {
                        let result = if let Some(exp) = exp {
                            let old_bb = bb;
                            let result = Some(exp.generate(&mut info, func_data, &mut bb));
                            if old_bb != bb {
                                info.set_outer_should_update_bb(bb);
                            }
                            result
                        } else {
                            None
                        };
                        let inst = func_data.dfg_mut().new_value().ret(result);
                        func_data.layout_mut().bb_mut(bb).insts_mut().extend([inst]);
                        info.set_returned();
                        return;
                    },
                    Stmt::ASN{ lval, exp } => {
                        let mut dest = info.find_var_symbol(&lval.ident).unwrap().clone();
                        let (ty, _) = info.find_symbol(&lval.ident).unwrap().clone();
                        let old_bb = bb;
                        let mut first_iter = true;
                        for index in &lval.indexes {
                            let value = index.generate(&info, func_data, &mut bb);
                            let inst = if first_iter {
                                match &ty {
                                    VarType::Var(SysyType::ARR(_, _)) => {
                                        func_data.dfg_mut().new_value().get_elem_ptr(dest, value)
                                    },
                                    VarType::Var(SysyType::PTR(_)) => {
                                        let src = func_data.dfg_mut().new_value().load(dest);
                                        func_data.layout_mut().bb_mut(bb).insts_mut().extend([src]);
                                        func_data.dfg_mut().new_value().get_ptr(src, value)
                                    },
                                    _ => todo!()
                                }
                            } else {
                                func_data.dfg_mut().new_value().get_elem_ptr(dest, value)
                            };
                            func_data.layout_mut().bb_mut(bb).insts_mut().extend([inst]);
                            dest = inst;
                            first_iter = false;
                        }
                        let result = exp.generate(&mut info, func_data, &mut bb);
                        if old_bb != bb {
                            info.set_outer_should_update_bb(bb);
                        }
                        let inst = func_data.dfg_mut().new_value().store(result, dest);
                        func_data.layout_mut().bb_mut(bb).insts_mut().extend([inst]);
                    },
                    Stmt::EXP(Some(exp)) => {
                        let old_bb = bb;
                        exp.generate(&mut info, func_data, &mut bb);
                        if old_bb != bb {
                            info.set_outer_should_update_bb(bb);
                        }
                    },
                    Stmt::EXP(None) => (),
                    Stmt::BLK(blk) => {
                        process_block(func_data, blk, &bb, Some(&mut info));
                        // if inner block updates bb, we should update and propagate
                        if let Some(new_bb) = info.should_update_bb {
                            info.should_update_bb = None;
                            info.set_outer_should_update_bb(new_bb);
                            bb = new_bb;
                        }
                    },
                    Stmt::CND { cond, stmt, else_stmt } => {
                        let true_bb = func_data.dfg_mut().new_bb().basic_block(None);
                        let false_bb = func_data.dfg_mut().new_bb().basic_block(None);
                        let next_bb = if let Some(_) = else_stmt {
                            func_data.dfg_mut().new_bb().basic_block(None)
                        }
                        else {
                            false_bb
                        };
                        func_data.layout_mut().bbs_mut().extend([true_bb, false_bb, next_bb]);
                        let cond = cond.generate(&mut info, func_data, &mut bb);
                        let inst = func_data.dfg_mut().new_value().branch(cond, true_bb, false_bb);
                        func_data.layout_mut().bb_mut(bb).insts_mut().extend([inst]);
                        info.ret_target_bb = Some(next_bb);
                        match &**stmt {
                            Stmt::BLK(blk) => {
                                process_block(func_data, &blk, &true_bb, Some(&mut info));
                            },
                            _ => {
                                panic!("Not optimized?");
                            }
                        }
                        if let Some(else_stmt) = else_stmt {
                            match &**else_stmt {
                                Stmt::BLK(blk) => {
                                    process_block(func_data, &blk, &false_bb, Some(&mut info));
                                },
                                _ => {
                                    panic!("Not optimized?");
                                }
                            }
                        }
                        info.ret_target_bb = None;
                        bb = next_bb;
                        info.set_outer_should_update_bb(next_bb);
                        // We should update bb, but not related to inner block
                        info.should_update_bb = None;
                    },
                    Stmt::WHILE { cond, stmt } => {
                        let mut cond_bb = func_data.dfg_mut().new_bb().basic_block(None);
                        let body_bb = func_data.dfg_mut().new_bb().basic_block(None);
                        let next_bb = func_data.dfg_mut().new_bb().basic_block(None);
                        func_data.layout_mut().bbs_mut().extend([cond_bb, body_bb, next_bb]);
                        // end current block
                        let inst = func_data.dfg_mut().new_value().jump(cond_bb);
                        func_data.layout_mut().bb_mut(bb).insts_mut().extend([inst]);
                        // generate condition
                        let old_cond_bb = cond_bb;
                        let cond = cond.generate(&mut info, func_data, &mut cond_bb);
                        let inst = func_data.dfg_mut().new_value().branch(cond, body_bb, next_bb);
                        func_data.layout_mut().bb_mut(cond_bb).insts_mut().extend([inst]);
                        let cond_bb = old_cond_bb;
                        // generate body
                        info.ret_target_bb = Some(cond_bb);
                        info.loop_break_target = Some(next_bb);
                        info.loop_cont_target = Some(cond_bb);
                        match &**stmt {
                            Stmt::BLK(blk) => {
                                process_block(func_data, &blk, &body_bb, Some(&mut info));
                            },
                            _ => panic!("Not optimized?"),
                        }
                        info.ret_target_bb = None;
                        info.loop_break_target = None;
                        info.loop_cont_target = None;
                        bb = next_bb;
                        info.set_outer_should_update_bb(next_bb);
                        info.should_update_bb = None;
                        
                    },
                    Stmt::BREAK => {
                        let target = info.get_loop_break_target().expect("break not in a loop?");
                        let inst = func_data.dfg_mut().new_value().jump(target);
                        func_data.layout_mut().bb_mut(bb).insts_mut().extend([inst]);
                        info.set_returned();
                        return;
                    },
                    Stmt::CONT => {
                        let target = info.get_loop_cont_target().expect("continue not in a loop?");
                        let inst = func_data.dfg_mut().new_value().jump(target);
                        func_data.layout_mut().bb_mut(bb).insts_mut().extend([inst]);
                        info.set_returned();
                        return;
                    },
                }
            }
        }
    }
    if !info.returned {
        if let Some(target_bb) = info.get_ret_target_bb() {
            let branch = func_data.dfg_mut().new_value().jump(target_bb);
            func_data.layout_mut().bb_mut(bb).insts_mut().extend([branch]);
        }
        else if info.is_func_info() {
            let retval = match func_data.ty().kind() {
                TypeKind::Function(_, ty) => {
                    if *ty == Type::get_i32() {
                        Some(func_data.dfg_mut().new_value().integer(114514))
                    } else {
                        None
                    }
                },
                _ => None,
            };
            let ret = func_data.dfg_mut().new_value().ret(retval);
            func_data.layout_mut().bb_mut(bb).insts_mut().extend([ret]);
        }
    }
}

fn process_func(prog: &mut Program, ret_type: &SysyType, func_params: &Vec<FuncParam>, ident: &String, block: &mut Block, global_info: Option<* mut AstGlobalInfo>) -> Function {
    let mut info = AstGlobalInfo::new();
    info.set_outer(global_info);

    let func_data = FunctionData::new(format!("@{}", ident), func_params.iter().map(|param| param.ty(&info).get_koopa_type()).collect(), ret_type.get_koopa_type());
    let func = prog.new_func(func_data);
    let func_data = prog.func_mut(func);
    let entry = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
    func_data.layout_mut().bbs_mut().extend([entry]);
    if let Some(ptr) = global_info {
        unsafe {
            (*ptr).add_func_symbol(ident.clone(), func);
        }
    }

    // Process params to local variables
    let mut local_params = Vec::new();
    let params: Vec<Value> = func_data.params().iter().map(|x| x.clone()).collect();
    for param in params {
        let alloc_type = func_data.dfg().value(param.clone()).ty().clone();
        let alloc = func_data.dfg_mut().new_value().alloc(alloc_type);
        let assign = func_data.dfg_mut().new_value().store(param.clone(), alloc);
        func_data.layout_mut().bb_mut(entry).insts_mut().extend([alloc, assign]);
        local_params.push(alloc);
    }

    // Add params to local symbol table
    let mut iter = local_params.iter();
    for param in func_params {
        if let Some(ident) = &param.ident {
            info.add_symbol(ident.clone(), (VarType::Var(param.ty(&info)), 0));
            info.add_var_symbol(ident.clone(), iter.next().unwrap().clone());
        }
    }

    optimize_block(block, Some(&mut info)); // Symbol check and const propagation
    let block = &*block;
    process_block(func_data, &block, &entry, Some(&mut info));

    func
}

pub fn ast_to_koopa(mut input: CompUnit) -> Program {
    let mut prog = Program::new();
    let mut global_info = AstGlobalInfo::new();
    
    // Add runtime lib symbols

    // int getint()
    let func = prog.new_func(FunctionData::new_decl("@getint".into(), vec![], Type::get_i32()));
    global_info.add_func_symbol("getint".into(), func);
    // int getch()
    let func = prog.new_func(FunctionData::new_decl("@getch".into(), vec![], Type::get_i32()));
    global_info.add_func_symbol("getch".into(), func);
    // int getarray(int *)
    let func = prog.new_func(FunctionData::new_decl("@getarray".into(), vec![Type::get_pointer(Type::get_i32())], Type::get_i32()));
    global_info.add_func_symbol("getarray".into(), func);
    // void putint(int)
    let func = prog.new_func(FunctionData::new_decl("@putint".into(), vec![Type::get_i32()], Type::get_unit()));
    global_info.add_func_symbol("putint".into(), func);
    // void putch(int)
    let func = prog.new_func(FunctionData::new_decl("@putch".into(), vec![Type::get_i32()], Type::get_unit()));
    global_info.add_func_symbol("putch".into(), func);
    // void putarray(int, int *)
    let func = prog.new_func(FunctionData::new_decl("@putarray".into(), vec![Type::get_i32(), Type::get_pointer(Type::get_i32())], Type::get_unit()));
    global_info.add_func_symbol("putarray".into(), func);
    // void starttime()
    let func = prog.new_func(FunctionData::new_decl("@starttime".into(), vec![], Type::get_unit()));
    global_info.add_func_symbol("starttime".into(), func);
    // void stoptime()
    let func = prog.new_func(FunctionData::new_decl("@stoptime".into(), vec![], Type::get_unit()));
    global_info.add_func_symbol("stoptime".into(), func);

    // process global declarations and definitions:
    for global_decl in &mut input.global_decls {
        match global_decl {
            GlobalDecl::FuncDecl { ret_type, ident, func_params, block } => {
                if global_info.has_symbol_conflict(ident.clone()) {
                    // TODO: Multiple decl, single def
                    panic!("global ident {} conflict", ident);
                }
                match block.as_mut() {
                    Some(blk) => {
                        process_func(&mut prog, &ret_type, &func_params, &ident, blk, Some(&mut global_info));
                    },
                    None => {
                        let func_data = FunctionData::new_decl(format!("@{}", ident).into(), func_params.iter().map(|x| x.ty(&global_info).get_koopa_type()).collect(), ret_type.get_koopa_type());
                        let func = prog.new_func(func_data);
                        global_info.add_func_symbol(ident.clone(), func);
                    },
                }
            },
            GlobalDecl::OtherDecl(decl) => {
                match decl {
                    Decl::C(cdecl) => {
                        let mut doit = |x: &mut ConstDef| {
                            if global_info.has_symbol_conflict(x.ident.clone()) {
                                panic!("global ident {} conflict", x.ident);
                            }
                            x.init_val.optimize(&global_info);
                            if x.indexes.is_empty() {
                                match &x.init_val {
                                    ConstInitVal::Exp(exp) => {global_info.add_symbol(x.ident.clone(), (VarType::Const(cdecl.decl_type.clone()), exp.eval(&global_info)));},
                                    ConstInitVal::List(_) => panic!("InitList mismatch"),
                                }
                            } else {
                                let mut ty = cdecl.decl_type.clone();
                                for i in x.indexes.iter().rev() {
                                    ty = SysyType::ARR(Box::new(ty), i.eval(&global_info) as usize);
                                }
                                global_info.add_symbol(x.ident.clone(), (VarType::Var(ty.clone()), 0));
                                let init = x.init_val.global_generate(&global_info, &mut prog, ty.clone());
                                let value = prog.new_value().global_alloc(init);
                                prog.set_value_name(value, Some(format!("@{}", x.ident)));
                                global_info.add_var_symbol(x.ident.clone(), value);
                            }
                        };
                        doit(&mut cdecl.def1);
                        let _: Vec<()> = cdecl.def_remain.iter_mut().map(doit).collect();
                    }
                    Decl::V(vdecl) => {
                        let mut doit = |x: &mut VarDef| {
                            if global_info.has_symbol_conflict(x.ident.clone()) {
                                panic!("global ident {} conflict", x.ident);
                            }
                            let mut ty = vdecl.decl_type.clone();
                            for i in x.indexes.iter().rev() {
                                ty = SysyType::ARR(Box::new(ty), i.eval(&global_info) as usize);
                            }
                            let init = if let Some(exp) = &mut x.init_val {
                                exp.optimize(&global_info);
                                exp.to_const().global_generate(&global_info, &mut prog, ty.clone())
                            } else {
                                prog.new_value().zero_init(ty.get_koopa_type())
                            };
                            let _ = global_info.add_symbol(x.ident.clone(), (VarType::Var(ty), 0));
                            if let Some(init_val) = &mut x.init_val {
                                init_val.optimize(&global_info);
                            }
                            let value = prog.new_value().global_alloc(init);
                            prog.set_value_name(value, Some(format!("@{}", x.ident)));
                            let _ = global_info.add_var_symbol(x.ident.clone(), value);
                        };
                        doit(&mut vdecl.def1);
                        let _: Vec<()> = vdecl.def_remain.iter_mut().map(doit).collect();
                    }
                }
            }
        }
    }
    // end for

    prog
}
