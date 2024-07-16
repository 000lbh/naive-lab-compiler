use std::collections::HashMap;

use koopa::ir::types::Type;
use koopa::ir::entities::{Value, Function};
use koopa::ir::BasicBlock;

pub trait AbstractAst {
    
}

pub trait AbstractExp {
    fn eval(&self, info: &AstGlobalInfo) -> Option<i32>;
    fn optimize(&mut self, info: &AstGlobalInfo) -> Option<i32>;
    fn new_single_value(value: i32) -> Self;
}

pub trait AbstractInitVal {
    fn optimize(&mut self, info: &AstGlobalInfo);
}

#[derive(Debug, Clone)]
pub enum VarType {
    Const(SysyType),
    Var(SysyType),
    Func(SysyType),
}

#[derive(Debug)]
pub struct AstGlobalInfo {
    symbol_table: HashMap<String, (VarType, i32)>,
    var_symbol_table: HashMap<String, Value>,
    func_symbol_table: HashMap<String, Function>,
    pub ret_target_bb: Option<BasicBlock>,
    pub should_update_bb: Option<BasicBlock>,
    pub loop_break_target: Option<BasicBlock>,
    pub loop_cont_target: Option<BasicBlock>,
    pub returned: bool,
    pub counter: i32,
    pub outer: Option<* mut AstGlobalInfo>,
}

impl AstGlobalInfo {
    pub fn new() -> Self {
        AstGlobalInfo {
            symbol_table: HashMap::new(),
            var_symbol_table: HashMap::new(),
            func_symbol_table: HashMap::new(),
            ret_target_bb: None,
            should_update_bb: None,
            loop_break_target: None,
            loop_cont_target: None,
            counter: 0,
            returned: false,
            outer: None,
        }
    }
    pub fn set_outer(&mut self, outer: Option<* mut AstGlobalInfo>) {
        self.outer = outer;
    }
    pub fn find_symbol(&self, key: &String) -> Option<&(VarType, i32)> {
        let mut result = self.symbol_table.get(key);
        if result.is_none() {
            if let Some(ptr) = self.outer {
                unsafe {
                    result = (*ptr).find_symbol(key);
                }
            }
        }
        result
    }
    pub fn add_symbol(&mut self, key: String, value: (VarType, i32)) -> Option<(VarType, i32)> {
        self.symbol_table.insert(key, value)
    }
    pub fn find_var_symbol(&self, key: &String) -> Option<&Value> {
        let mut result = self.var_symbol_table.get(key);
        if result.is_none() {
            if let Some(ptr) = self.outer {
                unsafe {
                    result = (*ptr).find_var_symbol(key);
                }
            }
        }
        result
    }
    pub fn add_var_symbol(&mut self, key: String, value: Value) -> Option<Value> {
        self.var_symbol_table.insert(key, value)
    }
    pub fn find_func_symbol(&self, key: &String) -> Option<&Function> {
        let mut result = self.func_symbol_table.get(key);
        if result.is_none() {
            if let Some(ptr) = self.outer {
                unsafe {
                    result = (*ptr).find_func_symbol(key);
                }
            }
        }
        result
    }
    // Always add to the top.
    pub fn add_func_symbol(&mut self, key: String, value: Function) -> Option<Function> {
        if let Some(ptr) = self.outer {
            unsafe {
                (*ptr).add_func_symbol(key, value)
            }
        } else {
            self.func_symbol_table.insert(key, value)
        }
    }
    pub fn has_symbol_conflict(&self, key: String) -> bool {
        self.symbol_table.contains_key(&key) || 
        self.var_symbol_table.contains_key(&key) ||
        self.func_symbol_table.contains_key(&key)
    }
    pub fn get_ret_target_bb(&self) -> Option<BasicBlock> {
        if let Some(ptr) = self.outer {
            unsafe {
                (*ptr).ret_target_bb
            }
        }
        else {
            None
        }
    }
    pub fn get_loop_break_target(&self) -> Option<BasicBlock> {
        if let Some(ptr) = self.outer {
            unsafe {
                if (*ptr).loop_break_target.is_some() {
                    (*ptr).loop_break_target
                }
                else {
                    (*ptr).get_loop_break_target()
                }
            }
        }
        else {
            None
        }
    }
    pub fn get_loop_cont_target(&self) -> Option<BasicBlock> {
        if let Some(ptr) = self.outer {
            unsafe {
                if (*ptr).loop_cont_target.is_some() {
                    (*ptr).loop_cont_target
                }
                else {
                    (*ptr).get_loop_cont_target()
                }
            }
        }
        else {
            None
        }
    }
    pub fn set_outer_should_update_bb(&mut self, bb: BasicBlock) {
        if self.is_func_info() || self.is_global_info() {
            return;
        }
        if let Some(ptr) = self.outer {
            unsafe {
                (*ptr).should_update_bb = Some(bb);
            }
        }
    }
    pub fn set_returned(&mut self) {
        if self.ret_target_bb.is_some() {
            return;
        }
        self.returned = true;
        if let Some(ptr) = self.outer {
            unsafe {
                (*ptr).set_returned();
            }
        }
    }
    pub fn is_global_info(&self) -> bool {
        self.outer.is_none()
    }
    pub fn is_func_info(&self) -> bool {
        if let Some(ptr) = self.outer {
            unsafe {
                if let Some(ptr) = (*ptr).outer {
                    (*ptr).is_global_info()
                }
                else {
                    false
                }
            }
        }
        else {
            false
        }
    }
}

#[derive(Debug)]
pub struct CompUnit {
    pub global_decls: Vec<GlobalDecl>,
}

impl AbstractAst for CompUnit {}

#[derive(Debug)]
pub enum GlobalDecl {
    FuncDecl{
        ret_type: SysyType,
        ident: String,
        func_params: Vec<FuncParam>,
        block: Option<Block>,
    },
    OtherDecl(Decl),
}

impl AbstractAst for GlobalDecl {}

#[derive(Debug)]
pub struct FuncParam {
    pub param_type: SysyType,
    pub ident: Option<String>,
    pub indexes: Vec<Option<ConstExp>>,
}

impl AbstractAst for FuncParam {}

impl FuncParam {
    pub fn ty(&self, info: &AstGlobalInfo) -> SysyType {
        let mut ty = self.param_type.clone();
        if self.indexes.is_empty() {
            return ty
        }
        for (i, index) in self.indexes.iter().enumerate().rev() {
            match index.as_ref() {
                Some(exp) => ty = SysyType::ARR(Box::new(ty), exp.eval(info) as usize),
                None => {
                    if i != 0 {
                        panic!("not last dem");
                    }
                    ty = SysyType::PTR(Box::new(ty));
                }
            }
        }
        ty
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SysyType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    VOID,
    ARR(Box<SysyType>, usize), // elem type, elem num
    FUNC(Box<SysyType>, Vec<SysyType>), // retval type, params' types
    PTR(Box<SysyType>),
}

impl SysyType {
    pub fn get_koopa_type(&self) -> Type {
        match self {
            Self::VOID => Type::get_unit(),
            Self::I8 | Self::I16 | Self::I32 => Type::get_i32(),
            Self::PTR(ptr) => Type::get_pointer(ptr.get_koopa_type()),
            Self::ARR(elem, len) => Type::get_array(elem.get_koopa_type(), *len),
            _ => todo!()
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub items: Vec<BlockItem>,
}

impl AbstractAst for Block {}

impl Block {
    pub fn new_single_stmt(stmt: Stmt) -> Self {
        Block{ items: vec![BlockItem::S(stmt)] }
    }
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    S(Stmt),
    D(Decl),
}

impl AbstractAst for BlockItem {}

#[derive(Debug, Clone)]
pub enum Stmt {
    RET(Option<Expression>),
    ASN{ lval: LVal, exp: Expression },
    EXP(Option<Expression>),
    CND{ cond: Expression, stmt: Box<Stmt>, else_stmt: Option<Box<Stmt>> },
    WHILE{ cond: Expression, stmt: Box<Stmt> },
    BREAK,
    CONT,
    BLK(Box<Block>),
}

impl AbstractAst for Stmt {}

#[derive(Debug, Clone)]
pub enum Decl {
    C(ConstDecl),
    V(VarDecl),
}

impl AbstractAst for Decl {}

#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub decl_type: SysyType,
    pub def1: ConstDef,
    pub def_remain: Vec<ConstDef>,
}

impl AbstractAst for ConstDecl {}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub decl_type: SysyType,
    pub def1: VarDef,
    pub def_remain: Vec<VarDef>,
}

impl AbstractAst for VarDecl{}

#[derive(Debug, Clone)]
pub struct ConstDef {
    pub ident: String,
    pub indexes: Vec<ConstExp>,
    pub init_val: ConstInitVal,
}

impl AbstractAst for ConstDef {}

#[derive(Debug, Clone)]
pub struct VarDef {
    pub ident: String,
    pub indexes: Vec<ConstExp>,
    pub init_val: Option<InitVal>,
}

impl AbstractAst for VarDef {}

#[derive(Debug, Clone)]
pub struct ConstExp {
    pub expression: Expression,
}

impl AbstractAst for ConstExp {}

impl ConstExp {
    pub fn eval(&self, info: &AstGlobalInfo) -> i32 {
        self.expression.eval(info).unwrap()
    }
}

#[derive(Debug, Clone)]
pub enum ConstInitVal {
    Exp(ConstExp),
    List(Vec<ConstInitVal>),
}

impl AbstractAst for ConstInitVal {}

impl AbstractInitVal for ConstInitVal {
    fn optimize(&mut self, info: &AstGlobalInfo) {
        match self {
            Self::Exp(exp) => {exp.expression.optimize(info);},
            Self::List(list) => {
                for l in list {
                    l.optimize(info);
                }
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum InitVal {
    Exp(Expression),
    List(Vec<InitVal>),
}

impl AbstractAst for InitVal {}

impl AbstractInitVal for InitVal {
    fn optimize(&mut self, info: &AstGlobalInfo) {
        match self {
            Self::Exp(exp) => {exp.optimize(info);},
            Self::List(list) => {
                for l in list {
                    l.optimize(info);
                }
            },
        }
    }
}

impl InitVal {
    pub fn to_const(&self) -> ConstInitVal {
        let clone = self.clone();
        match clone {
            InitVal::Exp(expression) => ConstInitVal::Exp(ConstExp{expression}),
            InitVal::List(lst) => ConstInitVal::List(lst.into_iter().map(|x| x.to_const()).collect()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LVal {
    pub ident: String,
    pub indexes: Vec<Expression>,
}

impl AbstractAst for LVal {}

#[derive(Debug, Clone)]
pub struct Expression {
    pub binary_exp: BinaryExp,
}

impl AbstractAst for Expression {}

impl AbstractExp for Expression {
    fn eval(&self, info: &AstGlobalInfo) -> Option<i32> {
        self.binary_exp.eval(info)
    }
    fn optimize(&mut self, info: &AstGlobalInfo) -> Option<i32> {
        self.binary_exp.optimize(info)
    }
    fn new_single_value(value: i32) -> Self {
        Expression{binary_exp: BinaryExp::new_single_value(value)}
    }
}

#[derive(Debug, Clone)]
pub enum PrimaryExp {
    Exp(Box<Expression>),
    Num(i32),
    Ident(String),
}

impl AbstractAst for PrimaryExp {}

impl AbstractExp for PrimaryExp {
    fn eval(&self, info: &AstGlobalInfo) -> Option<i32> {
        match self {
            Self::Num(i) => Some(*i),
            Self::Exp(e) => e.eval(info),
            Self::Ident(id) => {
                if let Some(i) = info.find_symbol(id) {
                    match i.0 {
                        VarType::Const(_) => Some(i.1),
                        VarType::Var(_) => None,
                        VarType::Func(_) => None,
                    }
                }
                else {
                    panic!("{} not found", id)
                }
            },
        }
    }
    fn optimize(&mut self, info: &AstGlobalInfo) -> Option<i32> {
        let result = match self {
            Self::Num(i) => Some(*i),
            Self::Exp(e) => e.optimize(info),
            Self::Ident(id) => {
                if let Some(i) = info.find_symbol(id) {
                    match i.0 {
                        VarType::Const(_) => Some(i.1),
                        VarType::Var(_) => None,
                        VarType::Func(_) => None,
                    }
                }
                else {
                    panic!("{} not found", id)
                }
            },
        };
        match self {
            Self::Exp(_) | Self::Ident(_) => if let Some(i) = result {
                *self = Self::new_single_value(i);
            },
            _ => {}
        }
        result
    }
    fn new_single_value(value: i32) -> Self {
        Self::Num(value)
    }
}

#[derive(Debug, Clone)]
pub enum UnaryExp {
    U(PrimaryExp),
    B(Op, Box<UnaryExp>),
    C(String, Vec<Expression>),
    I(String, Vec<Expression>),
}

impl AbstractAst for UnaryExp {}

impl AbstractExp for UnaryExp {
    fn eval(&self, info: &AstGlobalInfo) -> Option<i32> {
        match self {
            Self::U(exp) => exp.eval(info),
            Self::B(op, exp) => {
                let val = exp.eval(info)?;
                Some(match op {
                    Op::Add => val,
                    Op::Sub => -val,
                    Op::Not => (val == 0) as i32,
                    _ => unreachable!()
                })
            },
            Self::C(_, _) => None,
            Self::I(_, _) => None,
        }
    }
    fn optimize(&mut self, info: &AstGlobalInfo) -> Option<i32> {
        let result = match self {
            Self::U(exp) => exp.optimize(info),
            Self::B(op, exp) => {
                let val = exp.optimize(info)?;
                Some(match op {
                    Op::Add => val,
                    Op::Sub => -val,
                    Op::Not => (val == 0) as i32,
                    _ => unreachable!()
                })
            }
            Self::C(_, params) => {
                let _: Vec<Option<i32>> = params.iter_mut().map(|x| x.optimize(info)).collect();
                None
            }
            Self::I(_, indexes) => {
                let _: Vec<Option<i32>> = indexes.iter_mut().map(|x| x.optimize(info)).collect();
                None
            }
        };
        match self {
            Self::B(_, _) => if let Some(i) = result {
                *self = Self::new_single_value(i);
            },
            _ => {}
        }
        result
    }
    fn new_single_value(value: i32) -> Self {
        Self::U(PrimaryExp::new_single_value(value))
    }
}

#[derive(Debug, Clone)]
pub enum BinaryExp {
    U(UnaryExp),
    T(Box<BinaryExp>, Op, Box<BinaryExp>),
}

impl AbstractAst for BinaryExp {}

impl AbstractExp for BinaryExp {
    fn eval(&self, info: &AstGlobalInfo) -> Option<i32> {
        match self {
            Self::U(exp) => exp.eval(info),
            Self::T(lhs, op, rhs) => {
                let lhs = lhs.eval(info)?;
                let rhs = rhs.eval(info)?;
                Some(match op {
                    // level 1
                    Op::Mul => lhs * rhs,
                    Op::Div => lhs / rhs,
                    Op::Mod => lhs % rhs,
                    // level 2
                    Op::Add => lhs + rhs,
                    Op::Sub => lhs - rhs,
                    // level 3
                    Op::Lt => (lhs < rhs) as i32,
                    Op::Le => (lhs <= rhs) as i32,
                    Op::Gt => (lhs > rhs) as i32,
                    Op::Ge => (lhs >= rhs) as i32,
                    // level 4
                    Op::Eq => (lhs == rhs) as i32,
                    Op::Ne => (lhs != rhs) as i32,
                    // level 5
                    Op::And => (lhs != 0 && rhs != 0) as i32,
                    // level 6
                    Op::Or => (lhs != 0 || rhs != 0) as i32,
                    _ => unreachable!()
                })
            }
        }
    }
    fn optimize(&mut self, info: &AstGlobalInfo) -> Option<i32> {
        let result = match self {
            Self::U(exp) => exp.optimize(info),
            Self::T(lhs, op, rhs) => {
                let lhs = lhs.optimize(info);
                let rhs = rhs.optimize(info);
                let lhs = lhs?;
                let rhs = rhs?;
                if *op == Op::Div && rhs == 0 {
                    panic!("Division by zero");
                }
                Some(match op {
                    // level 1
                    Op::Mul => lhs * rhs,
                    Op::Div => lhs / rhs,
                    Op::Mod => lhs % rhs,
                    // level 2
                    Op::Add => lhs + rhs,
                    Op::Sub => lhs - rhs,
                    // level 3
                    Op::Lt => (lhs < rhs) as i32,
                    Op::Le => (lhs <= rhs) as i32,
                    Op::Gt => (lhs > rhs) as i32,
                    Op::Ge => (lhs >= rhs) as i32,
                    // level 4
                    Op::Eq => (lhs == rhs) as i32,
                    Op::Ne => (lhs != rhs) as i32,
                    // level 5
                    Op::And => (lhs != 0 && rhs != 0) as i32,
                    // level 6
                    Op::Or => (lhs != 0 || rhs != 0) as i32,
                    _ => unreachable!()
                })
            }
        };
        match self {
            Self::T(_, _, _) => if let Some(i) = result {
                *self = Self::new_single_value(i);
            },
            _ => {}
        }
        result
    }
    fn new_single_value(value: i32) -> Self {
        Self::U(UnaryExp::new_single_value(value))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Not,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
}
