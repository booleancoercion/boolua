use boolua_parser::structs::{Attrib, BinOp, LuaStr, Name, Num, UnOp};

#[derive(Clone, Debug)]
pub enum ResId {
    Name(Name),
    Internal(u32),
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub ret: Option<Vec<Expr>>,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Assignment(Vec<Var>, Vec<Expr>),
    FnCall(FnCall),
    Label(Name),
    Break,
    Goto(Name),
    Do(Block),
    While(Expr, Block),
    Repeat(Block, Expr),
    If(Expr, Block, Vec<(Expr, Block)>, Option<Block>),
    LocalAssignment(Vec<(ResId, Attrib)>, Vec<Expr>),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Nil,
    False,
    True,
    Num(Num),
    Str(LuaStr),
    VarArgs,
    FnDef(Vec<Name>, bool, Block),
    Prefix(PrefixExpr),
    TableCtor(Vec<Field>),
    Binary(Box<Self>, BinOp, Box<Self>),
    Unary(UnOp, Box<Self>),
}

#[derive(Clone, Debug)]
pub struct FnCall(pub PrefixExpr, pub Option<Name>, pub Vec<Expr>);

#[derive(Clone, Debug)]
pub enum Var {
    LocalName(ResId),
    Indexed(PrefixExpr, Expr),
}

#[derive(Clone, Debug)]
pub enum PrefixExpr {
    Var(Box<Var>),
    FnCall(Box<FnCall>),
    Parend(Box<Expr>),
}

#[derive(Clone, Debug)]
pub enum Field {
    ExprExpr(Expr, Expr),
    Expr(Expr),
}
