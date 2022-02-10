#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub ret: Option<Vec<Expr>>,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Empty,
    Assignment(Vec<Var>, Vec<Expr>),
    FnCall(FnCall),
    Label(Name),
    Break,
    Goto(Name),
    Do(Block),
    While(Expr, Block),
    Repeat(Block, Expr),
    If(Expr, Block, Vec<(Expr, Block)>, Option<Block>),
    ForNumeric(Name, Expr, Expr, Option<Expr>, Block),
    ForGeneric(Vec<Name>, Vec<Expr>, Block),
    FnDecl(FnName, Vec<Name>, bool, Block),
    LocalFnDecl(Name, Vec<Name>, bool, Block),
    LocalAssignment(Vec<(Name, Attrib)>, Vec<Expr>),
}

#[derive(Clone, Debug)]
pub struct Attrib(pub Option<Name>);

#[derive(Clone, Debug)]
pub struct FnName(pub Vec<Name>, pub Option<Name>);

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
    Simple(Name),
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
    NameExpr(Name, Expr),
    Expr(Expr),
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug)]
pub enum BinOp {
    Add, Sub, Mul, Div, FloorDiv, Exp, Mod,
    BitAnd, BitXor, BitOr, RightShift, LeftShift,
    Concat, Less, LessEq, Greater, GreaterEq, Eq,
    NEq, And, Or,
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug)]
pub enum UnOp {
    Minus, Not, Length, BitNot,
}

#[derive(Clone)]
pub struct Name(pub String);

#[derive(Clone)]
pub struct LuaStr(pub Vec<u8>);

#[derive(Clone, Debug)]
pub enum Num {
    Int(i64),
    Float(f64),
}
