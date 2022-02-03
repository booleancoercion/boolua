use std::ops::Range;

use crate::lex::Token;

use chumsky::prelude::*;
use chumsky::recursive::Recursive;

#[derive(Clone)]
pub struct Block {
    stmts: Vec<Stmt>,
    ret: Option<RetStmt>,
}

#[derive(Clone)]
pub enum Stmt {
    Empty,
    Assignment(VarList, ExprList),
    FnCall(FnCall),
    Label(Label),
    Break,
    Goto(Name),
    Do(Block),
    While(Expr, Block),
    Repeat(Block, Expr),
    If(Expr, Block, Vec<(Expr, Block)>, Option<Block>),
    ForNumeric(Name, Expr, Expr, Option<Expr>, Block),
    ForGeneric(NameList, ExprList, Block),
    FnDecl(FnName, FnBody),
    LocalFnDecl(Name, FnBody),
    LocalAssignment(AttNameList, Option<ExprList>),
}

#[derive(Clone)]
pub struct AttNameList(Vec<(Name, Attrib)>);

#[derive(Clone)]
pub struct Attrib(Option<Name>);

#[derive(Clone)]
pub struct RetStmt(Option<ExprList>);

#[derive(Clone)]
pub struct Label(Name);

#[derive(Clone)]
pub struct FnName(Vec<Name>, Option<Name>);

#[derive(Clone)]
pub struct VarList(Vec<Var>);

#[derive(Clone)]
pub enum Var {
    Simple(Name),
    TableElem(PrefixExpr, Expr),
}

#[derive(Clone)]
pub struct NameList(Vec<Name>);

#[derive(Clone)]
pub struct ExprList(Vec<Expr>);

#[derive(Clone)]
pub enum Expr {
    Nil,
    False,
    True,
    Num(Num),
    Str(String),
    VarArgs,
    FnDef(FnDef),
    Prefix(PrefixExpr),
    TableCtor(TableCtor),
    Binary {
        head: Box<Expr>,
        op: BinOp,
        tail: Box<Expr>,
    },
    Unary {
        op: UnOp,
        expr: Box<Expr>,
    },
}

#[derive(Clone)]
pub enum PrefixExpr {
    Var(Box<Var>),
    FnCall(Box<FnCall>),
    Parend(Box<Expr>),
}

#[derive(Clone)]
pub enum FnCall {
    Free {
        function: PrefixExpr,
        args: Args,
    },
    Method {
        table: PrefixExpr,
        name: Name,
        args: Args,
    },
}

#[derive(Clone)]
pub enum Args {
    List(Option<ExprList>),
    TableCtor(TableCtor),
    Str(String),
}

#[derive(Clone)]
pub struct FnDef(FnBody);

#[derive(Clone)]
pub struct FnBody(Option<ParList>, Block);

#[derive(Clone)]
pub enum ParList {
    Params(NameList, bool),
    VarArgs,
}

#[derive(Clone)]
pub struct TableCtor(Option<FieldList>);

#[derive(Clone)]
pub struct FieldList(Vec<Field>);

#[derive(Clone)]
pub enum Field {
    ExprExpr(Expr, Expr),
    NameExpr(Name, Expr),
    Expr(Expr),
}

#[derive(Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    FloorDiv,
    Exp,
    Mod,
    BitAnd,
    BitXor,
    BitOr,
    RightShift,
    LeftShift,
    Concat,
    Less,
    LessEq,
    Eq,
    NEq,
    And,
    Or,
}

#[derive(Clone, Copy)]
pub enum UnOp {
    Minus,
    Not,
    Length,
    BitNot,
}

#[derive(Clone)]
pub struct Name(pub String);

#[derive(Clone)]
pub enum Num {
    Int(i64),
    Float(f64),
}

macro_rules! declare {
    ($ident:ident) => {
        let mut $ident = Recursive::declare();

        let $ident = || $ident.clone();
    };

    ($($ident:ident),*) => {
        $(
            let $ident = Recursive::declare();
        )*

        $(
            let $ident = || $ident.clone();
        )*
    };
}

macro_rules! J {
    ($tok:tt) => {
        just(T![$tok])
    };

    ($($tok:tt),*) => {
        just([$(
            T![$tok]
        ),*])
    }
}

fn list<O, E, T>(
    parser: impl Parser<Token, O, Error = E> + Clone,
    delim: impl Parser<Token, T, Error = E>,
) -> impl Parser<Token, Vec<O>, Error = E>
where
    E: chumsky::Error<Token>,
{
    parser.clone().chain(delim.ignore_then(parser).repeated())
}

pub fn chunk(source: &str) -> impl Parser<Token, Block, Error = Simple<Token>> + '_ {
    let name = J![ident].map_with_span(|_, span: Range<usize>| Name(source[span].to_owned()));

    let litstring = todo();

    declare!(
        block,
        stmt,
        attnamelist,
        attrib,
        retstmt,
        label,
        funcname,
        varlist,
        var,
        namelist,
        exprlist,
        expr,
        prefixexpr,
        functioncall,
        args,
        functiondef,
        funcbody,
        parlist,
        tableconstructor,
        fieldlist,
        field
    );

    block().define(
        stmt()
            .repeated()
            .then(retstmt().or_not())
            .map(|(stmts, ret)| Block { stmts, ret }),
    );

    stmt().define(choice((
        J![;].to(Stmt::Empty),
        varlist()
            .then_ignore(J![=])
            .then(exprlist())
            .map(|(vars, exprs)| Stmt::Assignment(vars, exprs)),
        functioncall().map(Stmt::FnCall),
        label().map(Stmt::Label),
        J![break].to(Stmt::Break),
        J![goto].ignore_then(name).map(Stmt::Goto),
        block().delimited_by(T![do], T![end]).map(Stmt::Do),
        J![while]
            .ignore_then(expr())
            .then(block().delimited_by(T![do], T![end]))
            .map(|(expr, block)| Stmt::While(expr, block)),
        block()
            .delimited_by(T![repeat], T![until])
            .then(expr())
            .map(|(block, expr)| Stmt::Repeat(block, expr)),
        J![if]
            .ignore_then(expr())
            .then(
                block()
                    .then(
                        expr()
                            .delimited_by(T![elseif], T![then])
                            .then(block())
                            .repeated(),
                    )
                    .then(J![else].ignore_then(block()).or_not())
                    .delimited_by(T![then], T![end]),
            )
            .map(|(condexpr, ((block, elseifvec), elseblock))| {
                Stmt::If(condexpr, block, elseifvec, elseblock)
            }),
        J![for]
            .ignore_then(name)
            .then_ignore(J![=])
            .then(expr())
            .then_ignore(J![,])
            .then(expr())
            .then(J![,].ignore_then(expr()).or_not())
            .then(block().delimited_by(T![do], T![end]))
            .map(|((((name, expr1), expr2), optexpr), block)| {
                Stmt::ForNumeric(name, expr1, expr2, optexpr, block)
            }),
        J![for]
            .ignore_then(namelist())
            .then_ignore(J![in])
            .then(exprlist())
            .then(block().delimited_by(T![do], T![end]))
            .map(|((namelist, exprlist), block)| Stmt::ForGeneric(namelist, exprlist, block)),
        J![function]
            .ignore_then(funcname())
            .then(funcbody())
            .map(|(funcname, funcbody)| Stmt::FnDecl(funcname, funcbody)),
        J![local, function]
            .ignore_then(name)
            .then(funcbody())
            .map(|(name, funcbody)| Stmt::LocalFnDecl(name, funcbody)),
        J![local]
            .ignore_then(attnamelist())
            .then(J![=].ignore_then(exprlist()).or_not())
            .map(|(attnamelist, exprlist)| Stmt::LocalAssignment(attnamelist, exprlist)),
    )));

    attnamelist().define(list(name.then(attrib()), J![,]).map(AttNameList));

    attrib().define(name.delimited_by(T![<], T![>]).or_not().map(Attrib));

    retstmt().define(
        J![return]
            .ignore_then(exprlist().or_not())
            .then_ignore(J![;].or_not())
            .map(RetStmt),
    );

    label().define(name.delimited_by(T![::], T![::]).map(Label));

    funcname().define(
        list(name, J![.])
            .then(J![:].ignore_then(name).or_not())
            .map(|(names, optname)| FnName(names, optname)),
    );

    varlist().define(list(var(), J![,]).map(VarList));

    var().define(choice((
        name.map(Var::Simple),
        prefixexpr()
            .then(expr().delimited_by(T!['['], T![']']))
            .map(|(prefixexpr, expr)| Var::TableElem(prefixexpr, expr)),
        prefixexpr()
            .then_ignore(J![.])
            .then(name)
            .map(|(prefixexpr, name)| {
                let expr = Expr::Str(name.0); // desugaring
                Var::TableElem(prefixexpr, expr)
            }),
    )));

    namelist().define(list(name, J![,]).map(NameList));

    exprlist().define(list(expr(), J![,]).map(ExprList));

    expr().define(todo());

    prefixexpr().define(choice((
        var().map(|var| PrefixExpr::Var(Box::new(var))),
        functioncall().map(|fncall| PrefixExpr::FnCall(Box::new(fncall))),
        expr()
            .delimited_by(T!['('], T![')'])
            .map(|expr| PrefixExpr::Parend(Box::new(expr))),
    )));

    functioncall().define(choice((
        prefixexpr()
            .then(args())
            .map(|(function, args)| FnCall::Free { function, args }),
        prefixexpr()
            .then_ignore(J![:])
            .then(name)
            .then(args())
            .map(|((table, name), args)| FnCall::Method { table, name, args }),
    )));

    args().define(choice((
        exprlist()
            .or_not()
            .delimited_by(T!['('], T![')'])
            .map(Args::List),
        tableconstructor().map(Args::TableCtor),
        litstring.map(Args::Str),
    )));

    functiondef().define(J![function].ignore_then(funcbody()).map(FnDef));

    funcbody().define(
        parlist()
            .or_not()
            .delimited_by(T!['('], T![')'])
            .then(block())
            .then_ignore(J![end])
            .map(|(optparlist, block)| FnBody(optparlist, block)),
    );

    parlist().define(choice((
        namelist()
            .then(J![,, ...].or_not())
            .map(|(namelist, opt)| ParList::Params(namelist, opt.is_some())),
        J![...].to(ParList::VarArgs),
    )));

    tableconstructor().define(
        fieldlist()
            .or_not()
            .delimited_by(T!['{'], T!['}'])
            .map(TableCtor),
    );

    let fieldsep = J![,].or(J![;]);

    fieldlist().define(
        list(field(), fieldsep)
            .then_ignore(fieldsep.or_not())
            .map(FieldList),
    );

    field().define(choice((
        expr()
            .delimited_by(T!['['], T![']'])
            .then_ignore(J![=])
            .then(expr())
            .map(|(expr1, expr2)| Field::ExprExpr(expr1, expr2)),
        name.then_ignore(J![=])
            .then(expr())
            .map(|(name, expr)| Field::NameExpr(name, expr)),
        expr().map(Field::Expr),
    )));

    block()
}
