use std::ops::Range;

use crate::lex::Token;

use chumsky::prelude::*;
use chumsky::recursive::Recursive;

#[derive(Clone, Debug)]
pub struct Block {
    stmts: Vec<Stmt>,
    ret: Option<RetStmt>,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct AttNameList(Vec<(Name, Attrib)>);

#[derive(Clone, Debug)]
pub struct Attrib(Option<Name>);

#[derive(Clone, Debug)]
pub struct RetStmt(Option<ExprList>);

#[derive(Clone, Debug)]
pub struct Label(Name);

#[derive(Clone, Debug)]
pub struct FnName(Vec<Name>, Option<Name>);

#[derive(Clone, Debug)]
pub struct VarList(Vec<Var>);

#[derive(Clone, Debug)]
pub enum Var {
    Simple(Name),
    Indexed(PrefixStart, Vec<Expr>),
}

#[derive(Clone, Debug)]
pub struct NameList(Vec<Name>);

#[derive(Clone, Debug)]
pub struct ExprList(Vec<Expr>);

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum PrefixStart {
    FnCall(Box<FnCall>),
    Parend(Box<Expr>),
    Name(Name),
}

#[derive(Clone, Debug)]
pub struct PrefixExpr(PrefixStart, Vec<Expr>);

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum Args {
    List(Option<ExprList>),
    TableCtor(TableCtor),
    Str(String),
}

#[derive(Clone, Debug)]
pub struct FnDef(FnBody);

#[derive(Clone, Debug)]
pub struct FnBody(Option<ParList>, Block);

#[derive(Clone, Debug)]
pub enum ParList {
    Params(NameList, bool),
    VarArgs,
}

#[derive(Clone, Debug)]
pub struct TableCtor(Option<FieldList>);

#[derive(Clone, Debug)]
pub struct FieldList(Vec<Field>);

#[derive(Clone, Debug)]
pub enum Field {
    ExprExpr(Expr, Expr),
    NameExpr(Name, Expr),
    Expr(Expr),
}

#[derive(Clone, Copy, Debug)]
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

#[derive(Clone, Copy, Debug)]
pub enum UnOp {
    Minus,
    Not,
    Length,
    BitNot,
}

#[derive(Clone, Debug)]
pub struct Name(pub String);

#[derive(Clone, Debug)]
pub enum Num {
    Int(i64),
    Float(f64),
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
    delim: impl Parser<Token, T, Error = E> + Clone,
) -> impl Parser<Token, Vec<O>, Error = E> + Clone
where
    E: chumsky::Error<Token>,
{
    parser.clone().chain(delim.ignore_then(parser).repeated())
}

pub fn chunk(source: &str) -> impl Parser<Token, Block, Error = Simple<Token>> + '_ {
    let name = J![ident].map_with_span(|_, span: Range<usize>| Name(source[span].to_owned()));

    let litstring = todo();

    let stmt = Recursive::declare();
    let expr = Recursive::declare();
    let prefixexpr = Recursive::declare();

    let stmt = || stmt.clone();
    let expr = || expr.clone();
    let prefixexpr = || prefixexpr.clone();

    let namelist = list(name, J![,]).map(NameList);

    let exprlist = list(expr(), J![,]).map(ExprList);

    let fieldsep = J![,].or(J![;]);

    let field = choice((
        expr()
            .delimited_by(T!['['], T![']'])
            .then_ignore(J![=])
            .then(expr())
            .map(|(expr1, expr2)| Field::ExprExpr(expr1, expr2)),
        name.then_ignore(J![=])
            .then(expr())
            .map(|(name, expr)| Field::NameExpr(name, expr)),
        expr().map(Field::Expr),
    ));

    let fieldlist = list(field, fieldsep)
        .then_ignore(fieldsep.or_not())
        .map(FieldList);

    let tableconstructor = fieldlist
        .or_not()
        .delimited_by(T!['{'], T!['}'])
        .map(TableCtor);

    let args = choice((
        exprlist
            .clone()
            .or_not()
            .delimited_by(T!['('], T![')'])
            .map(Args::List),
        tableconstructor.map(Args::TableCtor),
        litstring.map(Args::Str),
    ));

    let functioncall = choice((
        prefixexpr()
            .then(args.clone())
            .map(|(function, args)| FnCall::Free { function, args }),
        prefixexpr()
            .then_ignore(J![:])
            .then(name)
            .then(args)
            .map(|((table, name), args)| FnCall::Method { table, name, args }),
    ));

    let parlist = choice((
        namelist
            .clone()
            .then(J![,, ...].or_not())
            .map(|(namelist, opt)| ParList::Params(namelist, opt.is_some())),
        J![...].to(ParList::VarArgs),
    ));

    let retstmt = J![return]
        .ignore_then(exprlist.clone().or_not())
        .then_ignore(J![;].or_not())
        .map(RetStmt);

    let block = stmt()
        .repeated()
        .then(retstmt.or_not())
        .map(|(stmts, ret)| Block { stmts, ret });
    let block = || block.clone();

    let funcbody = parlist
        .or_not()
        .delimited_by(T!['('], T![')'])
        .then(block())
        .then_ignore(J![end])
        .map(|(optparlist, block)| FnBody(optparlist, block));

    let _functiondef = J![function].ignore_then(funcbody.clone()).map(FnDef);

    let attrib = name.delimited_by(T![<], T![>]).or_not().map(Attrib);
    let attnamelist = list(name.then(attrib), J![,]).map(AttNameList);

    let label = name.delimited_by(T![::], T![::]).map(Label);

    let funcname = list(name, J![.])
        .then(J![:].ignore_then(name).or_not())
        .map(|(names, optname)| FnName(names, optname));

    let prefixstart = choice((
        functioncall
            .clone()
            .map(|fncall| PrefixStart::FnCall(Box::new(fncall))),
        expr()
            .delimited_by(T!['('], T![')'])
            .map(|expr| PrefixStart::Parend(Box::new(expr))),
        name.map(PrefixStart::Name),
    ));

    let prefixmid = choice((
        expr().delimited_by(T!['['], T![']']),
        J![.].ignore_then(name).map(|name| Expr::Str(name.0)),
    ));

    let var = choice((
        name.map(Var::Simple),
        prefixstart
            .clone()
            .then(prefixmid.clone().repeated().at_least(1))
            .map(|(start, indices)| Var::Indexed(start, indices)),
    ));

    let varlist = list(var.clone(), J![,]).map(VarList);

    stmt().define(choice((
        J![;].to(Stmt::Empty),
        varlist
            .then_ignore(J![=])
            .then(exprlist.clone())
            .map(|(vars, exprs)| Stmt::Assignment(vars, exprs)),
        functioncall.clone().map(Stmt::FnCall),
        label.map(Stmt::Label),
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
            .ignore_then(namelist.clone())
            .then_ignore(J![in])
            .then(exprlist.clone())
            .then(block().delimited_by(T![do], T![end]))
            .map(|((namelist, exprlist), block)| Stmt::ForGeneric(namelist, exprlist, block)),
        J![function]
            .ignore_then(funcname)
            .then(funcbody.clone())
            .map(|(funcname, funcbody)| Stmt::FnDecl(funcname, funcbody)),
        J![local, function]
            .ignore_then(name)
            .then(funcbody)
            .map(|(name, funcbody)| Stmt::LocalFnDecl(name, funcbody)),
        J![local]
            .ignore_then(attnamelist.clone())
            .then(J![=].ignore_then(exprlist).or_not())
            .map(|(attnamelist, exprlist)| Stmt::LocalAssignment(attnamelist, exprlist)),
    )));

    expr().define(todo());

    prefixexpr().define(
        prefixstart
            .then(prefixmid.repeated())
            .map(|(start, indices)| PrefixExpr(start, indices)),
    );

    block()
}
