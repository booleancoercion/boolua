#[cfg(test)]
mod tests;
use crate::lex::{string, Token};
use crate::Span;

use chumsky::prelude::*;
use chumsky::recursive::Recursive;
use chumsky::select;

use std::ops::Range;

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub ret: Option<RetStmt>,
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
pub struct AttNameList(pub Vec<(Name, Attrib)>);

#[derive(Clone, Debug)]
pub struct Attrib(pub Option<Name>);

#[derive(Clone, Debug)]
pub struct RetStmt(pub Option<ExprList>);

#[derive(Clone, Debug)]
pub struct Label(pub Name);

#[derive(Clone, Debug)]
pub struct FnName(pub Vec<Name>, pub Option<Name>);

#[derive(Clone, Debug)]
pub struct VarList(pub Vec<Var>);

#[derive(Clone, Debug)]
pub struct NameList(pub Vec<Name>);

#[derive(Clone, Debug)]
pub struct ExprList(pub Vec<Expr>);

#[derive(Clone, Debug)]
pub enum Expr {
    Nil,
    False,
    True,
    Num(Num),
    Str(Vec<u8>),
    VarArgs,
    FnDef(FnDef),
    Prefix(PrefixExpr),
    TableCtor(TableCtor),
    Binary(Box<Self>, BinOp, Box<Self>),
    Unary(UnOp, Box<Self>),
}

#[derive(Clone, Debug)]
pub enum Args {
    List(Option<ExprList>),
    TableCtor(TableCtor),
    Str(Vec<u8>),
}

#[derive(Clone, Debug)]
pub struct FnCall(pub PrefixExpr, pub Option<Name>, pub Args);

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
pub struct FnDef(pub FnBody);

#[derive(Clone, Debug)]
pub struct FnBody(pub Option<ParList>, pub Block);

#[derive(Clone, Debug)]
pub enum ParList {
    Params(NameList, bool),
    VarArgs,
}

#[derive(Clone, Debug)]
pub struct TableCtor(pub Option<FieldList>);

#[derive(Clone, Debug)]
pub struct FieldList(pub Vec<Field>);

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

#[derive(Clone, Debug)]
pub struct Name(pub String);

#[derive(Clone, Debug)]
pub enum Num {
    Int(i64),
    Float(f64),
}

macro_rules! J {
    (longstring) => {
        select! {Token::LongString(n) => n}
    };

    ($($tok:tt)*) => {
        just(T![$($tok)*])
    };
}

fn list<O, E, T>(
    parser: impl Parser<Token, O, Error = E> + Clone,
    delim: impl Parser<Token, T, Error = E> + Clone,
) -> impl Parser<Token, Vec<O>, Error = E> + Clone
where
    E: chumsky::Error<Token>,
{
    parser.separated_by(delim).at_least(1)
}

pub fn chunk(source: &str) -> impl Parser<Token, Block, Error = Simple<Token>> + '_ {
    let name = J![ident].map_with_span(|_, span: Range<usize>| Name(source[span].to_owned()));

    let litstring = choice((
        J![shortstring]
            .map_with_span(|_, span: Span| {
                let innerspan = span.start + 1..span.end - 1; // remove "" or ''
                string::parse_short_string(&source[innerspan])
            })
            .validate(|(bytes, errors), span: Span, report| {
                for error in &errors {
                    let offset = span.start + 1;
                    let newspan = error.span.start + offset..error.span.end + offset;

                    report(Simple::custom(newspan, error.data.get_message()))
                }

                (bytes, errors)
            })
            .map(|(bytes, _)| bytes),
        J![longstring].map_with_span(|n, span: Span| {
            let slice = &source[span];
            let n = n + 2;

            let literal = &slice[n..slice.len() - n];
            let mut bytes = literal.as_bytes().to_owned();
            string::normalize_newlines(&mut bytes);
            if bytes.first() == Some(&b'\n') {
                bytes.remove(0);
            }

            bytes
        }),
    ));

    let stmt = Recursive::declare();
    let expr = Recursive::declare();

    let stmt = || stmt.clone();
    let expr = || expr.clone();

    let namelist = list(name, J![,]).map(NameList);

    let exprlist = list(expr(), J![,]).map(ExprList);

    let fieldsep = J![,].or(J![;]);

    let field = choice((
        expr()
            .delimited_by(J!['['], J![']'])
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
        .delimited_by(J!['{'], J!['}'])
        .map(TableCtor);

    let args = choice((
        exprlist
            .clone()
            .or_not()
            .delimited_by(J!['('], J![')'])
            .map(Args::List),
        tableconstructor.clone().map(Args::TableCtor),
        litstring.map(Args::Str),
    ));

    let parlist = choice((
        namelist
            .clone()
            .then(just([T![,], T![...]]).or_not())
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
        .delimited_by(J!['('], J![')'])
        .then(block())
        .then_ignore(J![end])
        .map(|(optparlist, block)| FnBody(optparlist, block));

    let functiondef = J![function].ignore_then(funcbody.clone()).map(FnDef);

    let attrib = name.delimited_by(J![<], J![>]).or_not().map(Attrib);
    let attnamelist = list(name.then(attrib), J![,]).map(AttNameList);

    let label = name.delimited_by(J![::], J![::]).map(Label);

    let funcname = list(name, J![.])
        .then(J![:].ignore_then(name).or_not())
        .map(|(names, optname)| FnName(names, optname));

    let atomexpr = choice((
        expr()
            .delimited_by(J!['('], J![')'])
            .map(|expr| PrefixExpr::Parend(Box::new(expr))),
        name.map(|name| PrefixExpr::Var(Box::new(Var::Simple(name)))),
    ));

    let callormethod = (J![:].ignore_then(name)).or_not().then(args.clone());

    let infix = choice((
        expr().delimited_by(J!['['], J![']']),
        J![.].ignore_then(name).map(|name| Expr::Str(name.0.into())),
    ));

    let fncallrest = infix.clone().repeated().then(callormethod);

    let foldinfix = |prefix, infix| PrefixExpr::Var(Box::new(Var::Indexed(prefix, infix)));
    let foldinfixes = move |prefix, infixes: Vec<Expr>| infixes.into_iter().fold(prefix, foldinfix);

    let functioncall = atomexpr
        .clone()
        .then(fncallrest.clone())
        .map(move |(prefix, (infixes, (nameopt, args)))| {
            let prefix = foldinfixes(prefix, infixes);
            FnCall(prefix, nameopt, args)
        })
        .then(fncallrest.repeated())
        .foldl(move |fncall, (infixes, (nameopt, args))| {
            let prefix = PrefixExpr::FnCall(Box::new(fncall));
            let prefix = foldinfixes(prefix, infixes);
            FnCall(prefix, nameopt, args)
        });

    let prefixstart = choice((
        functioncall
            .clone()
            .map(|fncall| PrefixExpr::FnCall(Box::new(fncall))),
        atomexpr,
    ));

    let var = choice((
        name.map(Var::Simple),
        prefixstart
            .clone()
            .then(infix.clone())
            .map(|(prefix, infix)| Var::Indexed(prefix, infix))
            .then(infix.clone().repeated())
            .foldl(|var, infix| {
                let prefix = PrefixExpr::Var(Box::new(var));
                Var::Indexed(prefix, infix)
            }),
    ));

    let prefixexpr = prefixstart.then(infix.repeated()).foldl(foldinfix);

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
        block().delimited_by(J![do], J![end]).map(Stmt::Do),
        J![while]
            .ignore_then(expr())
            .then(block().delimited_by(J![do], J![end]))
            .map(|(expr, block)| Stmt::While(expr, block)),
        block()
            .delimited_by(J![repeat], J![until])
            .then(expr())
            .map(|(block, expr)| Stmt::Repeat(block, expr)),
        J![if]
            .ignore_then(expr())
            .then(
                block()
                    .then(
                        expr()
                            .delimited_by(J![elseif], J![then])
                            .then(block())
                            .repeated(),
                    )
                    .then(J![else].ignore_then(block()).or_not())
                    .delimited_by(J![then], J![end]),
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
            .then(block().delimited_by(J![do], J![end]))
            .map(|((((name, expr1), expr2), optexpr), block)| {
                Stmt::ForNumeric(name, expr1, expr2, optexpr, block)
            }),
        J![for]
            .ignore_then(namelist.clone())
            .then_ignore(J![in])
            .then(exprlist.clone())
            .then(block().delimited_by(J![do], J![end]))
            .map(|((namelist, exprlist), block)| Stmt::ForGeneric(namelist, exprlist, block)),
        J![function]
            .ignore_then(funcname)
            .then(funcbody.clone())
            .map(|(funcname, funcbody)| Stmt::FnDecl(funcname, funcbody)),
        just([T![local], T![function]])
            .ignore_then(name)
            .then(funcbody)
            .map(|(name, funcbody)| Stmt::LocalFnDecl(name, funcbody)),
        J![local]
            .ignore_then(attnamelist.clone())
            .then(J![=].ignore_then(exprlist).or_not())
            .map(|(attnamelist, exprlist)| Stmt::LocalAssignment(attnamelist, exprlist)),
    )));

    let numlit = choice((
        J![declit].map_with_span(|_, span| todo!()),
        J![hexlit].map_with_span(|_, span| todo!()),
    ));

    let atomexpr = choice((
        J![nil].to(Expr::Nil),
        J![false].to(Expr::False),
        J![true].to(Expr::True),
        numlit.map(Expr::Num),
        litstring.map(Expr::Str),
        J![...].to(Expr::VarArgs),
        functiondef.map(Expr::FnDef),
        prefixexpr.map(Expr::Prefix),
        tableconstructor.map(Expr::TableCtor),
    ));

    let exponent = binary_right(atomexpr, J![^].to(BinOp::Exp));

    let op = choice((
        J![not].to(UnOp::Not),
        J![#].to(UnOp::Length),
        J![-].to(UnOp::Minus),
        J![~].to(UnOp::BitNot),
    ));
    let unary = op
        .repeated()
        .then(exponent)
        .foldr(|unop, acc| Expr::Unary(unop, Box::new(acc)));

    let op = choice((
        J![*].to(BinOp::Mul),
        J![/].to(BinOp::Div),
        J!["//"].to(BinOp::FloorDiv),
        J![%].to(BinOp::Mod),
    ));
    let multiplicative = binary_left(unary, op);

    let op = choice((J![+].to(BinOp::Add), J![-].to(BinOp::Sub)));
    let additive = binary_left(multiplicative, op);

    let concat = binary_right(additive, J![..].to(BinOp::Concat));

    let op = choice((J![>>].to(BinOp::RightShift), J![<<].to(BinOp::LeftShift)));
    let bitshift = binary_left(concat, op);

    let bitand = binary_left(bitshift, J![&].to(BinOp::BitAnd));

    let bitxor = binary_left(bitand, J![~].to(BinOp::BitXor));

    let bitor = binary_left(bitxor, J![|].to(BinOp::BitOr));

    let op = choice((
        J![<].to(BinOp::Less),
        J![>].to(BinOp::Greater),
        J![<=].to(BinOp::LessEq),
        J![>=].to(BinOp::GreaterEq),
        J![~=].to(BinOp::NEq),
        J![==].to(BinOp::Eq),
    ));
    let comparison = binary_left(bitor, op);

    let logic_and = binary_left(comparison, J![and].to(BinOp::And));

    let logic_or = binary_left(logic_and, J![or].to(BinOp::Or));

    expr().define(logic_or);

    block()
}

fn binary_left<'a, E>(
    previous: impl Parser<Token, Expr, Error = E> + Clone + 'a,
    operators: impl Parser<Token, BinOp, Error = E> + Clone + 'a,
) -> impl Parser<Token, Expr, Error = E> + Clone + 'a
where
    E: chumsky::Error<Token> + 'a,
{
    previous
        .clone()
        .then(operators.then(previous).repeated())
        .foldl(|acc, (op, expr)| Expr::Binary(Box::new(acc), op, Box::new(expr)))
        .boxed()
}

fn binary_right<'a, E>(
    previous: impl Parser<Token, Expr, Error = E> + Clone + 'a,
    operators: impl Parser<Token, BinOp, Error = E> + Clone + 'a,
) -> impl Parser<Token, Expr, Error = E> + Clone + 'a
where
    E: chumsky::Error<Token> + 'a,
{
    (previous.clone().then(operators).repeated())
        .then(previous)
        .foldr(|(expr, op), acc| Expr::Binary(Box::new(expr), op, Box::new(acc)))
        .boxed()
}
