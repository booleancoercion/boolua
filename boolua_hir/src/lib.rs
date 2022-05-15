mod structs;

use structs as hir;

use ast::Name;
use boolua_parser::structs as ast;

use std::collections::HashSet;

type Scope = HashSet<Name>;
type HirErr = ();

pub fn lower(input: ast::Block) -> Result<hir::Block, HirErr> {
    let mut scope = HashSet::new();
    scope.insert(Name("_ENV".into()));

    Ok(lower_block(input, &mut scope)?.0)
}

fn lower_block(input: ast::Block, scope: &mut Scope) -> Result<(hir::Block, Scope), HirErr> {
    let mut stmts = vec![];

    let mut inner_scope = HashSet::new();

    for stmt in input.stmts {
        match stmt {
            ast::Stmt::Assignment(vars, exprs) => {
                let newvars = vars.into_iter().map(|var| lower_var(var, scope)).collect();
                let newexprs = lower_exprs(exprs, scope)?;

                stmts.push(hir::Stmt::Assignment(newvars, newexprs))
            }
            ast::Stmt::Break => stmts.push(hir::Stmt::Break),
            ast::Stmt::Do(block) => {
                let (block, inner) = lower_block(block, scope)?;
                scope.retain(|elem| !inner.contains(elem));

                stmts.push(hir::Stmt::Do(block))
            }
            ast::Stmt::Empty => {}
            ast::Stmt::FnCall(ast::FnCall(prefix, method, args)) => {
                let prefix = lower_prefix(prefix, scope);
                let args = lower_exprs(args, scope)?;

                stmts.push(hir::Stmt::FnCall(hir::FnCall(prefix, method, args)))
            }
            ast::Stmt::FnDecl(fnname, args, hasvarargs, block) => {}
        }
    }

    todo!()
}

fn lower_var(input: ast::Var, scope: &mut Scope) -> hir::Var {
    todo!()
}

fn lower_expr(input: ast::Expr, scope: &mut Scope) -> Result<hir::Expr, HirErr> {
    todo!()
}

fn lower_exprs(input: Vec<ast::Expr>, scope: &mut Scope) -> Result<Vec<hir::Expr>, HirErr> {
    let mut newexprs = Vec::with_capacity(input.len());
    for expr in input {
        let expr = lower_expr(expr, scope)?;
        newexprs.push(expr);
    }

    Ok(newexprs)
}
