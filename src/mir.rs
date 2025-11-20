use crate::ast::AstNode;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Mir { pub stmts: Vec<MirStmt>, pub locals: HashMap<String, u32> }

#[derive(Debug, Clone)]
pub enum MirStmt {
    Assign { lhs: u32, rhs: MirExpr },
    Call { func: String, args: Vec<u32> },
    Return { val: u32 },
}

#[derive(Debug, Clone)]
pub enum MirExpr { Var(u32), Lit(i64), MethodCall { recv: u32, method: String, args: Vec<u32> } }

pub struct MirGen { next_id: u32, locals: HashMap<String, u32> }

impl MirGen {
    pub fn new() -> Self { Self { next_id: 0, locals: HashMap::new() } }

    pub fn gen_mir(&mut self, ast: &AstNode) -> Mir {
        let mut stmts = vec![];
        if let AstNode::FuncDef { body, params, .. } = ast {
            for (name, _) in params { let id = self.fresh(); self.locals.insert(name.clone(), id); }
            for node in body {
                if let AstNode::Let { name, rhs, .. } = node {
                    let lhs = self.fresh();
                    self.locals.insert(name.clone(), lhs);
                    if let Some(e) = self.gen_expr(rhs) {
                        stmts.push(MirStmt::Assign { lhs, rhs: e });
                    }
                }
            }
        }
        Mir { stmts, locals: self.locals.clone() }
    }

    fn fresh(&mut self) -> u32 { let id = self.next_id; self.next_id += 1; id }

    fn gen_expr(&mut self, node: &AstNode) -> Option<MirExpr> {
        match node {
            AstNode::Lit(v) => Some(MirExpr::Lit(*v)),
            AstNode::Var(name) => self.locals.get(name).map(|&id| MirExpr::Var(id)),
            _ => None,
        }
    }
}
