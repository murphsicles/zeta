// src/mir.rs
use crate::ast::AstNode;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemiringOp { Add, Mul }

#[derive(Debug, Clone)]
pub struct Mir {
    pub stmts: Vec<MirStmt>,
    pub locals: HashMap<String, u32>,
    pub ctfe_consts: HashMap<u32, i64>,
}

#[derive(Debug, Clone)]
pub enum MirStmt {
    Assign { lhs: u32, rhs: MirExpr },
    Call { func: String, args: Vec<u32>, dest: u32 },
    Return { val: u32 },
    Defer { stmt: Box<MirStmt> },
    SemiringFold { op: SemiringOp, values: Vec<u32>, result: u32 },
    Spawn { actor: u32, dest: u32 },
    Send { channel: u32, value: u32 },
    Recv { channel: u32, dest: u32 },
    Await { future: u32, dest: u32 },
}

#[derive(Debug, Clone)]
pub enum MirExpr {
    Var(u32),
    Lit(i64),
    MethodCall { recv: u32, method: String, args: Vec<u32> },
    ConstEval(i64),
}

pub struct MirGen {
    next_id: u32,
    locals: HashMap<String, u32>,
}

impl MirGen {
    pub fn new() -> Self { Self { next_id: 0, locals: HashMap::new() } }

    pub fn gen_mir(&mut self, ast: &AstNode) -> Mir {
        let mut stmts = vec![];
        if let AstNode::FuncDef { body, .. } = ast {
            for stmt in body { self.gen_stmt(stmt, &mut stmts); }
            if let Some(AstNode::Assign(name, expr)) = body.last() {
                if name == "_" {
                    let ret = self.gen_expr(expr);
                    let id = match ret {
                        MirExpr::Var(v) => v,
                        MirExpr::Lit(n) => { let id = self.next_id(); self.locals.insert(format!("__lit_{}", n), id); id }
                        _ => 0,
                    };
                    stmts.push(MirStmt::Return { val: id });
                }
            }
        }
        Mir { stmts, locals: self.locals.clone(), ctfe_consts: HashMap::new() }
    }

    fn gen_stmt(&mut self, node: &AstNode, out: &mut Vec<MirStmt>) {
        match node {
            AstNode::Assign(name, expr) => {
                let lhs = self.alloc_local(name);
                let rhs = self.gen_expr(expr);
                out.push(MirStmt::Assign { lhs, rhs });
            }
            AstNode::Call { receiver, method, args } => {
                let recv = self.gen_expr(receiver);
                let recv_id = match recv {
                    MirExpr::Var(v) => v,
                    _ => { let id = self.next_id(); out.push(MirStmt::Assign { lhs: id, rhs: recv }); id }
                };
                let arg_ids = args.iter().map(|a| match self.gen_expr(a) {
                    MirExpr::Var(v) => v,
                    other => { let id = self.next_id(); out.push(MirStmt::Assign { lhs: id, rhs: other }); id }
                }).collect::<Vec<_>>();
                let dest = self.next_id();
                out.push(MirStmt::Call { func: method.clone(), args: vec![recv_id, arg_ids[0]], dest });
            }
            _ => {}
        }
    }

    fn gen_expr(&mut self, node: &AstNode) -> MirExpr {
        match node {
            AstNode::Lit(n) => MirExpr::Lit(*n),
            AstNode::Var(v) => MirExpr::Var(self.lookup_or_alloc(v)),
            AstNode::Call { .. } => {
                let dest = self.next_id();
                self.gen_stmt(node, &mut Vec::new()); // emit into temp
                MirExpr::Var(dest)
            }
            _ => MirExpr::Lit(0),
        }
    }

    fn alloc_local(&mut self, name: &str) -> u32 {
        let id = self.next_id();
        self.locals.insert(name.to_string(), id);
        id
    }

    fn lookup_or_alloc(&mut self, name: &str) -> u32 {
        let name_str = name.to_string();
        *self.locals.entry(name_str).or_insert_with(|| {
            let id = self.next_id;
            self.next_id += 1;
            id
        })
    }
    fn next_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}
