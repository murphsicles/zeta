// src/mir.rs
use crate::ast::AstNode;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemiringOp {
    Add,
    Mul,
}

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
    pub fn new() -> Self {
        Self {
            next_id: 0,
            locals: HashMap::new(),
        }
    }

    pub fn gen_mir(&mut self, ast: &AstNode) -> Mir {
        let mut stmts = vec![];
        let ctfe_consts = HashMap::new();

        match ast {
            AstNode::FuncDef { body, params, .. } => {
                for (name, _) in params {
                    let id = self.alloc_local(name);
                    stmts.push(MirStmt::Assign {
                        lhs: id,
                        rhs: MirExpr::Var(id),
                    });
                }
                for node in body {
                    self.gen_stmt(node, &mut stmts);
                }
                stmts.push(MirStmt::Return { val: 0 });
            }
            _ => {}
        }

        Mir {
            stmts,
            locals: self.locals.clone(),
            ctfe_consts,
        }
    }

    fn gen_stmt(&mut self, node: &AstNode, out: &mut Vec<MirStmt>) {
        match node {
            AstNode::Assign(name, expr) => {
                let lhs = self.alloc_local(name);
                let rhs = self.gen_expr(expr);
                out.push(MirStmt::Assign { lhs, rhs });
            }
            AstNode::Call {
                receiver,
                method,
                args,
            } => {
                let recv = self.lookup_local(receiver);
                let arg_ids = args.iter().map(|a| self.lookup_local(a)).collect::<Vec<_>>();
                let dest = self.next_id();
                out.push(MirStmt::Call {
                    func: format!("{receiver}.{method}"),
                    args: vec![recv, arg_ids[0]],
                    dest,
                });
            }
            AstNode::Defer(inner) => {
                let mut deferred = vec![];
                self.gen_stmt(inner, &mut deferred);
                out.push(MirStmt::Defer {
                    stmt: Box::new(deferred.remove(0)),
                });
            }
            AstNode::SpawnActor { actor_ty, init_args } => {
                let actor_id = self.next_id();
                let dest = self.next_id();
                out.push(MirStmt::Spawn { actor: actor_id, dest });
            }
            AstNode::Await { expr } => {
                let future = self.gen_expr(expr);
                let dest = self.next_id();
                out.push(MirStmt::Await { future: dest, dest });
            }
            _ => {}
        }
    }

    fn gen_expr(&mut self, node: &AstNode) -> MirExpr {
        match node {
            AstNode::Lit(n) => MirExpr::Lit(*n),
            AstNode::Var(v) => MirExpr::Var(self.lookup_local(v)),
            _ => MirExpr::Lit(0),
        }
    }

    fn alloc_local(&mut self, name: &str) -> u32 {
        let id = self.next_id();
        self.locals.insert(name.to_string(), id);
        id
    }

    fn lookup_local(&self, name: &str) -> u32 {
        *self.locals.get(name).unwrap_or(&0)
    }

    fn next_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}
