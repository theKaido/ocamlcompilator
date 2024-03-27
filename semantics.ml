open Ast
open Ast.IR

exception Error of string * Lexing.position

let rec analyze_expr env expr =
  match expr with
  | Syntax.Int n -> Int n.value
  | Syntax.Bool b -> Bool b.value
  | Syntax.Var v -> Var v.name
  | Syntax.Call c ->
      let args = List.map (analyze_expr env) c.args in
      Call (c.func, args)

let rec analyze_instr instr env =
  match instr with
  | Syntax.VarDecl dv -> (VarDecl dv.name, env)
  | Syntax.Assign a ->
                        let analyseexpr = analyze_expr env a.expr in
                            (Assign (a.var, analyseexpr), env)
  | Syntax.Return r ->
                      let ret = analyze_expr env r.expr in
                           (Return ret, env)
  | Syntax.Cond c ->
                    let condexpr = analyze_expr env c.cond in
                    let thenb = List.map (fun i -> fst (analyze_instr i env)) c.thenblock in
                    let elseb = List.map (fun i -> fst (analyze_instr i env)) c.elseblock in
                        (Cond (condexpr, thenb, elseb), env)
  | Syntax.Loop l ->
                    let condexpr = analyze_expr env l.cond in
                    let b = List.map (fun i -> fst (analyze_instr i env)) l.block in
                        (Loop (condexpr, b), env)

let rec analyze_block block env =
  match block with
  | i :: b ->
      let ai, new_env = analyze_instr i env in
      ai :: analyze_block b new_env
  | [] -> []

let analyze parsed = analyze_block parsed Baselib._types_
