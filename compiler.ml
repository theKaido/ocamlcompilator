open Ast.IR
open Mips
module Env = Map.Make (String)

type cinfo = { asm : Mips.instr list; env : Mips.loc Env.t; fpo : int }

let newlabel =
  let counter = ref 0 in
  fun () ->
    incr counter;
    "l" ^ string_of_int !counter

let rec compile_expr e env =
  match e with
  | Int n          -> [ Li (V0, n) ]
  | Bool b         -> [ Li (V0, if b then 1 else 0) ]
  | Var v          -> [ Lw (V0, Env.find v env) ]
  | Call (f, args) -> let ca = List.map (fun a -> compile_expr a env @ [ Addi (SP, SP, -4); Sw (V0, Mem (SP, 0)) ])
        args
      in
        List.flatten ca @ [ Jal f; Addi (SP, SP, 4 * List.length args) ]

let rec compile_instr instr info =
  match instr with
  | VarDecl v     -> {
                      info with
                      fpo = info.fpo - 4;
                      env = Env.add v (Mem (FP, info.fpo)) info.env;
                      }
  | Assign (v, e) -> {
                      info with
                      asm =
                      info.asm 
                      @ compile_expr e info.env 
                      @ [ Sw (V0, Env.find v info.env) ];
                      }
  | Return e -> let compiled_expr = compile_expr e info.env in
                      { 
                        info with asm =
                        info.asm 
                        @ compiled_expr 
                        @ [ Move (SP, FP); Jr RA ] 
                      }
  | Cond (cond, then_block, else_block) ->
                                        let elsel = newlabel () in
                                        let endl = newlabel () in
                                        let ninfo ={
                                                        info with
                                                        asm =
                                                        info.asm 
                                                        @ compile_expr cond info.env 
                                                        @ [ Beqz (V0, elsel) ];
                                                      }
                                                      in
                                                      let ithen = compile_block then_block ninfo in
                                                      let idthen =
                                                      {
                                                        ithen with
                                                        asm = 
                                                        ithen.asm 
                                                        @ [ Jal endl; Label elsel ];
                                                      }
                                                      in
                                                      let ielse = compile_block else_block idthen in
                                                      let idelse = { 
                                                                              ielse with 
                                                                              asm = ielse.asm 
                                                                              @ [ Label endl ] 
                                                                            }
                                                      in
                                                      
                                                        idelse
  | Loop (cond, block) ->
                          let loopl = newlabel () in
                          let endl = newlabel () in
                          let ninfo ={
                                          info with
                                          asm = 
                                          info.asm 
                                          @ compile_expr cond info.env 
                                          @ [ Beqz (V0, endl) ];
                                        }
                                        in
                                        let iloop = compile_block block ninfo in
                                        let idloop ={
                                                              iloop with
                                                              asm = 
                                                              iloop.asm 
                                                              @ [ B loopl; Label endl ];
                                                              }
                                        in
                                        
                                        { 
                                          idloop with 
                                          asm = 
                                          info.asm 
                                          @ [ Label loopl ] 
                                        }

and compile_block block info =
  match block with
  | i :: b ->
      let ninfo = compile_instr i info in
      compile_block b ninfo
  | [] -> info

let compile ir =
  let info =
    compile_block ir { 
                      asm = Baselib.builtins;
                            env = Env.empty;
                            fpo = 0 
                      }
  in
  { text = Move (FP, SP) :: Addi (FP, SP, info.fpo) :: info.asm; data = [] }
