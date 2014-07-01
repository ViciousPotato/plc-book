(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)

(* module Intro2 *)

(* Association lists map object language variables to their values *)

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

let emptyenv = []; (* the empty environment *)

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let cvalue = lookup env "c";;


(* Object language expressions with variables *)

type expr = 
  | CstI of int
  | Var of string
  | Prim of string * expr * expr;;

let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;


(* Evaluation within an environment *)

let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Prim("+", e1, e2) -> eval e1 env + eval e2 env
    | Prim("*", e1, e2) -> eval e1 env * eval e2 env
    | Prim("-", e1, e2) -> eval e1 env - eval e2 env
    | Prim("max", e1, e2) ->
      let e1v = eval e1 env
      let e2v = eval e2 env
      if e1v > e2v then e1v else e2v
    | Prim("min", e1, e2) ->
      let e1v = eval e1 env
      let e2v = eval e2 env
      if e1v < e2v then e1v else e2v
    | Prim("==", e1, e2) ->
      if (eval e1 env) = (eval e2 env) then 1 else 0
    | Prim _            -> failwith "unknown primitive";;

let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;

let mine = Prim("min", CstI 2, CstI 3);;
let minv = eval mine env;;
assert (minv = 2);;

(* b = 10, c = 2, eval min(b, 1==c) *)
let env2 = [("b", 10); ("c", 2)];;
let equalc = Prim("==", CstI 1, Var "c");;
let complex_expression = Prim("min", Var "b", equalc);;
let eval_complex_expression = eval complex_expression env2;;
assert (eval_complex_expression = 0);;
