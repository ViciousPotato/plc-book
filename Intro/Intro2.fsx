let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x

type expr = 
  | CstI of int
  | Var of string
  | Prim of string * expr * expr
  | If of expr * expr * expr

type aexpr =
    | ACstI of int
    | AVar of string
    | Mul of aexpr * aexpr
    | Add of aexpr * aexpr
    | Sub of aexpr * aexpr

let rec fmt e =
    match e with
    | ACstI i -> i.ToString()
    | AVar x -> x
    | Mul (e1, e2) -> "(" + (fmt e1) + "*" + (fmt e2) + ")"
    | Add (e1, e2) -> "(" + (fmt e1) + "+" + (fmt e2) + ")"
    | Sub (e1, e2) -> "(" + (fmt e1) + "-" + (fmt e2) + ")"

let rec eq e1 e2 =
    match e1, e2 with
    | AVar v1, AVar v2 -> v1 = v2
    | ACstI c1, ACstI c2 -> c1 = c2
    | Mul(m1, m2),  Mul(n1, n2) -> (eq m1 n1) && (eq m2 n2) || (eq m1 n2) && (eq m2 n1)
    | Add(m1, m2), Add(n1, n2) -> (eq m1 n1) && (eq m2 n2) || (eq m1 n2) && (eq m2 n1)
    | Sub(m1, m2), Sub(n1, n2) -> (eq m1 n1) && (eq m2 n2)
    | _, _ -> false

let rec simplify e =
    match e with
    | ACstI i -> ACstI i
    | AVar x -> AVar x
    | Mul(e1, e2) ->
        match e1, e2 with
        | ACstI 0, _ -> ACstI 0
        | ACstI 1, _ -> simplify e2
        | _, ACstI 0 -> ACstI 0
        | _, ACstI 1 -> simplify e1
        | _, _ -> simplify (Mul(simplify e1, simplify e2))
    | Add(e1, e2) ->
        match e1, e2 with
        | ACstI 0, _ -> simplify e2
        | _, ACstI 0 -> simplify e1
        | _, _ -> simplify (Add(simplify e1, simplify e2))
    | Sub (e1, e2) -> if eq e1 e2 then ACstI 0 else simplify (Sub(simplify e1, simplify e2))
    | _ -> e

 
(* Evaluation within an environment *)
let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x
    | Prim(ope, e1, e2) ->
        let i1 = eval e1 env
        let i2 = eval e2 env
        match ope with
        | "+" -> i1 + i2
        | "*" -> i1 * i2
        | "-" -> i1 - i2
        | "max" -> if i1 > i2 then i1 else i2
        | "min" -> if i1 > i2 then i2 else i1
        | "==" -> if i1 = i2 then 1 else 0
        | _ -> failwith "Unknown primitive"
    | If (e1, e2, e3) ->
        if eval e1 env <> 0 then eval e2 env else eval e3 env
    ;;

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

let emptyenv = []; (* the empty environment *)

let cvalue = lookup env "c";;

let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;

let e1v  = eval e1 env;;

let e2v1 = eval e2 env;;

let e2v2 = eval e2 [("a", 314)];;

let e3v  = eval e3 env;;

let mine = Prim("min", CstI 2, CstI 3);;

let minv = eval mine env;;

assert (minv = 2);;

(* b = 10, c = 2, eval min(b, 1==c) = 0 *)
let env2 = [("b", 10); ("c", 2)];;

let equalc = Prim("==", CstI 1, Var "c");;

let complex_expression = Prim("min", Var "b", equalc);;

let eval_complex_expression = eval complex_expression env2;;

assert (eval_complex_expression = 0);;

(* 1.1 v *)
let ifexpr = If(Var("a"), CstI 11, CstI 12);;
let ifv = eval ifexpr [("a", 1)];;
printfn "%d" ifv;;

(* 1.2 iii *)
let fv = Sub(AVar "x", ACstI 34)
printfn "%s" (fmt fv)

(* 1.2 iv (1+0) * (x+0) = x *)
let simexpr = Mul( Add( ACstI 1, ACstI 0), Add( AVar "x", ACstI 0) )
let s = simplify simexpr
printfn "%s" (fmt s)

(*1.2 v - 1.4 *)
(*
Write an F# function to perform symbolic differentiation of simple arithmetic expressions (such as aexpr) with respect to a single variable.
This is a bit hard I will leave it to future
*)