type AExp =                            (* Arithmetical expressions  *) 
          | N   of int                 (* numbers                   *)
          | V   of string              (* variables                 *)
          | Add of AExp * AExp         (* addition                  *)
          | Mul of AExp * AExp         (* multiplication            *)
          | Sub of AExp * AExp         (* subtraction               *)
          | Div of AExp * AExp         (* division                  *)
          | Mod of AExp * AExp         (* modulus                   *)


type BExp =                            (* boolean expressions       *)
          | TT                         (* true                      *)
          | FF                         (* false                     *)
          | Eq  of AExp * AExp         (* equality                  *)
          | GEq of AExp * AExp         (* greater equal             *)
          | LEq of AExp * AExp         (* Less equal                *)
          | Lt  of AExp * AExp         (* less than                 *)
          | Gt  of AExp * AExp         (* greater than              *)
          | Neg of BExp                (* negation                  *)
          | Con of BExp * BExp         (* conjunction               *)
          | Dis of BExp * BExp         (* disjunction               *)

type Stm  =                            (* statements                *)
          | Ass   of string * AExp     (* assignment                *)
          | Skip
          | Seq   of Stm * Stm         (* sequential composition    *)
          | ITE   of BExp * Stm * Stm  (* if-then-else              *)
          | IT    of BExp * Stm        (* if-then                   *)      
          | While of BExp * Stm        (* while                     *)



type State = Map<string,int>

(* update: string -> int -> State -> State  *)
let update x v s = Map.add x v s 

(* A: AExp -> State -> int                   *)
let rec A a s      = 
   match a with 
    | N n         -> n
    | V x         -> Map.find x s
    | Add(a1, a2) -> A a1 s + A a2 s
    | Mul(a1, a2) -> A a1 s * A a2 s
    | Sub(a1, a2) -> A a1 s - A a2 s
    | Div(a1, a2) -> A a1 s / A a2 s
    | Mod(a1, a2) -> A a2 s % A a2 s

(* B: BExp -> State -> bool                  *)
let rec B b s =
   match b with 
    | TT          -> true
    | FF          -> false
    | Eq(e1, e2)  -> A e1 s = A e2 s
    | LEq(e1, e2) -> A e1 s <= A e2 s
    | GEq(e1, e2) -> A e1 s >= A e2 s
    | Lt(e1, e2)  -> A e1 s < A e2 s 
    | Gt(e1, e2)  -> A e1 s > A e2 s
    | Neg e       -> not (B e s) 
    | Con(e1, e2) -> (B e1 s) && (B e2 s)
    | Dis(e1, e2) -> (B e1 s) || (B e2 s)

(* I: Stm -> State -> State                          *)
let rec I stm s =
    match stm with 
    | Ass(x,a)          -> update x (A a s) s
    | Skip              -> s
    | Seq(stm1, stm2)   -> I stm2 (I stm1 s)
    | ITE(b,stm1,stm2)  -> if (B b s) then I stm1 s else I stm2 s
    | IT(b, stm1) -> if (B b s) then I stm1 s else s
    | While(b, stm)     -> if (B b s) then I <| While(b, stm) <| I stm s else s

(* Factorial computation 
{pre: x = K and x>=0} 
   y:=1 ; while !(x=0) do (y:= y*x;x:=x-1) 
post: {y = K!}
*)

let fac = Seq(Ass("y", N 1), 
              While(Neg(Eq(V "x", N 0)), 
                    Seq(Ass("y", Mul(V "x", V "y")) , Ass("x", Sub(V "x", N 1)) ))
             )

let ifthen = IT(Eq(V "x", N 0), Ass("x", N 1))
let ifthenS0 = Map.ofList[("x",0)]
let ifthenS1 =  I ifthen ifthenS0
Map.find "x" ifthenS1
(* Define an initial state                           *)
//let s0 = Map.ofList [("x",4)]

(* Interpret the program                             *)
//let s1 = I fac s0

(* Inspect the resulting state                       *)
//Map.find "y" s1