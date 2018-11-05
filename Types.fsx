namespace ProgramAnalysis
module Types =

    type Program =
                 | Program of Decl * Stm

    and Stm      =                              (* statements                *)
                 | Ass     of Label * AExp     (* assignment                *)
                 | Skip
                 | Seq     of Stm  * Stm         (* sequential composition    *)
                 | ITE     of BExp * Stm * Stm  (* if-then-else              *)
                 | IT      of BExp * Stm        (* if-then                   *)      
                 | While   of BExp * Stm        (* while                     *)

    and Label    =
                 | Fst     of string
                 | Snd     of string
                 | ArId    of string * AExp
                 | Id      of string

    and AExp     =                              (* Arithmetical expressions  *) 
                 | Int     of int               (* numbers                   *)
                 | Var     of Label            (* variables                 *)
                 | AOp     of AExp * OpA * AExp

    and BExp     =                              (* boolean expressions       *)
                 | TT                          (* true                      *)
                 | FF                           (* false                     *)
                 | ROp     of AExp * OpR * AExp
                 | BOp     of BExp * OpB * BExp
                 | Neg     of BExp

    and Decl     =
                 | DInt    of string
                 | DRecord of string
                 | DArray  of int  * string
                 | DD      of Decl * Decl
                 | DEmpty

    and OpA      =  
                 | Add 
                 | Sub 
                 | Mul 
                 | Div 
                 | Mod

    and OpR      =  
                 | Lt  
                 | Gt  
                 | Leq  
                 | Geq  
                 | Eq

    and OpB      =  
                 | Con 
                 | Dis

    and Action   = 
                 | D of Decl
                 | S of Stm
                 | B of BExp


    let rec prettyStm  = function
                 | Ass (l, a) -> prettyLabel l + prettyAExp a
                 | Skip -> "skip"
                 | Seq (s1, s2) -> prettyStm s1 + prettyStm s2
                 | ITE (b, s1, s2) -> prettyBExp b + prettyStm s1 + prettyStm s2
                 | IT (b, s) -> prettyBExp b + prettyStm s     
                 | While (b, s) -> prettyBExp b + prettyStm s
                
    and prettyLabel = function
        | Fst f -> f
        | Snd s -> s
        | ArId (s,a) -> s+"[" + prettyAExp a + "]"
        | Id i -> i

    and prettyAExp = function
        | Int i -> string i 
        | Var v -> prettyLabel v 
        | AOp (a1, opa, a2)  -> prettyAExp a1 + prettyOpA opa + prettyAExp a2

    and prettyBExp = function
        | TT -> "true"
        | FF -> "false"             
        | ROp (a1, opr, a2) -> prettyAExp a1 + prettyOpR opr + prettyAExp a2
        | BOp (b1, opb, b2) -> prettyBExp b1 + prettyOpB opb + prettyBExp b2 
        | Neg s -> "!" + prettyBExp s 

    and prettyOpA = function
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | Mod -> "%"

    and prettyOpR = function
        | Lt -> "<"
        | Gt -> ">"
        | Leq -> "<="
        | Geq -> ">="
        | Eq -> "=="

    and prettyOpB = function
        | Con -> "&&"
        | Dis -> "||"

    type ProgramGraph = Map<int, (int * Action) list>