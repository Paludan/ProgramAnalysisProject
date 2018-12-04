namespace ProgramAnalysis

module Types =

    type Program =
                 | Program of Decl * Stm

    and Stm      =                             
                 | Ass     of Label * AExp   
                 | RAss    of Label * AExp * AExp                
                 | Skip
                 | Seq     of Stm  * Stm        
                 | ITE     of BExp * Stm * Stm 
                 | IT      of BExp * Stm            
                 | While   of BExp * Stm       
                 | Read    of Label
                 | Write   of AExp

    and Label    =
                 | Fst     of string 
                 | Snd     of string
                 | ArId    of string * AExp
                 | Id      of string

    and AExp     =                               
                 | Int     of int               
                 | Var     of Label           
                 | AOp     of AExp * OpA * AExp

    and BExp     =                             
                 | TT                          
                 | FF                           
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

    type Signs   =
                 | Negative
                 | Zero
                 | Positive

    type ProgramGraph = Map<int, (int * Action) list>

    let rec prettyDecl d = 
        match d with 
        | DInt l -> "int " + l + ";\n"
        | DRecord l -> "R = " + l + ";\n"
        | DArray (i, l) -> l + "[" + i.ToString() + "];\n"
        | DD (d1, d2)  -> prettyDecl d1 + prettyDecl d2
        | DEmpty  -> ""

    and prettyStm s = 
        match s with
        | Ass (l, a) -> prettyLabel l + " = " + prettyAExp a + ";\n"
        | RAss (l, a1, a2) -> prettyLabel l + " = " + prettyAExp a1 + prettyAExp a2 + ";\n"
        | Skip -> ""
        | Seq (s1, s2) -> prettyStm s1 + prettyStm s2
        | ITE (b, s1, s2) -> "if (" + prettyBExp b + ") " + prettyStm s1 + " else " + prettyStm s2 + "\n"
        | IT (b, s) -> "if (" + prettyBExp b + ") " + prettyStm s + "\n"
        | While (b, s) -> "while (" + prettyBExp b + ") " + prettyStm s + "\n"
        | Read l -> "read " + prettyLabel l + ";\n"
        | Write a -> "write " + prettyAExp a + ";\n"

    and prettyLabel (l : Label) = 
        match l with
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

    let prettyAST (ast: (Decl * Stm) ) : string = (fst ast |> prettyDecl) + (snd ast |> prettyStm)