namespace ProgramAnalysis
#load "Types.fsx"
#load "Programgraph.fsx"

open ProgramAnalysis.Types
module Analysis =

    let labelString = function
        | Fst(str)     -> str
        | Snd(str)     -> str
        | ArId(str, _) -> str
        | Id(str)      -> str

    (* 
        We can ignore warnings here, as the only actions ever to be added as input 
        here will be atomic actions 
    *)
    let rec grabIdentifier (act : Action) = 
        match act with
        | S(s) -> match s with
                  | Ass (l, _)    -> labelString l
                  | RAss (l, a1, a2) -> labelString l
                  | Read l        -> labelString l
                  | Write _       -> "Ø"
        | D(d) -> match d with
                  | DInt s        -> s
                  | DRecord s     -> s
                  | DArray (_, s) -> s     
        | B(_) -> "Ø"

    let rec tfRD ( ( ( (q :int), (act : Action), (q' : int)), (m : Map<string, (int * int) list>)) ) : Map<string, (int * int) list> =
         m.Add(grabIdentifier act, [(q, q')])