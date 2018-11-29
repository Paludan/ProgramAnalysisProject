namespace ProgramAnalysis
open System
#load "Types.fsx"
#load "Programgraph.fsx"

open ProgramAnalysis.Types
open Programgraph

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

    let mapIsSubset (m1 : Map<'a, 'b list>) (m2 : Map<'a, 'b list>) : bool =
        m1
        |> Map.fold (fun result key value ->
            result &&
            match m2.TryFind key with
            | Some y -> Set.isSubset (Set.ofList value) (Set.ofList y)
            | _ -> false
        ) true
    let mapValueUnion (m1 : Map<'a, 'b list>) (m2 : Map<'a, 'b list>) =
        m1 |> Map.fold (fun m key value ->
            match Map.tryFind key m with
            | Some x -> m.Add(key, value @ x)
            | None   -> m
        ) m2
        
    let leastUpperBound (m1 : Map<'a, 'b list>) (m2 : Map<'a, 'b list>) f =
        m1 |> f <| m2

    let grabIdentifier (act : Action) = 
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

    let analysisResultInit (pg : ProgramGraph) bottom : Map<int, Map<'a, 'b>> =
        pg |> Map.fold (fun x key _ ->
            x.Add(key, bottom)
        ) Map.empty

    let workListInit (pg : ProgramGraph) : int list =
        pg |> Map.fold(fun x key _ ->
                x @ [key]
            ) []
            
    let tfRD (q : int) (q' : int, act : Action) (m : Map<string, (int * int) list>)  : Map<string, (int * int) list> =
         m.Add(grabIdentifier act, [(q, q')]) 

    let rec workListSolve (pg : ProgramGraph) workList (analysisResult : Map<int, Map<string, 'b list>>) (tf : int -> int  * Action -> Map<string,(int * int) list> -> Map<string,(int * int) list> ) = 
        match workList with
        | [] -> analysisResult
        | q::xs ->
            let (analysisResult', workList') =
                Map.find q pg
                |> List.fold (fun ((m : Map<_,_>), workList) (q', act) -> 
                    if ( not ( mapIsSubset (tf q (q', act) (Map.find q m)) (Map.find q' analysisResult) ) ) 
                        then m.Add(q', (leastUpperBound (Map.find q' analysisResult) (tf q (q', act) (Map.find q m) ) mapValueUnion) ), xs @ [q'] 
                    else (m, xs) 
                ) (analysisResult, workList)
            workListSolve pg workList' analysisResult' tf
        
        
    let workListAlgorithm (pg : ProgramGraph) bottom dhat tf =
        let mutable analysisResult = analysisResultInit <| pg <| bottom
        let mutable workList = workListInit pg
        //analysisResult.Add(0, [dhat])
        workListSolve pg workList analysisResult tf