namespace ProgramAnalysis

open System
#load "Types.fsx"
#load "Programgraph.fsx"

open Types
open Programgraph

module Analysis =

    (* Takes a Label and returns the string *)
    let labelString = function
        | Fst(str)     -> str
        | Snd(str)     -> str
        | ArId(str, _) -> str
        | Id(str)      -> str

    let lifo (worklist : 'a list) (q : 'a) : 'a list =
        worklist @ [q]

    let fifo (worklist : 'a list) (q : 'a) : 'a list = 
        [q] @ worklist

    (* Checks if the first map is a subseteq of the second map, returns true/false *)
    let mapIsSubseteq (m1 : Map<'a, 'b Set>) (m2 : Map<'a, 'b Set>) : bool =
        m1
        |> Map.fold (fun result key value ->
            result &&
            match m2.TryFind key with
            | Some y -> (Set.isSubset value y) || (y.Equals(value))
            | _ -> false
        ) true

    (* Takes two maps and returns a map with the values of m1 Union with values of m2 *)
    let mapPointwiseUnion (m1 : Map<'a, 'b Set>) (m2 : Map<'a, 'b Set>) =
        m1 |> Map.fold (fun m key value ->
            match Map.tryFind key m with
            | Some x -> m.Add(key, Set.union value x)
            | None   -> m
        ) m2

    (* Takes two maps and a leastUpperBound function e.g. Union *)
    let leastUpperBound (m1 : Map<'a, 'b Set>) (m2 : Map<'a, 'b Set>) f =
        m1 |> f <| m2

    (* Takes an atomic action and finds the identifer the action is done on e.g. x in x = a *)
    let grabIdentifier (act : Action) = 
        match act with
        | S(s) -> match s with
                  | Ass (l, _)    -> labelString l
                  | RAss (l, _, _) -> labelString l
                  | Read l        -> labelString l
                  | Write _       -> "Ø"
        | D(d) -> match d with
                  | DInt s        -> s
                  | DRecord s     -> s
                  | DArray (_, s) -> s     
        | B(b) -> prettyBExp b

    (* Addition: Takes two signs and maps to a set of signs *)
    let addSignsMap = (Map.empty
                        ).Add( (Negative, Negative), Set.empty.Add(Negative)
                        ).Add( (Negative,Zero), Set.empty.Add(Negative)
                        ).Add( (Negative,Positive), Set.empty.Add(Negative).Add(Zero).Add(Positive)
                        ).Add( (Zero,Negative), Set.empty.Add(Negative)
                        ).Add( (Zero,Zero), Set.empty.Add(Zero)
                        ).Add( (Zero,Positive), Set.empty.Add(Positive)
                        ).Add( (Positive,Negative), Set.empty.Add(Negative).Add(Zero).Add(Positive)
                        ).Add( (Positive,Zero), Set.empty.Add(Positive)
                        ).Add( (Positive,Positive), Set.empty.Add(Positive))

    (* Subtraction: Takes two signs and maps to a set of signs *)
    let subSignsMap = (Map.empty
                        ).Add( (Negative, Negative), Set.empty.Add(Negative).Add(Zero).Add(Positive)
                        ).Add( (Negative,Zero), Set.empty.Add(Negative)
                        ).Add( (Negative,Positive), Set.empty.Add(Negative)
                        ).Add( (Zero,Negative), Set.empty.Add(Positive)
                        ).Add( (Zero,Zero), Set.empty.Add(Zero)
                        ).Add( (Zero,Positive), Set.empty.Add(Negative)
                        ).Add( (Positive,Negative), Set.empty.Add(Positive)
                        ).Add( (Positive,Zero), Set.empty.Add(Positive)
                        ).Add( (Positive,Positive), Set.empty.Add(Negative).Add(Zero).Add(Positive))

    (* Multiplication: Takes two signs and maps to a set of signs *)
    let mulSignsMap = (Map.empty
                        ).Add( (Negative, Negative), Set.empty.Add(Positive)
                        ).Add( (Negative,Zero), Set.empty.Add(Zero)
                        ).Add( (Negative,Positive), Set.empty.Add(Negative)
                        ).Add( (Zero,Negative), Set.empty.Add(Zero)
                        ).Add( (Zero,Zero), Set.empty.Add(Zero)
                        ).Add( (Zero,Positive), Set.empty.Add(Zero)
                        ).Add( (Positive,Negative), Set.empty.Add(Negative)
                        ).Add( (Positive,Zero), Set.empty.Add(Zero)
                        ).Add( (Positive,Positive), Set.empty.Add(Positive)
                       )

    (* Division: Takes two signs and maps to a set of signs *)
    let divSignsMap = (Map.empty
                        ).Add( (Negative, Negative), Set.empty.Add(Positive)
                        ).Add( (Negative,Zero), Set.empty
                        ).Add( (Negative,Positive), Set.empty.Add(Negative)
                        ).Add( (Zero,Negative), Set.empty
                        ).Add( (Zero,Zero), Set.empty
                        ).Add( (Zero,Positive), Set.empty
                        ).Add( (Positive,Negative), Set.empty.Add(Negative)
                        ).Add( (Positive,Zero), Set.empty
                        ).Add( (Positive,Positive), Set.empty.Add(Positive))

    (* Modulos: Takes two signs and maps to a set of signs *)
    let modSignsMap = (Map.empty
                        ).Add( (Negative, Negative), Set.empty.Add(Negative).Add(Zero)
                        ).Add( (Negative,Zero), Set.empty
                        ).Add( (Negative,Positive), Set.empty.Add(Zero).Add(Positive)
                        ).Add( (Zero,Negative), Set.empty
                        ).Add( (Zero,Zero), Set.empty
                        ).Add( (Zero,Positive), Set.empty
                        ).Add( (Positive,Negative), Set.empty.Add(Zero).Add(Positive)
                        ).Add( (Positive,Zero), Set.empty
                        ).Add( (Positive,Positive), Set.empty.Add(Positive))

    (* Signs of operators *)
    let signsOpA (s1 : Signs) (opa : OpA) (s2 : Signs) : Signs Set =
        match opa with
        | Add -> Map.find (s1, s2) addSignsMap
        | Sub -> Map.find (s1, s2) subSignsMap
        | Mul -> Map.find (s1, s2) mulSignsMap
        | Div -> Map.find (s1, s2) divSignsMap
        | Mod -> Map.find (s1, s2) modSignsMap

    (* Signs of Arithmetic Expressions *) 
    let rec signsOfAExp (exp : AExp) (m : Map<string, (Signs) Set>) : Signs Set =
        match exp with
        | Int n -> if n < 0 then Set.empty.Add(Negative) 
                   elif n.Equals(0) then Set.empty.Add(Zero)
                   else Set.empty.Add(Positive)
        | Var v -> match v with
                   | Fst s| Snd s| Id s  -> let t = Map.find s m
                                            t
                   | ArId (s, _) -> Map.find s m
        | AOp (a1, opa, a2) -> 
                   let s1 = (signsOfAExp a1 m)
                   let s2 = (signsOfAExp a2 m)
                   let ret = (s1 |> Set.fold(fun a e ->
                         Set.union a (s2 |> Set.fold (fun b x ->
                             Set.union b (signsOpA e opa x)
                            ) Set.empty)
                        ) Set.empty)
                   ret

    (* Transfer function for Detection of Signs *)                                    
    let tfDS (q : int) (q' : int, act : Action) (m : Map<string, (Signs) Set>)  : Map<string, (Signs) Set> =
        match act with
        | B(_) -> m
        | S(Write(_)) -> m
        | S(Ass(ArId (l, _), a)) -> mapPointwiseUnion m (Map.empty.Add(l, signsOfAExp a m))
        | S(Ass(l, a)) -> let x = signsOfAExp a m
                          m.Add(labelString l, x)

        | S(RAss(l, a1, a2)) -> m.Add(labelString l, Set.union (signsOfAExp a1 m) (signsOfAExp a2 m))
        | D d -> match d with
                 | DInt s  -> m.Add(s, signsOfAExp (Int 0) m)
                 | DRecord s -> m.Add(s, signsOfAExp (Int 0) m)
                 | DArray (_, s) -> m.Add(s, signsOfAExp (Int 0) m)
                 | _ -> m
        | _ -> m

    (* Transfer function for Reaching Definitions *)
    let tfRD (q : int) (q' : int, act : Action) (m : Map<string, (int * int) Set>)  : Map<string, (int * int) Set> =
        match act with
        | B(_) -> m
        | S(Write(_)) -> m
        | S(Ass(ArId (_), _)) -> mapPointwiseUnion m (Map.empty.Add(grabIdentifier act, Set.empty.Add(q, q'))) 
        | _ -> m.Add(grabIdentifier act, Set.empty.Add(q, q'))


    (* Init process: Initialises the analysis result based on a program graph and a bottom *)
    let analysisResultInit (pg : ProgramGraph) bottom : Map<int, Map<'a, 'b>> =
        pg |> Map.fold (fun x key _ ->
            x.Add(key, bottom)
        ) (Map.empty.Add(-1, bottom))

    (* Init process: Initialises the worklist based on the program graph *)
    let workListInit (pg : ProgramGraph) : int list =
        pg |> Map.fold(fun x key _ ->
                x @ [key]
            ) [-1]

    (* Solve process: The solve part of the worklist algorithm *)
    let rec workListSolve (pg : ProgramGraph) workList (analysisResult : Map<int, Map<string, 'a Set>>) tf concat = 
        match workList with
        | [] -> analysisResult
        | q::xs when q.Equals(-1) -> workListSolve pg xs analysisResult tf concat
        | q::xs ->
            let (analysisResult', workList') =
                Map.find q pg 
                |> List.fold (fun ((m : Map<_,_>), workList) (q', act) ->
                    let x1 = Map.find q m
                    let x2 = Map.find q' m 
                    if ( not ( mapIsSubseteq (tf q (q', act) x1) x2 ) ) 
                        then m.Add(q', (leastUpperBound x2 (tf q (q', act) x1 ) mapPointwiseUnion) ), concat workList q' 
                    else (m, xs) 
                ) (analysisResult, workList)
            workListSolve pg workList' analysisResult' tf concat

    (* The worklist algorithm: Connects the initial and solve process, returns the analysis result *)
    let workListAlgorithm (pg : ProgramGraph) bottom dhat tf concat =
        let analysisResult = (analysisResultInit <| pg <| bottom).Add(0, dhat)
        let workList = workListInit pg
        workListSolve pg workList analysisResult tf concat