namespace ProgramAnalysis

#load "Types.fsx"
open Types

module Programgraph =
    let addEdge (fromState: int) (toState: int) (tag: Action) (graph: ProgramGraph): ProgramGraph =
        match Map.tryFind fromState graph with
        | Some edgeList ->
            (toState, tag) :: edgeList
            |> Map.add fromState <| graph
        | None ->
            graph
            |> Map.add fromState [(toState, tag)]

    let rec stmGraph (startEdge: int) (finalEdge: int) (stm: Stm) (graph: ProgramGraph): ProgramGraph =
        match stm with
        | Ass _ ->
            graph
            |> addEdge startEdge finalEdge (S <| stm)
        | RAss _ ->
            graph
            |> addEdge startEdge finalEdge (S <| stm)
        | Skip ->
            graph
            |> addEdge startEdge finalEdge (S <| stm)
        | Seq (s1, s2) ->
            let q1 = graph.Count + 1
            graph 
            |> stmGraph startEdge q1 s1
            |> stmGraph q1 finalEdge s2
        | ITE (b, s1, s2) ->
            let q1 = graph.Count + 1
            let q2 = graph.Count + 2
            graph 
            |> addEdge startEdge q1 (B <| b)
            |> addEdge startEdge q2 (B <|(Neg (b)))
            |> stmGraph q1 finalEdge s1
            |> stmGraph q2 finalEdge s2
        | IT (b, s) ->
            let q1 = graph.Count + 1
            graph 
            |> addEdge startEdge q1 (B <| b)
            |> addEdge startEdge finalEdge (B <| Neg b)
            |> stmGraph q1 finalEdge s
        | While (b, s) ->
            let q1 = graph.Count + 1
            graph 
            |> addEdge startEdge q1 (B <| b)
            |> addEdge startEdge finalEdge (B <| Neg(b))
            |> stmGraph q1 startEdge s
        | Read _ -> 
            graph
            |> addEdge startEdge finalEdge (S <| stm)
        | Write _ ->
            graph
            |> addEdge startEdge finalEdge (S <| stm) 

    let rec declGraph (startEdge: int) (finalEdge: int) (decl: Decl) (graph: ProgramGraph): ProgramGraph =
        match decl with
        | DInt _ ->
            graph 
            |> addEdge startEdge finalEdge (D <| decl)
        | DRecord _ ->
            graph 
            |> addEdge startEdge finalEdge (D <| decl)
        | DArray _ ->
            graph 
            |> addEdge startEdge finalEdge (D <| decl)
        | DD (d1, d2) ->
            let q1 = graph.Count + 1
            graph 
            |> declGraph startEdge q1 d1
            |> declGraph q1 finalEdge d2
        | DEmpty _ -> 
            graph

    let rec amountOfDecls = function
        | DEmpty -> 0
        | DD (d1, d2) -> amountOfDecls d1 + amountOfDecls d2
        | _ -> 1

    let mapMerge m1 m2 = 
        Map.fold (fun s k v -> Map.add k v s) m1 m2