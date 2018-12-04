#load "Types.fsx"
#load "Programgraph.fsx"
#load "Analysis.fsx"

open ProgramAnalysis.Types
open ProgramAnalysis.Programgraph
open ProgramAnalysis.Analysis
open System

[<EntryPoint>]
let main argv =
      let inputProgram = (DD(DInt("x"), DInt("y")), Seq(Ass(Id "y", Int 1), 
                            While(Neg(ROp(Var (Id "y"), Eq, Var (Id "x"))), 
                             Seq(Ass(Id "y", AOp(Var (Id "x"), Add, Var (Id "y"))) , Ass(Id "x", AOp(Var (Id "x"), Sub, Int 1)) ))
                          ))

      let dg = declGraph 0 (amountOfDecls (fst inputProgram)) (fst inputProgram) (Map.ofList[])
      let sg = stmGraph (amountOfDecls (fst inputProgram)) -1 (snd inputProgram) dg 
      let pg = mapMerge dg sg

      let bottom = Map.empty

      let dhatDS = Map.empty.Add("x", Set.empty.Add(Zero)).Add("y", Set.empty.Add(Zero))
      let dhatRD = bottom

      let finalResultRD = workListAlgorithm pg bottom dhatRD tfRD lifo
      let finalResultDS = workListAlgorithm pg bottom dhatDS tfDS lifo

      let printResult res =
          res |> Map.iter(fun key value ->
                printfn "%A: %A" key value
            )

      printfn "Input program:"
      printfn "%A" (inputProgram |> prettyAST)

      printfn ""
      printfn "Reaching Definitions:"
      printResult finalResultRD

      printfn ""
      printfn "Detection of Signs:"
      printResult finalResultDS

      0