#load "Types.fsx"
#load "Programgraph.fsx"
#load "Analysis.fsx"

open ProgramAnalysis.Types
open ProgramAnalysis.Programgraph
open ProgramAnalysis.Analysis

let fac = (DD(DInt("x"), DInt("y")), Seq(Ass(Id "y", Int 1), 
              While(Neg(ROp(Var (Id "y"), Eq, Var (Id "x"))), 
                    Seq(Ass(Id "y", AOp(Var (Id "x"), Mul, Var (Id "y"))) , Ass(Id "x", AOp(Var (Id "x"), Sub, Int 1)) ))
             ))

let ifthen = (DD(DInt("x"), DInt("y")), IT(ROp(Var(Id "x"), Eq, Int 0), Ass(Id "x", Int 1)))

let dg = declGraph 0 (amountOfDecls (fst fac)) (fst fac) (Map.ofList[])
let sg = stmGraph (amountOfDecls (fst fac)) -1 (snd fac) dg 

let pg = mapMerge dg sg

//let a = workListInit pg
//let b = analysisResultInit <| pg <| Map.empty


let analysisResult = analysisResultInit pg Map.empty
let workList = workListInit pg
let s = workListSolve pg workList analysisResult tfRD
//let b = workListAlgorithm <| pg <| [("Ø", 0, 0)] <| "dhat" <| tfRD