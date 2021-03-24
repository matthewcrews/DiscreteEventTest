// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Desif
open Desif.Types
open Desif.Planning


[<EntryPoint>]
let main argv =
    printfn "Hello world!"
            
    let resources = 
        [for i in 1..2 -> Resource $"Resource:{i}"]
        |> Set
    
    let simplePlan =
        planner {
            let! a = allocateOneOf (resources)
            delay (TimeSpan 2.0)
            free a
        } |> Planning.create
    
    let g1 = Generator.create "G1" (Distribution.Constant 2.0) (PossibilityType.PlanArrival simplePlan)
    
    let m : Model = {
        Resources = Set resources
        Generators = Set [g1]
    }
    
    let maxTime = TimeStamp 10.0
    
    let r = Simulation.run maxTime m
    printfn "%A" (List.rev r.History)
    0 // return an integer exit code