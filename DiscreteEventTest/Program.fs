﻿// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Daws
open Daws.Domain
open Daws.Planning
    


[<EntryPoint>]
let main argv =
    printfn "Hello world!"
            
    let resources = 
        [for i in 1..5 -> Resource $"Resource:{i}"]
        |> Set
    
    let simplePlan =
        planner {
            let! a = allocateOneOf (resources)
            delay (TimeSpan 2.0)
            free a
            let! a2 = allocateOneOf resources
            delay (TimeSpan 1.0)
            free a2
        } |> Planning.create
    
    let g1 = Generator.create "G1" (Distribution.Constant 2.0) (PossibilityType.PlanArrival simplePlan)
    
    let m : Model = {
        Resources = Set resources
        Generators = Set [g1]
    }
    
    let maxTime = TimeStamp 10.0
    
    let ms = ModelState.initialize maxTime m
    let r = Simulation.run maxTime ms

    0 // return an integer exit code