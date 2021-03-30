open System
open Desif
open Desif.Types
open Desif.Planning


[<EntryPoint>]
let main argv =
    printfn "Hello world!"
            
    let resources = 
        [for i in 1..1 -> Resource $"Resource:{i}"]
        |> Set
    
    let r1 = resources.MinimumElement

    let simplePlan =
        planner {
            let! a = allocateOneOf (resources)
            delay (TimeSpan 4.0)
            free a
        } |> Planning.create
    
    let failurePlan =
        planner {
            fail r1
            delay (TimeSpan 3.0)
            restore r1
        } |> Planning.create

    //let g1 = Generator.create "G1" (Distribution.Constant 2.0) (PossibilityType.PlanArrival simplePlan)
    
    let m : Model = {
        Resources = Set resources
        Generators = Set.empty
        Schedule = 
            Schedule [
                ScheduledEvent.StartPlan (simplePlan, TimeStamp.zero)
                ScheduledEvent.StartPlan (failurePlan, (TimeStamp 2.0))
            ]
    }
    
    let maxTime = TimeStamp 10.0
    
    let r = Simulation.run maxTime m
    printfn "%A" (List.rev r.History)

    Printer.history r
    printfn "Press ENTER to close..."
    Console.ReadKey() |> ignore
    0 // return an integer exit code