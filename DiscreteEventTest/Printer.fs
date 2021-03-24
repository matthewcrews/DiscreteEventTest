module Printer

open Desif.Types
open Spectre.Console

// Shorthand for ignoring output
let inline private (!!) x = x |> ignore

let history (state: State) =

    let table = Table ()

    !! table.AddColumn("FactId")
    !! table.AddColumn("TimeStamp")
    !! table.AddColumn("Procedure")
    !! table.AddColumn("FactType")
    !! table.AddColumn("Step")

    let facts = List.rev state.History

    for fact in facts do
        
        match fact.FactType with
        | FactType.Allocated (p, a, r) -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "Allocated"; ""|])
        | FactType.AllocationRequested a -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); a.ProcedureId.ToString(); "AllocatedRequested"; ""|])
        | FactType.Freed (p, a, r) -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "Freed"; ""|])
        | FactType.StepStarted (p, s, step) -> 
            match step.StepType with
            | StepType.Allocate _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepStarted"; "Allocate" |])
            | StepType.Delay _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepStarted"; "Delay" |])
            | StepType.Free _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepStarted"; "Free" |])
        | FactType.StepCompleted (p, s, step) -> 
            match step.StepType with
            | StepType.Allocate _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepCompleted"; "Allocate" |])
            | StepType.Delay _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepCompleted"; "Delay" |])
            | StepType.Free _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepCompleted"; "Free" |])
        | FactType.ProcedureStarted p -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "ProcedureStarted"; ""|])
        | FactType.ProcedureCompleted p -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "ProcedureCompleted"; ""|])
        |> ignore


    !! AnsiConsole.Render(table)