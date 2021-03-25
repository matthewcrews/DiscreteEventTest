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
        | FactType.ProcedureStarted p -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "ProcedureStarted"; ""|])
        | FactType.ProcedureCompleted p -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "ProcedureCompleted"; ""|])
        | FactType.Failed f -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); ""; "Failed"; ""|])
        | FactType.Restored r -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); ""; "Restored"; ""|])
        | FactType.Resumed p -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "Restored"; ""|])
        | FactType.Suspended p -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "Restored"; ""|])
        | FactType.StepStarted (p, s, step) -> 
            match step.StepType with
            | StepType.Allocate _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepStarted"; "Allocate" |])
            | StepType.Delay _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepStarted"; "Delay" |])
            | StepType.Free _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepStarted"; "Free" |])
            | StepType.Fail _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepStarted"; "Fail" |])
            | StepType.Restore _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepStarted"; "Restore" |])
        | FactType.StepCompleted (p, s, step) -> 
            match step.StepType with
            | StepType.Allocate _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepCompleted"; "Allocate" |])
            | StepType.Delay _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepCompleted"; "Delay" |])
            | StepType.Free _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepCompleted"; "Free" |])
            | StepType.Fail _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepCompleted"; "Fail" |])
            | StepType.Restore _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepCompleted"; "Restore" |])
        |> ignore


    !! AnsiConsole.Render(table)