module Printer

open Desif.Types
open Spectre.Console

let history (state: State) =

    let table = Table ()

    table.AddColumn("FactId")     |> ignore
    table.AddColumn("TimeStamp")  |> ignore
    table.AddColumn("FactType")   |> ignore
    table.AddColumn("Procedure")  |> ignore
    table.AddColumn("Step")  |> ignore

    let facts = List.rev state.History

    for fact in facts do
        
        match fact.FactType with
        | FactType.Allocated (p, a, r) -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); "Allocated"; p.ToString(); ""|])
        | FactType.AllocationRequested a -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); "AllocatedRequested"; a.ProcedureId.ToString(); ""|])
        | FactType.Freed (p, a, r) -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); "Freed"; p.ToString(); ""|])
        | FactType.StepStarted (p, s, step) -> 
            match step.StepType with
            | StepType.Allocate _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); "StepStarted"; p.ToString(); "Allocate" |])
            | StepType.Delay _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); "StepStarted"; p.ToString(); "Delay" |])
            | StepType.Free _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); "StepStarted"; p.ToString(); "Free" |])
        | FactType.StepCompleted (p, s, step) -> 
            match step.StepType with
            | StepType.Allocate _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); "StepCompleted"; p.ToString(); "Allocate" |])
            | StepType.Delay _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); "StepCompleted"; p.ToString(); "Delay" |])
            | StepType.Free _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); "StepCompleted"; p.ToString(); "Free" |])
        | FactType.ProcedureStarted p -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); "ProcedureStarted"; p.ToString(); ""|])
        | FactType.ProcedureCompleted p -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); "ProcedureCompleted"; p.ToString(); ""|])
        |> ignore


    AnsiConsole.Render(table)