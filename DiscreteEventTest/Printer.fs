module Printer

// The Spectre.Console API heavily uses mutation and I didn't want to
// have to pipe into `ignore` constantly
#nowarn "0020"

open Desif.Types
open Spectre.Console


let history (state: State) =

    let table = Table ()

    table.AddColumn("FactId")
    table.AddColumn("TimeStamp")
    table.AddColumn("Procedure")
    table.AddColumn("FactType")
    table.AddColumn("Step")

    let facts = List.rev state.History

    for fact in facts do
        
        match fact.FactType with
        | FactType.AllocationAdded (p, a, r) -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "AllocationAdded"; ""|])
        | FactType.AllocationRemoved (p, a, r) -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "AllocationRemoved"; ""|])
        | FactType.AllocationRequestAdded a -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); a.ProcedureId.ToString(); "AllocationRequestAdded"; ""|])
        | FactType.AllocationRequestRemoved a -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); a.ProcedureId.ToString(); "AllocationRequestRemoved"; ""|])
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
            | StepType.FreeAllocation _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepStarted"; "FreeAllocation" |])
            | StepType.Fail _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepStarted"; "Fail" |])
            | StepType.Restore _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepStarted"; "Restore" |])
        | FactType.StepCompleted (p, s, step) -> 
            match step.StepType with
            | StepType.Allocate _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepCompleted"; "Allocate" |])
            | StepType.Delay _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepCompleted"; "Delay" |])
            | StepType.FreeAllocation _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepCompleted"; "Free" |])
            | StepType.Fail _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepCompleted"; "Fail" |])
            | StepType.Restore _ -> table.AddRow([|fact.FactId.ToString(); fact.TimeStamp.ToString(); p.ToString(); "StepCompleted"; "Restore" |])

    AnsiConsole.Render(table)