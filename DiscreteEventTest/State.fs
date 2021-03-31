/// Functions for transforming the state. They also track the History
module Desif.State

open Desif.Types

type State = private {
    Now : TimeStamp
    LastFactId : FactId
    LastPossibilityId : PossibilityId
    LastProcedureId : ProcedureId
    LastInstantId : InstantId
    FreeResources : Set<Resource>
    DownResources : Set<Resource>
    Allocations : Map<ProcedureId * AllocationId, Set<Resource>>
    Assignments : Map<Resource, ProcedureId * AllocationId>
    Procedures : Map<ProcedureId, Procedure>
    Instants : Instant list
    Possibilities : Possibility list
    OpenRequests : Set<AllocationRequest>
    History : Fact list
}


let initial =
    {
        Now = TimeStamp.zero
        LastFactId = FactId 0L
        LastPossibilityId = PossibilityId 0L
        LastProcedureId = ProcedureId 0L
        LastInstantId = InstantId 0L
        FreeResources = Set.empty
        DownResources = Set.empty
        Allocations = Map.empty
        Assignments = Map.empty
        Procedures = Map.empty
        Instants = []
        Possibilities = []
        OpenRequests = Set.empty
        History = []
    }


let setNow (now: TimeStamp) (state: State) =
    { state with Now = now }


let addFact (factType: FactType) (state: State) =
    let nextFactId = FactId.next state.LastFactId
    let fact = Fact.create nextFactId state.Now factType
    { state with
        LastFactId = nextFactId
        History = fact::state.History
    }


let enqueuePossibility (delay: Interval) (possibilityType: PossibilityType) (state: State) =
    let nextPossibilityId = PossibilityId.next state.LastPossibilityId
    let newPossibility = Possibility.create nextPossibilityId (state.Now + delay) possibilityType
    { state with
        LastPossibilityId = nextPossibilityId
        Possibilities = newPossibility::state.Possibilities
    }
    // Note: We do not addFact here because this may or may not happen.
    // Facts are only things that HAVE happened


let addResource (resource: Resource) (state: State) =
    { state with
        FreeResources = Set.add resource state.FreeResources
    }


let addResources resources state : State =
    (resources, state)
    ||> Seq.foldBack addResource


let private addScheduledEvent (scheduledEvent: ScheduledEvent) (state: State) =
    match scheduledEvent with
    | StartPlan (plan, arrivalTimeStamp) ->
        enqueuePossibility (arrivalTimeStamp - TimeStamp.zero) (PossibilityType.PlanArrival plan) state


let addSchedule (Schedule schedule) (state: State) =
            
    (schedule, state)
    ||> List.foldBack addScheduledEvent
    

let generatePossibilities (maxTime: TimeStamp) (generators: seq<Generator>) (modelState: State) : State =

    let rec add (lastTime: TimeStamp) (maxTime: TimeStamp) (generator: Generator) (state: State) =
        let nextTimeSpan = 
            // Yes, this is terrible. It will be refactored
            Distribution.sample generator.Distribution
            |> int
            |> fun x -> System.TimeSpan (0, x, 0)
            |> Interval
        let nextTime = lastTime + nextTimeSpan
        if nextTime > maxTime then
            state
        else
            state
            |> enqueuePossibility (nextTime - TimeStamp.zero) generator.PossibilityType
            |> add nextTime maxTime generator

    let modelState =
        (generators, modelState)
        ||> Seq.foldBack (add TimeStamp.zero maxTime)

    modelState


let initialize (maxTime: TimeStamp) (model: Model) =
        
    initial
    |> addResources (model.Resources)
    |> generatePossibilities maxTime model.Generators
    |> addSchedule model.Schedule


let popPossibility (state: State) =
    // TODO: Better data structure
    let sortedPossibilities = 
        state.Possibilities
        |> List.sortBy (fun x -> x.ArrivalTimeStamp, x.PossibilityId)

    match sortedPossibilities with
    | [] -> None
    | next::remaining -> 
        let newState = { state with Possibilities = remaining }
        Some (next, newState)


let popInstant (state: State) =
    match state.Instants with
    | [] -> None
    | next::remaining -> 
        let newState = { state with Instants = remaining }
        Some (next, newState)


//let private setProcedure (procedure: Procedure) (state: State) =
//    { state with Procedures = Map.add procedure.ProcedureId procedure state.Procedures }


let enqueueInstant instantType (state: State) =
    let nextInstantId = InstantId.next state.LastInstantId
    let nextInstant = Instant.create nextInstantId instantType
    { state with 
        LastInstantId = nextInstantId
        Instants = nextInstant::state.Instants
    }


let addAllocationRequest (allocationRequest: AllocationRequest) (state: State) =
    { state with OpenRequests = Set.add allocationRequest state.OpenRequests }
    |> addFact (FactType.allocationRequestAdded allocationRequest)


let removeAllocationRequest (allocationRequest: AllocationRequest) (state: State) =
  { state with OpenRequests = Set.remove allocationRequest state.OpenRequests }
  |> addFact (FactType.allocationRequestRemoved allocationRequest)


let addAllocation procedureId (allocation: Allocation) (state: State) =
    let newFreeResources = state.FreeResources - allocation.Resources
    let newAllocations = Map.add (procedureId, allocation.AllocationId) allocation.Resources state.Allocations
    let newAssignments =
        (state.Assignments, allocation.Resources)
        ||> Seq.fold (fun s r -> Map.add r (procedureId, allocation.AllocationId) s)
    { state with
        FreeResources = newFreeResources
        Allocations = newAllocations
        Assignments = newAssignments
    }
    |> addFact (FactType.allocationAdded procedureId allocation.AllocationId allocation.Resources)


let removeAllocation procedureId allocationId (state: State) =
    let removed = state.Allocations.[procedureId, allocationId]
    { state with
        Allocations = Map.remove (procedureId, allocationId) state.Allocations
    }
    |> addFact (FactType.allocationRemoved procedureId allocationId removed)


