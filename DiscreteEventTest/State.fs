/// Functions for transforming the state. They also track the History
[<RequireQualifiedAccess>]
module Desif.State

open Desif.Types

let initial =
    {
        Now = TimeStamp 0.0
        LastFactId = FactId 0L
        LastPossibilityId = PossibilityId 0L
        LastProcedureId = ProcedureId 0L
        LastInstantId = InstantId 0L
        FreeResources = Set.empty
        DownResources = Set.empty
        Allocations = Map.empty
        Assignments = Map.empty
        Procedures = Map.empty
        Instants = Set.empty
        Possibilities = Set.empty
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


let addPossibility (delay: TimeSpan) (possibilityType: PossibilityType) (state: State) =
    let nextPossibilityId = PossibilityId.next state.LastPossibilityId
    let possibility = Possibility.create nextPossibilityId (state.Now + delay) possibilityType
    { state with
        LastPossibilityId = nextPossibilityId
        Possibilities = Set.add possibility state.Possibilities
    }
    // Note: We do not addFact here because this may or may not happen.
    // Facts are only things that HAVE happened


let addResource (resource: Resource) (state: State) =
    { state with
        FreeResources = Set.add resource state.FreeResources
    }

module private Initializers =

    // This is a separate version for initialization since the State object is not moving forward
    // in time when initializing
    let private addPossibility (timeStamp: TimeStamp) (possibilityType: PossibilityType) (state: State) =
        let nextPossibilityId = PossibilityId.next state.LastPossibilityId
        let newPossibility = Possibility.create nextPossibilityId timeStamp possibilityType
        { state with 
            LastPossibilityId = nextPossibilityId
            Possibilities = Set.add newPossibility state.Possibilities
        }


    let addPossibilities (maxTime: TimeStamp) (generators: seq<Generator>) (modelState: State) : State =

        let rec add (lastTime: TimeStamp) (maxTime: TimeStamp) (generator: Generator) (state: State) =
            let nextTimeSpan = Distribution.sample generator.Distribution |> TimeSpan
            let nextTime = lastTime + nextTimeSpan
            if nextTime > maxTime then
                state
            else
                state
                |> addPossibility nextTime generator.PossibilityType
                |> add nextTime maxTime generator

        let modelState =
            (modelState, generators)
            ||> Seq.foldR (add TimeStamp.zero maxTime)

        modelState


    let addResources resources state : State =
        
        (state, resources)
        ||> Seq.foldR addResource


    let private addScheduledEvent (scheduledEvent: ScheduledEvent) (state: State) =
        match scheduledEvent with
        | StartPlan (plan, arrivalTimeStamp) ->
            addPossibility arrivalTimeStamp (PossibilityType.PlanArrival plan) state

    let addSchedule (Schedule schedule) (state: State) =
            
        (state, schedule)
        ||> List.foldR addScheduledEvent


let initialize (maxTime: TimeStamp) (model: Model) =
        
    initial
    |> Initializers.addResources (model.Resources)
    |> Initializers.addPossibilities maxTime model.Generators
    |> Initializers.addSchedule model.Schedule


let nextPossibility (modelState: State) =
    match modelState.Possibilities.IsEmpty with
    | true -> None
    | false -> 
        modelState.Possibilities
        // TODO: Better data structure
        |> Seq.sortBy (fun x -> x.ArrivalTimeStamp, x.PossibilityId)
        |> Seq.head
        |> Some


let nextInstant (state: State) =
    match state.Instants.IsEmpty with
    | true -> None
    | false ->
        state.Instants
        // TODO: Better data structure
        |> Seq.sortBy (fun x -> x.InstantId)
        |> Seq.head
        |> Some


let private setProcedure (procedure: Procedure) (state: State) =
    { state with Procedures = Map.add procedure.ProcedureId procedure state.Procedures }


let addInstant instantType (state: State) =
    let nextInstantId = InstantId.next state.LastInstantId
    let nextInstant = Instant.create nextInstantId instantType
    { state with 
        LastInstantId = nextInstantId
        Instants = Set.add nextInstant state.Instants
    }


let removeInstant (i: Instant) (state: State) =
    { state with Instants = Set.remove i state.Instants }


let addAllocationRequest (a: AllocationRequest) (state: State) =
    { state with OpenRequests = Set.add a state.OpenRequests }
    |> addFact (FactType.allocationRequestAdded a)


let removeAllocationRequest (a: AllocationRequest) (state: State) =
  { state with OpenRequests = Set.remove a state.OpenRequests }
  |> addFact (FactType.allocationRequestRemoved a)


let removePossibility (p: Possibility) (state: State) =
    { state with Possibilities = Set.remove p state.Possibilities }


let startProcedure plan (state: State) =
    let nextProcedureId = ProcedureId.next state.LastProcedureId
    let p = Procedure.ofPlan nextProcedureId plan
    { state with
        LastProcedureId = nextProcedureId
        Procedures = Map.add nextProcedureId p state.Procedures
    }
    |> addFact (FactType.procedureStarted nextProcedureId)
    |> addInstant (InstantType.proceed nextProcedureId)


let addAllocation procedureId (a: Allocation) (state: State) =
    let newFreeResources = state.FreeResources - a.Resources
    let newAllocations = Map.add (procedureId, a.AllocationId) a.Resources state.Allocations
    let newAssignments =
        (state.Assignments, a.Resources)
        ||> Seq.fold (fun s r -> Map.add r (procedureId, a.AllocationId) s)
    { state with
        FreeResources = newFreeResources
        Allocations = newAllocations
        Assignments = newAssignments
    }
    |> addFact (FactType.allocationAdded procedureId a.AllocationId a.Resources)


let removeAllocation procedureId allocationId (state: State) =
    let removed = state.Allocations.[procedureId, allocationId]
    { state with
        Allocations = Map.remove (procedureId, allocationId) state.Allocations
    }
    |> addFact (FactType.allocationRemoved procedureId allocationId removed)


let freeAllocation (procedureId: ProcedureId) (allocationId: AllocationId) (state: State) =
    let resources = state.Allocations.[procedureId, allocationId]
    { state with
        FreeResources = resources + state.FreeResources
        Allocations = Map.remove (procedureId, allocationId) state.Allocations
    }
    |> addFact (FactType.freed procedureId allocationId resources)
    |> addInstant (InstantType.proceed procedureId)


let private finishPreviousStep (procedureState: Procedure) (state: State) =
    match procedureState.Processed with
    | [] -> procedureState, state
    | last::previous ->
        state
        |> addFact (FactType.stepCompleted procedureState.ProcedureId procedureState.StateId last)
        |> (fun x -> procedureState, x)


module private ProcessStep =

    let allocate allocationId quantity resources procedure (state: State) =
        let allocation = Allocation.create allocationId quantity resources
        let request = AllocationRequest.create state.Now procedure allocation
        addAllocationRequest request state

    let delay timeSpan (next: Step) (procedure: Procedure) (state: State) =
        let nextStateId = StateId.next procedure.StateId
        let completion =
            {
                ProcedureId = procedure.ProcedureId
                StateId = nextStateId
                StepId = next.StepId
                CompletionTimeStamp = state.Now + timeSpan
            }
        let newProcedure = 
            { procedure with 
                StateId = nextStateId
                ProcedureState = ProcedureState.waiting completion 
            }
        state
        |> setProcedure newProcedure
        |> addPossibility timeSpan (PossibilityType.Completion completion)
    


let private processStep (next: Step) (procedure: Procedure) (state: State) =
    state |>
    match next.StepType with
    | StepType.Allocate (allocationId, quantity, resources) ->
        ProcessStep.allocate allocationId quantity resources procedure
    | StepType.Delay timeSpan ->
        ProcessStep.delay timeSpan next procedure
    | StepType.FreeAllocation allocationId ->
        addInstant (InstantType.free procedure.ProcedureId allocationId)
    | StepType.Fail resource ->
        addPossibility TimeSpan.zero (PossibilityType.failure procedure.ProcedureId resource)
    | StepType.Restore resource ->
        addInstant (InstantType.restore resource)
        

let private startNextStep (procedureState: Procedure) (state: State) =
    match procedureState.Pending with
    | [] ->
        state
        |> addFact (FactType.procedureCompleted procedureState.ProcedureId)
    | nextStep::remainingSteps ->
      let nextStateId = StateId.next procedureState.StateId
      let newProcedureState = 
          Procedure.create procedureState.ProcedureId nextStateId remainingSteps (nextStep::remainingSteps) (ProcedureState.Running)

      state
      |> setProcedure newProcedureState
      |> addFact (FactType.stepStarted procedureState.ProcedureId procedureState.StateId nextStep)
      |> processStep nextStep newProcedureState


let proceed (procedureId: ProcedureId) (state: State) =
    let procedureState = state.Procedures.[procedureId]

    (procedureState, state)
    ||> finishPreviousStep
    ||> startNextStep


let private addHandleFailure resource state =
    match Map.tryFind resource state.Assignments with
    | Some (procedureId, allocationId) ->
        state
        |> addInstant (InstantType.handleFailure resource procedureId allocationId)
    | None ->
        state


let private addHandleRestore resource state =
    match Map.tryFind resource state.Assignments with
    | Some (procedureId, allocationId) ->
        state
        |> addInstant (InstantType.handleRestore resource procedureId allocationId)
    | None ->
        state


let private addResourceToDown (resource: Resource) (state: State) =
  let newDownResources = Set.add resource state.DownResources
  { state with DownResources = newDownResources }


let private removeResourceFromDown (resource: Resource) (state: State) =
  let newDownResources = Set.remove resource state.DownResources
  { state with DownResources = newDownResources }


let failResource (procedureId: ProcedureId) (resource: Resource) (state: State) =
    state
    |> addResourceToDown resource
    |> addFact (FactType.failed resource)
    |> addHandleFailure resource
    |> proceed procedureId


let restoreResource (resource: Resource) (state: State) =
    state
    |> removeResourceFromDown resource
    |> addFact (FactType.restored resource)
    |> addHandleRestore resource
        

let handleFailure resource procedureId allocationId state =
    let procedure = state.Procedures.[procedureId]

    // NOTE: In the future we want more complex failure handling. For now
    // we are just going to have Pause/Resume behavior

    match procedure.ProcedureState with
    | ProcedureState.Running ->
        // Kind of strange if this happened ?
        failwith "Probably shouldn't happen :/"
        state
    | ProcedureState.WaitingFor possibility ->
        let nextStateId = StateId.next procedure.StateId
        let newProcedureState = ProcedureState.Suspended (state.Now, possibility, Set [resource])
        let newProcedure = 
            { procedure with
                StateId = nextStateId
                ProcedureState = newProcedureState 
            }
        setProcedure newProcedure state

    | ProcedureState.Suspended (suspendedAt, waitingFor, suspendedFor) ->
        let newProcedureState = ProcedureState.Suspended (suspendedAt, waitingFor, Set.add resource suspendedFor)
        let nextStateId = StateId.next procedure.StateId
        let newProcedure = 
            { procedure with
                StateId = nextStateId
                ProcedureState = newProcedureState 
            }
        setProcedure newProcedure state

module Possibility =

    let private whenRunning state =
        state

    let private whenWaitingFor state =
        state

    let private whenSuspendedButCanResume suspendedAt (procedure: Procedure) (completion: Completion) (state: State) =
        let newStateId = StateId.next procedure.StateId
        let newDelay = (completion.CompletionTimeStamp - suspendedAt)
        let newCompletion = 
            { completion with 
                CompletionTimeStamp = state.Now + newDelay
                StateId = newStateId
            }
            
        let newProcedure = 
            { procedure with
                ProcedureState = ProcedureState.running
                StateId = newStateId
            }
            
        state
        |> setProcedure newProcedure // TODO: Clean up this wording
        |> addPossibility newDelay (PossibilityType.Completion newCompletion)


    let private whenSuspendedButCannotResume suspendedAt completion newSuspendedFor procedure state =
        let newProcedure = 
            { procedure with
                ProcedureState = ProcedureState.Suspended (suspendedAt, completion, newSuspendedFor)
            }
        setProcedure newProcedure state // TODO: Clean up this wording


    let private whenSuspended (resource: Resource) (procedure: Procedure) suspendedAt completion suspendedFor (state: State) =
        let newSuspendedFor = Set.remove resource suspendedFor
            
        state |>
        match Set.isEmpty newSuspendedFor with
        | true -> whenSuspendedButCanResume suspendedAt procedure completion
        | false -> whenSuspendedButCannotResume suspendedAt completion newSuspendedFor procedure
                

    let apply resource procedureId allocationId state =
        let procedure = state.Procedures.[procedureId]

        state |>
        match procedure.ProcedureState with
        | ProcedureState.Running ->
            whenRunning
        | ProcedureState.WaitingFor _ ->
            // The procedure likely no longer needs the resource that was freed
            whenWaitingFor
        | ProcedureState.Suspended (suspendedAt, completion, suspendedFor) ->
            whenSuspended resource procedure suspendedAt completion suspendedFor
                

let processCompletion (completion: Completion) (state: State) =
    let p = state.Procedures.[completion.ProcedureId]

    if p.StateId = completion.StateId then
        addInstant (InstantType.proceed completion.ProcedureId) state
    else
        state
