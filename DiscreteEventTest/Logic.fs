[<RequireQualifiedAccess>]
module Desif.Logic

open Desif.Types


let freeAllocation (procedureId: ProcedureId) (allocationId: AllocationId) (state: State) =
    let resources = state.Allocations.[procedureId, allocationId]
    state
    |> State.addFreeResources resources
    |> State.removeAllocation procedureId allocationId
    |> State.removeAssignments resources
    |> State.addFact (FactType.freed procedureId allocationId resources)
    |> State.enqueueInstant (InstantType.proceed procedureId)


let startProcedure plan (state: State) =
    // TODO: This is bad. We're relying on the `addProcedure` call to udpate the lastProcedureId
    //let nextProcedureId = ProcedureId.next state.LastProcedureId
    let (newProcedure, newState) = State.createProcedure plan state
    newState
    |> State.addProcedure newProcedure
    |> State.addFact (FactType.procedureStarted newProcedure.ProcedureId)
    |> State.enqueueInstant (InstantType.proceed newProcedure.ProcedureId)


let private finishPreviousStep (procedureState: Procedure) (state: State) =
    match procedureState.Processed with
    | [] -> procedureState, state
    | last::previous ->
        state
        |> State.addFact (FactType.stepCompleted procedureState.ProcedureId procedureState.StateId last)
        |> (fun x -> procedureState, x)


module private ProcessStep =

    let allocate allocationId quantity resources procedure (state: State) =
        let allocation = Allocation.create allocationId quantity resources
        let request = AllocationRequest.create state.Now procedure allocation
        State.addAllocationRequest request state

    let delay timeSpan (next: Step) (procedure: Procedure) (state: State) =
        let (newCompletion, newProcedure) = Completion.create (state.Now + timeSpan) next procedure

        state
        |> State.updateProcedure newProcedure
        |> State.enqueuePossibility timeSpan (PossibilityType.Completion newCompletion)
    


let private processStep (next: Step) (procedure: Procedure) (state: State) =
    state |>
    match next.StepType with
    | StepType.Allocate (allocationId, quantity, resources) ->
        ProcessStep.allocate allocationId quantity resources procedure
    | StepType.Delay timeSpan ->
        ProcessStep.delay timeSpan next procedure
    | StepType.FreeAllocation allocationId ->
        State.enqueueInstant (InstantType.free procedure.ProcedureId allocationId)
    | StepType.Fail resource ->
        //addPossibility TimeSpan.zero (PossibilityType.failure procedure.ProcedureId resource)
        State.enqueueInstant (InstantType.Failure (procedure.ProcedureId, resource))
    | StepType.Restore resource ->
        State.enqueueInstant (InstantType.restore resource)
        

let private startNextStep (procedureState: Procedure) (state: State) =
    match procedureState.Pending with
    | [] ->
        state
        |> State.addFact (FactType.procedureCompleted procedureState.ProcedureId)
    | nextStep::remainingSteps ->
      let nextStateId = StateId.next procedureState.StateId
      let newProcedureState = 
          Procedure.create procedureState.ProcedureId nextStateId remainingSteps (nextStep::remainingSteps) (ProcedureState.Running)

      state
      |> State.updateProcedure newProcedureState
      |> State.addFact (FactType.stepStarted procedureState.ProcedureId procedureState.StateId nextStep)
      |> processStep nextStep newProcedureState


let proceed (procedureId: ProcedureId) (state: State) =
    let procedureState = state.Procedures.[procedureId]

    (procedureState, state)
    ||> finishPreviousStep
    ||> startNextStep


let private addHandleFailure resource (state: State) =
    match Map.tryFind resource state.Assignments with
    | Some (procedureId, allocationId) ->
        state
        |> State.enqueueInstant (InstantType.handleFailure resource procedureId allocationId)
    | None ->
        state


let private addHandleRestore resource (state: State) =
    match Map.tryFind resource state.Assignments with
    | Some (procedureId, allocationId) ->
        state
        |> State.enqueueInstant (InstantType.handleRestore resource procedureId allocationId)
    | None ->
        state


let failResource (procedureId: ProcedureId) (resource: Resource) (state: State) =
    state
    |> State.addResourceToDown resource
    |> State.addFact (FactType.failed resource)
    |> proceed procedureId
    |> addHandleFailure resource


let restoreResource (resource: Resource) (state: State) =
    state
    |> State.removeResourceFromDown resource
    |> State.addFact (FactType.restored resource)
    |> addHandleRestore resource
        

let handleFailure resource procedureId allocationId (state: State) =
    let procedure = state.Procedures.[procedureId]

    // NOTE: In the future we want more complex failure handling. For now
    // we are just going to have Pause/Resume behavior

    match procedure.ProcedureState with
    | ProcedureState.Running ->
        // Kind of strange if this happened ?
        failwith "Probably shouldn't happen :/"
        state
    | ProcedureState.WaitingFor possibility ->
        let newProcedure = Procedure.changeToWaitingFor state.Now possibility (Set [resource]) procedure
        State.updateProcedure newProcedure state

    | ProcedureState.Suspended (suspendedAt, waitingFor, suspendedFor) ->
        let newProcedure = Procedure.changeToSuspended suspendedAt waitingFor (Set.add resource suspendedFor) procedure
        State.updateProcedure newProcedure state

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
        |> State.updateProcedure newProcedure // TODO: Clean up this wording
        |> State.enqueuePossibility newDelay (PossibilityType.Completion newCompletion)


    let private whenSuspendedButCannotResume suspendedAt completion newSuspendedFor procedure state =
        let newProcedure = 
            { procedure with
                ProcedureState = ProcedureState.Suspended (suspendedAt, completion, newSuspendedFor)
            }
        State.updateProcedure newProcedure state // TODO: Clean up this wording


    let private whenSuspended (resource: Resource) (procedure: Procedure) suspendedAt completion suspendedFor (state: State) =
        let newSuspendedFor = Set.remove resource suspendedFor
            
        state |>
        match Set.isEmpty newSuspendedFor with
        | true -> whenSuspendedButCanResume suspendedAt procedure completion
        | false -> whenSuspendedButCannotResume suspendedAt completion newSuspendedFor procedure
                

    let apply resource procedureId allocationId (state: State) =
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
        State.enqueueInstant (InstantType.proceed completion.ProcedureId) state
    else
        state
