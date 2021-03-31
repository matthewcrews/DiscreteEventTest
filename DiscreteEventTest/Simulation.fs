module Desif.Simulation

open Desif.Types

[<RequireQualifiedAccess>]
type AllocationResult =
    | Success of state: State
    | Failure of allocationRequest: AllocationRequest

[<RequireQualifiedAccess>]
type SimulationState =
    | Complete of state: State
    | Processing of state: State

/// NOTE: We do not report Facts in this section. That is all handled by the State module.
/// This is reserved for "business logic"

module private Instant =

    let handle (instant: Instant) (state: State) =
        state |>
        match instant.InstantType with
        | InstantType.FreeAllocation (procedureId, allocationId) -> 
            State.freeAllocation procedureId allocationId
        | InstantType.Proceed procedureId ->
            State.proceed procedureId
        | InstantType.Failure (procedureId, resource) ->
            State.failResource procedureId resource
        | InstantType.Restore resource ->
            State.restoreResource resource
        | InstantType.HandleFailure (resource, procedureId, allocationId) ->
            State.handleFailure resource procedureId allocationId
        | InstantType.HandleRestore (resource, procedureId, allocationId) ->
            State.Possibility.apply resource procedureId allocationId


module private Possibility =

    let handle (next: Possibility) (state: State) : State =
        state |>
        match next.PossibilityType with
        | PossibilityType.Completion completion -> 
            State.processCompletion completion
        | PossibilityType.PlanArrival plan -> 
            State.startProcedure plan
        //| PossibilityType.Failure (procedureId, resource) ->
        //    State.failResource procedureId resource
        

module private Allocate =

    // NOTE: This is where you would want to put sophisticated logic around the priority
    // of different Procedures that are running. Right now this is just a FIFO queue
    // across all Procedures and Resources.
    let tryAllocate (r: AllocationRequest) (state: State) : AllocationResult =
        let matchingResources = Set.intersect state.FreeResources r.Resources

        match matchingResources.Count >= r.Quantity with
        | false -> AllocationResult.Failure r
        | true ->
            let toAllocate =
                matchingResources
                |> Seq.take r.Quantity
                |> Set
            let newAllocation = Allocation.create r.AllocationId r.Quantity toAllocate
            State.addAllocation r.ProcedureId newAllocation state
            |> State.removeAllocationRequest r
            |> State.enqueueInstant (InstantType.proceed r.ProcedureId)
            |> AllocationResult.Success


module private Phases =

    // NOTE: This is where business logic would need to go for prioritizing which allocations
    // are attempted first. Different procedures likely have some prioritization based on
    // delivery dates or start times.
    let private prioritizeAllocationRequests (state: State) =
        state.OpenRequests
        // We check that the procedure is still waiting for the allocation. If it is in
        // a new state due to a rollback or some other event, the allocation request is
        // no longer valid.
        |> Set.filter (fun x -> state.Procedures.[x.ProcedureId].StateId = x.StateId)
        |> Seq.sortBy (fun x -> x.RequestTimeStamp)
        |> List.ofSeq


    let private runAllocations (state: State) =
        let prioritizedRequests = prioritizeAllocationRequests state

        let rec processAllocations (unfulfilled: AllocationRequest list) (requests: AllocationRequest list) (state: State) : State =
            match requests with
            | [] -> state
            | next::remaining ->
                match Allocate.tryAllocate next state with
                | AllocationResult.Success newState -> processAllocations unfulfilled remaining newState
                | AllocationResult.Failure ar -> processAllocations (ar::unfulfilled) remaining state

        processAllocations [] prioritizedRequests state


    // We process all Instants and Allocations at a point in time before proceeding to the
    // next point in time. This is akin to the three-phase approach:
    // https://en.wikipedia.org/wiki/Discrete-event_simulation#Three-Phased_Approach
    // Instants are treated as a stack. An instant can enqueue additional instants that will
    // be processed next since they are on top of the stack.
    let rec runImmediatePhase (state: State) =

        match State.popInstant state with
        | Some (nextInstant, nextState) ->
            Instant.handle nextInstant nextState
            |> runImmediatePhase
        | None ->
            let newState = runAllocations state
            // If new Instants have spawned, we must process them
            if not (List.isEmpty newState.Instants) then
                runImmediatePhase newState
            else
                newState


    /// This is when we take a step forward in time and process the next Possibility
    let runTimeStepPhase (maxTime: TimeStamp) (state: State) =
        
        match State.popPossibility state with
        | Some (possibility, newState) ->
            if possibility.ArrivalTimeStamp > maxTime then
                SimulationState.Complete { newState with Now = maxTime }
            else
                newState
                |> State.setNow possibility.ArrivalTimeStamp
                |> Possibility.handle possibility
                |> SimulationState.Processing
        | None ->
            SimulationState.Complete { state with Now = maxTime }


let run (maxTime: TimeStamp) (model: Model) =

    let initialState = State.initialize maxTime model

    let rec loop (maxTime: TimeStamp) (state: State) =

        let r = 
            Phases.runImmediatePhase state
            |> Phases.runTimeStepPhase maxTime
        match r with
        | SimulationState.Processing newState -> loop maxTime newState
        | SimulationState.Complete newState -> newState

    loop maxTime initialState
