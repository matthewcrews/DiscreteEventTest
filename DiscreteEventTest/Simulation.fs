module Desif.Simulation

open Desif.Types

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
        |> State.removeInstant instant


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
        |> State.removePossibility next
        

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
            |> State.addInstant (InstantType.proceed r.ProcedureId)
            |> AllocationResult.Success


module private Phases =

    // We want to run all Instants at a point in time. Instants could spawn new instants which
    // is why recursion is used instead of just take the set of Instants that exist at the
    // time and processing through those.
    let rec private runInstants (state: State) =
        
        match State.nextInstant state with
        | Some i ->
            Instant.handle i state
            |> runInstants
        | None ->
            state


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
    let rec runImmediatePhase (state: State) =

        match State.nextInstant state with
        | Some i ->
            Instant.handle i state
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
        
        match State.nextPossibility state with
        | Some possibility ->
            if possibility.ArrivalTimeStamp > maxTime then
                SimulationState.Complete { state with Now = maxTime }
            else
                state
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