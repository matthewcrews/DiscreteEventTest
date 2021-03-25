namespace Desif

open Desif.Types

module Distribution =

    let sample (distribution: Distribution) =
        match distribution with
        | Constant c -> c
        | Uniform (lowerBound, upperBound) -> lowerBound // Yes, this is wrong


module Generator =

    let create name distribution possibilityType =
        {
            Name = GeneratorName name
            Distribution = distribution
            PossibilityType = possibilityType
        }


module TimeStamp =

    let zero = TimeStamp 0.0


module AllocationId =

    let next (AllocationId allocationId) =
        AllocationId (allocationId + 1L)


module Allocation =

    let create allocationId quantity resources =
        {
            AllocationId = allocationId
            Quantity = quantity
            Resources = resources
        }

module StepId =

    let next stepId =
        let (StepId s) = stepId
        StepId (s + 1L)


module StepType =

    let allocate allocationId quantity resources =
        StepType.Allocate (allocationId, quantity, resources)

    let free allocationId =
        StepType.Free allocationId

    let delay timeSpan =
        StepType.Delay timeSpan

module ProcedureId =

    let next (ProcedureId i) =
        ProcedureId (i + 1L)


module ProcedureState =

    let running =
        ProcedureState.Running

    let waiting completion =
        ProcedureState.WaitingFor completion

    let suspended suspendedAt remainingTime suspendedFor =
        ProcedureState.Suspended (suspendedAt, remainingTime, suspendedFor)


module Procedure =
    
    let create procedureId stateId pending processed procedureState =
        {
          ProcedureId = procedureId
          StateId = stateId
          Pending = pending
          Processed = processed
          ProcedureState = procedureState
        }

    let ofPlan procedureId (Plan steps) =
        create procedureId (StateId 0L) steps [] ProcedureState.running


module StateId =

    let next (StateId stateId) =
        StateId (stateId + 1L)

        
module InstantId =
            
    let next (InstantId i) =
        InstantId (i + 1L)


module InstantType =

    let free procedureId allocationId =
        InstantType.Free (procedureId, allocationId)

    let proceed procedureId =
        InstantType.Proceed procedureId

    let handleFailure resource procedureId allocationId =
        InstantType.HandleFailure (resource, procedureId, allocationId)

    let handleRestore resource procedureId allocationId =
        InstantType.HandleRestore (resource, procedureId, allocationId)

    let restore resource =
        InstantType.Restore resource


module Instant =

    let create instantId instantType =
        {
            InstantId = instantId
            InstantType = instantType
        }


module PossibilityId =
    
    let next (PossibilityId lastPossibilityId) =
        PossibilityId (lastPossibilityId + 1L)


module PossibilityType =

    let failure resource =
        PossibilityType.Failure resource


module Possibility =
    
    let create possibilityId timeStamp possibilityType =
        {
            PossibilityId = possibilityId
            ArrivalTimeStamp = timeStamp
            PossibilityType = possibilityType
        }


module AllocationRequest =

    let create timeStamp (procedure: Procedure) (allocation: Allocation) =
        {
            RequestTimeStamp = timeStamp
            ProcedureId = procedure.ProcedureId
            AllocationId = allocation.AllocationId
            StateId = procedure.StateId
            Quantity = allocation.Quantity
            Resources = allocation.Resources
        }


module FactId =

    let next (FactId factId) =
        FactId (factId + 1L)


module FactType =

    let allocationRequested allocationRequest  =
        FactType.AllocationRequested allocationRequest

    let allocated procedureId allocationId resources =
        FactType.Allocated (procedureId, allocationId, resources)

    let freed procedureId allocationId resources =
        FactType.Freed (procedureId, allocationId, resources)

    let stepStarted procedureId stateId step =
        FactType.StepStarted (procedureId, stateId, step)

    let stepCompleted procedureId stateId step =
        FactType.StepCompleted (procedureId, stateId, step)

    let procedureStarted procedureId =
        FactType.ProcedureStarted procedureId

    let procedureCompleted procedureId =
        FactType.ProcedureCompleted procedureId

    let failed resource =
        FactType.Failed resource

    let restored resource =
        FactType.Restored resource

    let suspended procedureId =
        FactType.Suspended procedureId

    let resumed procedureId =
        FactType.Resumed procedureId


module Fact =

    let create factId timeStamp factType =
        {
            FactId = factId
            TimeStamp = timeStamp
            FactType = factType
        }


/// Functions for transforming the state. They also track the History
[<RequireQualifiedAccess>]
module State =

    let initial =
        {
            Now = TimeStamp 0.0
            LastFactId = FactId 0L
            LastPossibilityId = PossibilityId 0L
            LastProcedureId = ProcedureId 0L
            LastInstantId = InstantId 0L
            Free = Set.empty
            Down = Set.empty
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


    let nextPossibilityId (s: State) =
        let next = PossibilityId.next s.LastPossibilityId
        next, { s with LastPossibilityId = next }


    let nextProcedureId (s: State) =
        let next = ProcedureId.next s.LastProcedureId
        next, { s with LastProcedureId = next}


    module Initializers =

        let private addPossibility state possibility =
            { state with Possibilities = Set.add possibility state.Possibilities }


        let private addPossibilities (maxTime: TimeStamp) (generators: seq<Generator>) modelState : State =

            let rec add (lastTime: TimeStamp) (maxTime: TimeStamp) (state: State) (generator: Generator) =
                let nextTimespan = Distribution.sample generator.Distribution
                let nextTime = lastTime + (TimeSpan nextTimespan)
                if nextTime > maxTime then
                    state
                else
                    let nextPossibilityId, modelState = nextPossibilityId state
                    let nextPossibility = Possibility.create nextPossibilityId nextTime generator.PossibilityType
                    let newState = addPossibility state nextPossibility
                    add nextTime maxTime newState generator

            let modelState =
                (modelState, generators)
                ||> Seq.fold (add TimeStamp.zero maxTime)

            modelState


        let private addResources resources state : State =

            let add modelState resource =
                { modelState with 
                    Free = Set.add resource state.Free
                }
        
            (state, resources)
            ||> Seq.fold add

        let private addSchedule (Schedule schedule) (state: State) =
            
            (state, schedule)
            ||> List.fold addPossibility

        let initialize (maxTime: TimeStamp) (model: Model) =
        
            initial
            |> addResources (model.Resources)
            |> addPossibilities maxTime model.Generators
            |> addSchedule model.Schedule


    let nextPossibility (modelState: State) =
        match modelState.Possibilities.IsEmpty with
        | true -> None
        | false -> 
            modelState.Possibilities
            |> Seq.sortBy (fun x -> x.ArrivalTimeStamp, x.PossibilityId)
            |> Seq.head
            |> Some


    let nextInstant (state: State) =
        match state.Instants.IsEmpty with
        | true -> None
        | false ->
            state.Instants
            |> Seq.sortBy (fun x -> x.InstantId)
            |> Seq.head
            |> Some


    let private setProcedureState (procedureState: Procedure) (state: State) =
        { state with Procedures = Map.add procedureState.ProcedureId procedureState state.Procedures }


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
        |> addFact (FactType.allocationRequested a)


    let addPossibility (delay: TimeSpan) (possibilityType: PossibilityType) (state: State) =
        let nextPossibilityId = PossibilityId.next state.LastPossibilityId
        let possibility = Possibility.create nextPossibilityId (state.Now + delay) possibilityType
        { state with
            LastPossibilityId = nextPossibilityId
            Possibilities = Set.add possibility state.Possibilities
        }
        // Note: We do not addFact here because this may or may not happen.
        // Facts are only things that HAVE happened

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
        let newFreeResources = state.Free - a.Resources
        let newAllocations = Map.add (procedureId, a.AllocationId) a.Resources state.Allocations
        { state with
            Free = newFreeResources
            Allocations = newAllocations
        }
        |> addFact (FactType.allocated procedureId a.AllocationId a.Resources)


    let setOpenRequests (requests: Set<AllocationRequest>) (state: State) =
        { state with
            OpenRequests = requests
        }


    let freeAllocation (procedureId: ProcedureId) (allocationId: AllocationId) (state: State) =
        let resources = state.Allocations.[procedureId, allocationId]
        { state with
            Free = resources + state.Free
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


    let private processStep (next: Step) (procedureState: Procedure) (state: State) =
        state |>
        match next.StepType with
        | StepType.Allocate (allocationId, quantity, resources) ->
            let allocation = Allocation.create allocationId quantity resources
            let request = AllocationRequest.create state.Now procedureState allocation
            addAllocationRequest request
        | StepType.Delay timeSpan ->
            let completion =
                {
                    ProcedureId = procedureState.ProcedureId
                    StateId = procedureState.StateId
                    StepId = next.StepId
                    CompletionTimeStamp = state.Now + timeSpan
                }
            addPossibility timeSpan (PossibilityType.Completion completion)
        | StepType.Free allocationId ->
            addInstant (InstantType.free procedureState.ProcedureId allocationId)
        | StepType.Fail (resource, timeTo) ->
            addPossibility timeTo (PossibilityType.failure resource)
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
          |> setProcedureState newProcedureState
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
      let newDown = Set.add resource state.Down
      { state with Down = newDown }


    let private removeResourceFromDown (resource: Resource) (state: State) =
      let newDown = Set.remove resource state.Down
      { state with Down = newDown }


    let failResource (resource: Resource) (state: State) =
        state
        |> addResourceToDown resource
        |> addFact (FactType.failed resource)
        |> addHandleFailure resource


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
            state
        | ProcedureState.WaitingFor possibility ->
            let nextStateId = StateId.next procedure.StateId
            let newProcedureState = ProcedureState.Suspended (state.Now, possibility, Set [resource])
            let newProcedure = 
                { procedure with
                    StateId = nextStateId
                    ProcedureState = newProcedureState 
                }
            setProcedureState newProcedure state

        | ProcedureState.Suspended (suspendedAt, waitingFor, suspendedFor) ->
            let newProcedureState = ProcedureState.Suspended (suspendedAt, waitingFor, Set.add resource suspendedFor)
            let nextStateId = StateId.next procedure.StateId
            let newProcedure = 
                { procedure with
                    StateId = nextStateId
                    ProcedureState = newProcedureState 
                }
            setProcedureState newProcedure state

    let handleRestore resource procedureId allocationId state =
        let procedure = state.Procedures.[procedureId]

        match procedure.ProcedureState with
        | ProcedureState.Running
        | ProcedureState.WaitingFor _ ->
            // The procedure likely no longer needs the resource that was freed
            state

        | ProcedureState.Suspended (suspendedAt, completion, suspendedFor) ->
            let newSuspendedFor = Set.remove resource suspendedFor

            match Set.isEmpty newSuspendedFor with
            | true ->
                // We can resume
                let newDelay = (completion.CompletionTimeStamp - suspendedAt)
                let newCompletion = 
                    { completion with 
                        CompletionTimeStamp = state.Now + newDelay
                        StateId = procedure.StateId
                    }

                let newProcedure = 
                    { procedure with
                        ProcedureState = ProcedureState.running
                    }

                state
                |> setProcedureState newProcedure // TODO: Clean up this wording
                |> addPossibility newDelay (PossibilityType.Completion newCompletion)
            | false ->
                let newProcedure = 
                    { procedure with
                        ProcedureState = ProcedureState.Suspended (suspendedAt, completion, newSuspendedFor)
                    }

                state
                |> setProcedureState newProcedure // TODO: Clean up this wording
                

    let processCompletion (completion: Completion) (state: State) =
        let p = state.Procedures.[completion.ProcedureId]

        if p.StateId = completion.StateId then
            addInstant (InstantType.proceed completion.ProcedureId) state
        else
            state


module Simulation =

    /// NOTE: We do not report Facts in this section. That is all handled by the State module.
    /// This is reserved for "business logic"

    module Instant =

        let handle (instant: Instant) (state: State) =
            state |>
            match instant.InstantType with
            | InstantType.Free (procedureId, allocationId) -> 
                State.freeAllocation procedureId allocationId
            | InstantType.Proceed procedureId ->
                State.proceed procedureId
            | InstantType.Restore resource ->
                State.restoreResource resource
            | InstantType.HandleFailure (resource, procedureId, allocationId) ->
                State.handleFailure resource procedureId allocationId
            | InstantType.HandleRestore (resource, procedureId, allocationId) ->
                State.handleRestore resource procedureId allocationId
            |> State.removeInstant instant


    module Possibility =

        let handle (next: Possibility) (state: State) : State =
            state |>
            match next.PossibilityType with
            | PossibilityType.Completion completion -> 
                State.processCompletion completion
            | PossibilityType.PlanArrival plan -> 
                State.startProcedure plan
            | PossibilityType.Failure resource ->
                State.failResource resource
            |> State.removePossibility next
        

    module Allocate =

        let tryAllocate (r: AllocationRequest) (state: State) : AllocationResult =
            let matchingResources = Set.intersect state.Free r.Resources

            match matchingResources.Count >= r.Quantity with
            | false -> AllocationResult.Failure r
            | true ->
                let toAllocate =
                    matchingResources
                    |> Seq.take r.Quantity
                    |> Set
                let newAllocation = Allocation.create r.AllocationId r.Quantity toAllocate
                State.addAllocation r.ProcedureId newAllocation state
                |> State.addInstant (InstantType.proceed r.ProcedureId)
                |> AllocationResult.Success


    let rec runInstantPhase (state: State) =
        
        match State.nextInstant state with
        | Some i ->
            Instant.handle i state
            |> runInstantPhase
        | None ->
            state


    let private prioritizeAllocationRequests (state: State) =
        state.OpenRequests
        // We check that the procedure is still waiting for the allocation. If it is in
        // a new state due to a rollback, the allocation request is no longer valid.
        |> Set.filter (fun x -> state.Procedures.[x.ProcedureId].StateId = x.StateId)
        |> Seq.sortBy (fun x -> x.RequestTimeStamp)
        |> List.ofSeq


    let runAllocationPhase (state: State) =
        let prioritizedRequests = prioritizeAllocationRequests state

        let rec processAllocations (unfulfilled: AllocationRequest list) (requests: AllocationRequest list) (state: State) : State =
            match requests with
            | [] -> State.setOpenRequests (Set unfulfilled) state
            | next::remaining ->
                match Allocate.tryAllocate next state with
                | AllocationResult.Success newState -> processAllocations unfulfilled remaining newState
                | AllocationResult.Failure ar -> processAllocations (ar::unfulfilled) remaining state

        processAllocations [] prioritizedRequests state


    /// We process all Instants and Allocations at a moment in time before proceeding to the
    /// next moment in time
    let rec immediatePhase (state: State) =

        match State.nextInstant state with
        | Some i ->
            Instant.handle i state
            |> immediatePhase
        | None ->
            let prevOpenRequests = state.OpenRequests
            let newState = runAllocationPhase state
            // If new Instants aArrivalTimeStampeed to process them
            if not (Set.isEmpty newState.Instants) then
                immediatePhase newState
            else
                newState


    /// This is when we take a step forward in time and process the next Possibility
    let timeStepPhase (maxTime: TimeStamp) (state: State) =
        
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


    let rec run (maxTime: TimeStamp) (model: Model) =

        let initialState = State.Initializers.initialize maxTime model

        let rec loop (maxTime: TimeStamp) (state: State) =

            let r = 
                immediatePhase state
                |> timeStepPhase maxTime
            match r with
            | SimulationState.Processing newState -> loop maxTime newState
            | SimulationState.Complete newState -> newState

        loop maxTime initialState


module Planning =

    type State<'a, 's> = ('s -> 'a * 's)
    type PlanAcc = PlanAcc of lastAllocationId:AllocationId * lastStepId:StepId * steps:Step list

    module State =
        // Explicit
        // let result x : State<'a, 's> = fun s -> x, s
        // Less explicit but works better with other, existing functions:
        let result x s = 
            x, s

        let bind (f:'a -> State<'b, 's>) (m:State<'a, 's>) : State<'b, 's> =
            // return a function that takes the state
            fun s ->
                // Get the value and next state from the m parameter
                let a, s' = m s
                // Get the next state computation by passing a to the f parameter
                let m' = f a
                // Apply the next state to the next computation
                m' s'

        /// Evaluates the computation, returning the result value.
        let eval (m:State<'a, 's>) (s:'s) = 
            m s 
            |> fst

        /// Executes the computation, returning the final state.
        let exec (m:State<'a, 's>) (s:'s) = 
            m s
            |> snd

        /// Returns the state as the value.
        let getState (s:'s) = 
            s, s

        /// Ignores the state passed in favor of the provided state value.
        let setState (s:'s) = 
            fun _ -> 
                (), s


    type PlanBuilder() =
        member __.Return(value) : State<'a, 's> = 
            State.result value
        member __.Bind(m:State<'a, 's>, f:'a -> State<'b, 's>) : State<'b, 's> = 
            State.bind f m
        member __.ReturnFrom(m:State<'a, 's>) = 
            m
        member __.Zero() =
            State.result ()
        member __.Delay(f) = 
            State.bind f (State.result ())


    let state = PlanBuilder()

    let allocateOneOf (resources: Set<Resource>) : State<_,PlanAcc> =
        state {
            let! (PlanAcc (lastAllocationId, lastStepId, steps)) = State.getState
            let nextStepId = StepId.next lastStepId
            let nextAllocationId = AllocationId.next lastAllocationId
            let quantity = 1 // This is the value for the `allocateOneOf` step command
            let stepType = StepType.Allocate (nextAllocationId, quantity, resources)
            let newStep = {
                StepId = nextStepId
                StepType = stepType
            }
            let newAcc = PlanAcc (nextAllocationId, nextStepId, newStep::steps)
            do! State.setState newAcc
            return nextAllocationId
        }

    type PlanBuilder with

        [<CustomOperation("delay", MaintainsVariableSpaceUsingBind=true)>]
        member this.Delay (st:State<_,PlanAcc>, [<ProjectionParameter>] (duration: 'a -> TimeSpan)) =
            state {
                let! x = st
                let d = duration x
                let! (PlanAcc (lastAllocationId, lastStepId, steps)) = State.getState
                let nextStepId = StepId.next lastStepId
                let stepType = StepType.Delay d
                let newStep = {
                    StepId = nextStepId
                    StepType = stepType
                }
                let newAcc = PlanAcc (lastAllocationId, nextStepId, newStep::steps)
                do! State.setState newAcc
                return x 
            }


        [<CustomOperation("free", MaintainsVariableSpaceUsingBind=true)>]
        member this.Free (st:State<_,PlanAcc>, [<ProjectionParameter>] (allocationId: 'a -> AllocationId)) =
            state {
                let! x = st
                let a = allocationId x
                let! (PlanAcc (lastAllocationId, lastStepId, steps)) = State.getState
                let nextStepId = StepId.next lastStepId
                let stepType = StepType.Free a
                let newStep = {
                    StepId = nextStepId
                    StepType = stepType
                }
                let newAcc = PlanAcc (lastAllocationId, nextStepId, newStep::steps)
                do! State.setState newAcc
                return x 
            }

    let planner = PlanBuilder ()

    let create (plan: State<_,_>) =
        let initialAcc = PlanAcc (AllocationId 0L, StepId 0L, [])
        let (PlanAcc (resultState, _, resultPlan)) = State.exec plan initialAcc
        Plan (List.rev resultPlan)


