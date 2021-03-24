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
    
    let create procedureId (Plan steps) =
        {
            ProcedureId = procedureId
            StateId = StateId 0L
            Pending = steps
            Processed = []
        }

        
module InstantId =
            
    let next (InstantId i) =
        InstantId (i + 1L)


module Instant =

    let create instantId instantType =
        {
            InstantId = instantId
            InstantType = instantType
        }


module PossibilityId =
    
    let next (PossibilityId lastPossibilityId) =
        PossibilityId (lastPossibilityId + 1L)


module Possibility =
    
    let create possibilityId timeStamp possibilityType =
        {
            PossibilityId = possibilityId
            TimeStamp = timeStamp
            PossibilityType = possibilityType
        }


module AllocationRequest =

    let create timeStamp (procedure: ProcedureState) (allocation: Allocation) =
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


module Fact =

    let create factId timeStamp factType =
        {
            FactId = factId
            TimeStamp = timeStamp
            FactType = factType
        }


/// Functions for transforming the state. They also track the History
module State =

    let initial =
        {
            Now = TimeStamp 0.0
            LastFactId = FactId 0L
            LastPossibilityId = PossibilityId 0L
            LastProcedureId = ProcedureId 0L
            LastInstantId = InstantId 0L
            FreeResources = Set.empty
            Allocations = Map.empty
            Assignments = Map.empty
            ProcedureStates = Map.empty
            Instants = Set.empty
            Possibilities = Set.empty
            OpenRequests = Set.empty
            History = []
        }
        
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

        let private addPossibility possibility modelState =
            { modelState with Possibilities = Set.add possibility modelState.Possibilities }


        let private addPossibilities (maxTime: TimeStamp) (generators: seq<Generator>) modelState : State =

            let rec add (lastTime: TimeStamp) (maxTime: TimeStamp) (modelState: State) (generator: Generator) =
                let nextTimespan = Distribution.sample generator.Distribution
                let nextTime = lastTime + (TimeSpan nextTimespan)
                if nextTime > maxTime then
                    modelState
                else
                    let nextPossibilityId, modelState = nextPossibilityId modelState
                    let nextPossibility = Possibility.create nextPossibilityId nextTime generator.PossibilityType
                    addPossibility nextPossibility modelState

            let modelState =
                (modelState, generators)
                ||> Seq.fold (add TimeStamp.zero maxTime)

            modelState


        let private addResources resources modelState : State =

            let add modelState resource =
                { modelState with FreeResources = Set.add resource modelState.FreeResources }
        
            (modelState, resources)
            ||> Seq.fold add

        let initialize (maxTime: TimeStamp) (model: Model) =
        
            initial
            |> addResources (model.Resources)
            |> addPossibilities maxTime model.Generators


    let nextPossibility (modelState: State) =
        match modelState.Possibilities.IsEmpty with
        | true -> None
        | false -> 
            modelState.Possibilities
            |> Seq.sortBy (fun x -> x.TimeStamp, x.PossibilityId)
            |> Seq.head
            |> Some

    let nextInstant (m: State) =
        match m.Instants.IsEmpty with
        | true -> None
        | false ->
            m.Instants
            |> Seq.sortBy (fun x -> x.InstantId)
            |> Seq.head
            |> Some

    let private setProcedureState procedureId procedureState (m: State) =
        { m with ProcedureStates = Map.add procedureId procedureState m.ProcedureStates }

    let addInstant instantType (m: State) =
        let nextInstantId = InstantId.next m.LastInstantId
        let nextInstant = Instant.create nextInstantId instantType
        { m with 
            LastInstantId = nextInstantId
            Instants = Set.add nextInstant m.Instants
        }

    let removeInstant (i: Instant) (m: State) =
        { m with Instants = Set.remove i m.Instants }

    let addAllocationRequest (a: AllocationRequest) (m: State) =
        { m with OpenRequests = Set.add a m.OpenRequests }
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
        let p = ProcedureState.create nextProcedureId plan
        { state with
            LastProcedureId = nextProcedureId
            ProcedureStates = Map.add nextProcedureId p state.ProcedureStates
        }
        |> addFact (FactType.procedureStarted nextProcedureId)
        |> addInstant (InstantType.ProcessNext nextProcedureId)

    let addAllocation procedureId (a: Allocation) (state: State) =
        let newFreeResources = state.FreeResources - a.Resources
        let newAllocations = Map.add (procedureId, a.AllocationId) a.Resources state.Allocations
        { state with
            FreeResources = newFreeResources
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
            FreeResources = resources + state.FreeResources
            Allocations = Map.remove (procedureId, allocationId) state.Allocations
        }
        |> addFact (FactType.freed procedureId allocationId resources)

    let private reportStepCompletion (procedureState: ProcedureState) (state: State) =
        match procedureState.Processed with
        | [] -> state
        | last::previous ->
            state
            |> addFact (FactType.stepCompleted procedureState.ProcedureId procedureState.StateId last)


    let incrementProcedure (procedureId: ProcedureId) (state: State) =
        let procedureState = state.ProcedureStates.[procedureId]
        match procedureState.Pending with
        | [] ->
            state
            |> addFact (FactType.procedureCompleted procedureId)
        | next::remaining ->
            let (StateId stateId) = procedureState.StateId
            let nextStateId = StateId (stateId + 1L)
            let newProcedureState =
                { procedureState with
                    Processed = next::procedureState.Processed
                    Pending = remaining
                    StateId = nextStateId
                }

            state
            |> reportStepCompletion procedureState
            |> setProcedureState procedureId newProcedureState
            |> addFact (FactType.stepStarted procedureId nextStateId next)


module Simulation =

    /// NOTE: We do not report Facts in this section. That is all handled by the State module.
    /// This is reserved for "business logic"

    open State


    module Instant =

        let private free (procedureId: ProcedureId) (allocationId: AllocationId) (state: State) =
            state
            |> State.freeAllocation procedureId allocationId
            |> addInstant (InstantType.Increment procedureId)

        let private increment (procedureId: ProcedureId) (state: State) =
            state
            |> State.incrementProcedure procedureId
            |> addInstant (InstantType.ProcessNext procedureId)

        let private processNext (procedureId: ProcedureId) (state: State) =
            let procedureState = state.ProcedureStates.[procedureId]
            match procedureState.Pending with
            | [] ->
                // Should report an empty plan
                state
            | next::remaining ->
                match next.StepType with
                | StepType.Allocate (allocationId, quantity, resources) ->
                    let allocation = Allocation.create allocationId quantity resources
                    let request = AllocationRequest.create state.Now procedureState allocation
                    addAllocationRequest request state
                    // TODO: Allocation requested fact
                | StepType.Delay s ->
                    addPossibility s (PossibilityType.Delay (procedureState.ProcedureId, procedureState.StateId)) state
                | StepType.Free allocationId ->
                    addInstant (InstantType.Free (procedureId, allocationId)) state

        let handle (i: Instant) (m: State) =
            match i.InstantType with
            | InstantType.Free (procedureId, allocationId) -> free procedureId allocationId m
            | InstantType.Increment procedureId -> increment procedureId m
            | InstantType.ProcessNext procedureId -> processNext procedureId m
            |> removeInstant i


    module Possibility =

        let private planArrival plan (modelState: State) =
            startProcedure plan modelState

        let private delay (procedureId: ProcedureId) (stateId: StateId) (m: State) =
            let p = m.ProcedureStates.[procedureId]

            if p.StateId = stateId then
                addInstant (InstantType.Increment procedureId) m
            else
                m

        let handle (next: Possibility) (modelState: State) : State =
            let m = { modelState with Now = next.TimeStamp }
            match next.PossibilityType with
            | PossibilityType.PlanArrival plan -> 
                planArrival plan m
            | PossibilityType.Delay (procedureId, stateId) -> 
                delay procedureId stateId modelState
            |> removePossibility next
        
    module Allocate =

        let tryAllocate (r: AllocationRequest) (m: State) : AllocationResult =
            let matchingResources = Set.intersect m.FreeResources r.Resources

            match matchingResources.Count >= r.Quantity with
            | false -> AllocationResult.Failure r
            | true ->
                let toAllocate =
                    matchingResources
                    |> Seq.take r.Quantity
                    |> Set
                let newAllocation = Allocation.create r.AllocationId r.Quantity toAllocate
                State.addAllocation r.ProcedureId newAllocation m
                |> State.addInstant (InstantType.Increment r.ProcedureId)
                |> AllocationResult.Success
                

    let rec runInstantPhase (m: State) =
        
        match State.nextInstant m with
        | Some i ->
            Instant.handle i m
            |> runInstantPhase
        | None ->
            m

    let runAllocationPhase (m: State) =
        let requests = 
            m.OpenRequests
            // We check that the procedure is still waiting for the allocation. If it is in
            // a new state due to a rollback, the allocation request is no longer valid.
            |> Set.filter (fun x -> m.ProcedureStates.[x.ProcedureId].StateId = x.StateId)
            |> Seq.sortBy (fun x -> x.RequestTimeStamp)
            |> List.ofSeq

        let rec processAllocations (unfulfilled: AllocationRequest list) (requests: AllocationRequest list) (m: State) : State =
            match requests with
            | [] -> State.setOpenRequests (Set unfulfilled) m
            | next::remaining ->
                match Allocate.tryAllocate next m with
                | AllocationResult.Success newState -> processAllocations unfulfilled remaining newState
                | AllocationResult.Failure ar -> processAllocations (ar::unfulfilled) remaining m

        processAllocations [] requests m


    /// We process all Instants and Allocations at a moment in time before proceeding to the
    /// next moment in time
    let rec immediatePhase (m: State) =

        match State.nextInstant m with
        | Some i ->
            Instant.handle i m
            |> immediatePhase
        | None ->
            let prevOpenRequests = m.OpenRequests
            let newState = runAllocationPhase m
            // If allocations have occured, we want to re-run the Instants
            if prevOpenRequests <> newState.OpenRequests then
                immediatePhase newState
            else
                newState


    /// This is when we take a step forward in time and process the next Possibility
    let timeStepPhase (maxTime: TimeStamp) (m: State) =
        
        match State.nextPossibility m with
        | Some possibility ->
            if possibility.TimeStamp > maxTime then
                SimulationState.Complete { m with Now = maxTime }
            else
                Possibility.handle possibility m
                |> SimulationState.Processing
        | None ->
            SimulationState.Complete { m with Now = maxTime }


    let rec run (maxTime: TimeStamp) (m: Model) =

        let initialState = State.Initializers.initialize maxTime m

        let rec loop (maxTime: TimeStamp) (modelState: State) =

            let r = 
                immediatePhase modelState
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


