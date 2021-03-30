namespace Desif

open Desif.Types

module List =

    let inline foldR (folder: 'T -> 'State -> 'State) acc elements =
      (acc, elements)
      ||> List.fold (fun a b -> folder b a)


module Seq =

    let inline foldR (folder: 'T -> 'State -> 'State) acc elements =
        (acc, elements)
        ||> Seq.fold (fun a b -> folder b a)


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

    let zero = TimeStamp System.TimeSpan.Zero


module TimeSpan =
    
    let zero = Interval System.TimeSpan.Zero


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
        StepType.FreeAllocation allocationId

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

    let failure procedureId resource =
        PossibilityType.Failure (procedureId, resource)


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

    let allocationRequestAdded allocationRequest  =
        FactType.AllocationRequestAdded allocationRequest

    let allocationRequestRemoved allocationRequest  =
      FactType.AllocationRequestRemoved allocationRequest

    let allocationRemoved procedureId allocationId resources =
        FactType.AllocationRemoved (procedureId, allocationId, resources)

    let allocationAdded procedureId allocationId resources =
        FactType.AllocationAdded (procedureId, allocationId, resources)

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


module private StateManagement =
    type State<'a, 's> = ('s -> 'a * 's)

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

module Planning =

    open StateManagement

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

    type PlanAcc = PlanAcc of lastAllocationId:AllocationId * lastStepId:StepId * steps:Step list

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
        member this.Delay (st:State<_,PlanAcc>, [<ProjectionParameter>] (duration: 'a -> Interval)) =
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
                let stepType = StepType.FreeAllocation a
                let newStep = {
                    StepId = nextStepId
                    StepType = stepType
                }
                let newAcc = PlanAcc (lastAllocationId, nextStepId, newStep::steps)
                do! State.setState newAcc
                return x 
            }

        [<CustomOperation("fail", MaintainsVariableSpaceUsingBind=true)>]
        member this.Fail (st:State<_,PlanAcc>, [<ProjectionParameter>] (resource: 'a -> Resource)) =
            state {
                let! x = st
                let r = resource x
                let! (PlanAcc (lastAllocationId, lastStepId, steps)) = State.getState
                let nextStepId = StepId.next lastStepId
                let stepType = StepType.Fail r
                let newStep = {
                    StepId = nextStepId
                    StepType = stepType
                }
                let newAcc = PlanAcc (lastAllocationId, nextStepId, newStep::steps)
                do! State.setState newAcc
                return x 
            }

        [<CustomOperation("restore", MaintainsVariableSpaceUsingBind=true)>]
        member this.Restore (st:State<_,PlanAcc>, [<ProjectionParameter>] (resource: 'a -> Resource)) =
            state {
                let! x = st
                let r = resource x
                let! (PlanAcc (lastAllocationId, lastStepId, steps)) = State.getState
                let nextStepId = StepId.next lastStepId
                let stepType = StepType.Restore r
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
