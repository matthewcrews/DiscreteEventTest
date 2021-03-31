
namespace Desif.Types


type State = private {
    _Now                : TimeStamp
    _LastFactId         : FactId
    _LastPossibilityId  : PossibilityId
    _LastProcedureId    : ProcedureId
    _LastInstantId      : InstantId
    _FreeResources      : Set<Resource>
    _DownResources      : Set<Resource>
    _Allocations        : Map<ProcedureId * AllocationId, Set<Resource>>
    _Assignments        : Map<Resource, ProcedureId * AllocationId>
    _Procedures         : Map<ProcedureId, Procedure>
    _Instants           : Instant list
    _Possibilities      : Possibility list
    _OpenRequests       : Set<AllocationRequest>
    _History            : Fact list
}
with

    member this.Now               = this._Now              
    member this.LastFactId        = this._LastFactId       
    member this.LastPossibilityId = this._LastPossibilityId
    member this.LastProcedureId   = this._LastProcedureId  
    member this.LastInstantId     = this._LastInstantId    
    member this.FreeResources     = this._FreeResources    
    member this.DownResources     = this._DownResources    
    member this.Allocations       = this._Allocations      
    member this.Assignments       = this._Assignments      
    member this.Procedures        = this._Procedures       
    member this.Instants          = this._Instants         
    member this.Possibilities     = this._Possibilities    
    member this.OpenRequests      = this._OpenRequests     
    member this.History           = this._History          

/// Functions for transforming the state. They also track the History
[<RequireQualifiedAccess>]
module State =

    open Desif

    let initial : State =
        {
            _Now = TimeStamp.zero
            _LastFactId = FactId 0L
            _LastPossibilityId = PossibilityId 0L
            _LastProcedureId = ProcedureId 0L
            _LastInstantId = InstantId 0L
            _FreeResources = Set.empty
            _DownResources = Set.empty
            _Allocations = Map.empty
            _Assignments = Map.empty
            _Procedures = Map.empty
            _Instants = []
            _Possibilities = []
            _OpenRequests = Set.empty
            _History = []
        }


    let setNow (now: TimeStamp) (state: State) =
        { state with _Now = now }


    let addFact (factType: FactType) (state: State) =
        let nextFactId = FactId.next state.LastFactId
        let fact = Fact.create nextFactId state.Now factType
        { state with
            _LastFactId = nextFactId
            _History = fact::state.History
        }


    let enqueuePossibility (delay: Interval) (possibilityType: PossibilityType) (state: State) =
        let nextPossibilityId = PossibilityId.next state.LastPossibilityId
        let newPossibility = Possibility.create nextPossibilityId (state.Now + delay) possibilityType
        { state with
            _LastPossibilityId = nextPossibilityId
            _Possibilities = newPossibility::state.Possibilities
        }
        // Note: We do not addFact here because this may or may not happen.
        // Facts are only things that HAVE happened


    let addResource (resource: Resource) (state: State) =
        { state with
            _FreeResources = Set.add resource state.FreeResources
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
            let newState = { state with _Possibilities = remaining }
            Some (next, newState)


    let popInstant (state: State) =
        match state.Instants with
        | [] -> None
        | next::remaining -> 
            let newState = { state with _Instants = remaining }
            Some (next, newState)


    let enqueueInstant instantType (state: State) =
        let nextInstantId = InstantId.next state.LastInstantId
        let nextInstant = Instant.create nextInstantId instantType
        { state with 
            _LastInstantId = nextInstantId
            _Instants = nextInstant::state.Instants
        }


    let addAllocationRequest (allocationRequest: AllocationRequest) (state: State) =
        { state with 
            _OpenRequests = Set.add allocationRequest state.OpenRequests 
        }
        |> addFact (FactType.allocationRequestAdded allocationRequest)


    let removeAllocationRequest (allocationRequest: AllocationRequest) (state: State) =
        { state with 
            _OpenRequests = Set.remove allocationRequest state.OpenRequests
        }
        |> addFact (FactType.allocationRequestRemoved allocationRequest)


    let addAllocation procedureId (allocation: Allocation) (state: State) =
        let newFreeResources = state.FreeResources - allocation.Resources
        let newAllocations = Map.add (procedureId, allocation.AllocationId) allocation.Resources state.Allocations
        let newAssignments =
            (state.Assignments, allocation.Resources)
            ||> Seq.fold (fun s r -> Map.add r (procedureId, allocation.AllocationId) s)
        { state with
            _FreeResources = newFreeResources
            _Allocations = newAllocations
            _Assignments = newAssignments
        }
        |> addFact (FactType.allocationAdded procedureId allocation.AllocationId allocation.Resources)


    let removeAllocation procedureId allocationId (state: State) =
        let removed = state.Allocations.[procedureId, allocationId]
        { state with
            _Allocations = Map.remove (procedureId, allocationId) state.Allocations
        }
        |> addFact (FactType.allocationRemoved procedureId allocationId removed)


    let addFreeResources (resources: Set<Resource>) (state: State) =
        { state with
            _FreeResources = state.FreeResources + resources
        }


    let removeAssignments resources (state: State) =
        { state with
            _Assignments = Map.removeAll resources state.Assignments 
        }


    let addProcedure (procedure: Procedure) (state: State) =
        { state with
            _Procedures = Map.add procedure.ProcedureId procedure state.Procedures
        }


    let createProcedure plan (state: State) =
        let nextProcedureId = ProcedureId.next state.LastProcedureId
        let newProcedure = Procedure.ofPlan nextProcedureId plan
        let newState = { state with _LastProcedureId = nextProcedureId }
        newProcedure, newState


    let updateProcedure (procedure: Procedure) (state: State) =
        { state with
            _Procedures = Map.add procedure.ProcedureId procedure state.Procedures
        }


    let addResourceToDown (resource: Resource) (state: State) =
        let newDownResources = Set.add resource state.DownResources
        { state with 
            _DownResources = newDownResources
        }


    let removeResourceFromDown (resource: Resource) (state: State) =
        let newDownResources = Set.remove resource state.DownResources
        { state with 
            _DownResources = newDownResources
        }