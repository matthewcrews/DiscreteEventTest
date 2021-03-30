module rec Desif.Types


type Distribution =
    | Constant of float
    | Uniform of lowerBound:float * upperBound:float

type GeneratorName = GeneratorName of string

type Generator = {
    Name : GeneratorName
    Distribution : Distribution
    PossibilityType : PossibilityType
}

type ScheduledEvent =
    | StartPlan of plan: Plan * arrivalTimeStamp: TimeStamp

// TODO: Make a better type for this that's better suited
type Schedule = Schedule of ScheduledEvent list

type Model = {
    Resources : Set<Resource>
    Generators : Set<Generator>
    Schedule : Schedule
}

type StateId = StateId of int64

type TimeStamp = TimeStamp of System.TimeSpan
    with

    static member (-) (TimeStamp t1, TimeStamp t2) =
        Interval (t1 - t2)

type Interval = Interval of System.TimeSpan
    with

    static member (+) (TimeStamp stamp, Interval span) =
        TimeStamp (stamp + span)

    static member (+) (span:Interval, stamp:TimeStamp) =
        stamp + span

    static member (+) (Interval i1, Interval i2) =
        Interval (i1 + i2)

type Resource = Resource of string

type AllocationId = AllocationId of int64

type Allocation = {
    AllocationId : AllocationId
    Quantity : int
    Resources : Set<Resource>
}

[<RequireQualifiedAccess>]
type StepType =
    | Allocate of allocationId: AllocationId * numberOf: int * resources: Set<Resource>
    | Delay of timeSpan: Interval
    | FreeAllocation of allocationId: AllocationId
    | Fail of resource: Resource
    | Restore of resource: Resource
    //| Move of item: ItemId * location: Location
    // | Open of flow: Flow * flowDescription: FlowDescription
    // | Close of flow: Flow

type StepId = StepId of int64

type Step = {
    StepId : StepId
    StepType : StepType
}

type Plan = Plan of Step list

type ProcedureId = ProcedureId of int64

[<RequireQualifiedAccess>]
type ProcedureState =
    // TODO: Waiting for Allocation
    | Running
    | WaitingFor of completion: Completion
    | Suspended of suspendedAt: TimeStamp * waitingFor: Completion * suspendedFor: Set<Resource>

type Procedure = {
    ProcedureId : ProcedureId
    StateId : StateId
    Processed : Step list
    Pending : Step list
    ProcedureState : ProcedureState
}

type InstantId = InstantId of int64

[<RequireQualifiedAccess>]
type InstantType =
    | FreeAllocation of procedureId: ProcedureId * allocationId: AllocationId
    | Failure of procedureId: ProcedureId * resource: Resource
    | Proceed of procedureId: ProcedureId
    | Restore of resource: Resource
    | HandleFailure of resource: Resource * procedureId: ProcedureId * allocationId: AllocationId
    | HandleRestore of resource: Resource * procedureId: ProcedureId * allocationId: AllocationId

type Instant = {
    InstantId : InstantId
    InstantType : InstantType
}

type Completion = {
    ProcedureId : ProcedureId
    StepId : StepId
    StateId : StateId
    CompletionTimeStamp : TimeStamp
}

type PossibilityId = PossibilityId of int64

[<RequireQualifiedAccess>]
type PossibilityType = 
    | Completion of completion: Completion
    | PlanArrival of plan: Plan

type Possibility =
    {
        PossibilityId : PossibilityId
        ArrivalTimeStamp : TimeStamp
        PossibilityType : PossibilityType
    }

type AllocationRequest = {
    RequestTimeStamp : TimeStamp
    ProcedureId : ProcedureId
    AllocationId : AllocationId
    StateId : StateId
    Quantity : int
    Resources : Set<Resource>
}

type FactId = FactId of int64

[<RequireQualifiedAccess>]
type FactType =
    | AllocationRequestAdded of allocationRequest: AllocationRequest
    | AllocationRequestRemoved of allocationRequest: AllocationRequest
    | AllocationAdded of procedureId:ProcedureId * allocationId:AllocationId * resources:Set<Resource>
    | AllocationRemoved of procedureId:ProcedureId * allocationId:AllocationId * resources:Set<Resource>
    | Freed of procedureId:ProcedureId * allocationId:AllocationId * resources:Set<Resource>
    | StepStarted of procedureId:ProcedureId * stateId:StateId * step:Step
    | StepCompleted of procedureId:ProcedureId * stateId:StateId * step:Step
    | ProcedureStarted of procedureId:ProcedureId
    | ProcedureCompleted of procedureId:ProcedureId
    | Failed of resource:Resource
    | Restored of resource:Resource
    | Suspended of procedureId:ProcedureId
    | Resumed of procedureId:ProcedureId

type Fact = {
    FactId : FactId
    TimeStamp : TimeStamp
    FactType : FactType
}

type State = {
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

[<RequireQualifiedAccess>]
type AllocationResult =
    | Success of state: State
    | Failure of allocationRequest: AllocationRequest

[<RequireQualifiedAccess>]
type SimulationState =
    | Complete of state: State
    | Processing of state: State