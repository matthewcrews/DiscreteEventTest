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

// TODO: Make a better type for this that's better suited
type Schedule = Schedule of Possibility list

type Model = {
    Resources : Set<Resource>
    Generators : Set<Generator>
    Schedule : Schedule
}

type StateId = StateId of int64

type TimeStamp = TimeStamp of float
    with

    static member (-) (TimeStamp t1, TimeStamp t2) =
        TimeSpan (t1 - t2)

type TimeSpan = TimeSpan of float
    with

    static member (+) (TimeStamp stamp, TimeSpan span) =
        TimeStamp (stamp + span)

    static member (+) (span:TimeSpan, stamp:TimeStamp) =
        stamp + span

type Resource = Resource of string

type AllocationId = AllocationId of int64

type Allocation = {
    AllocationId : AllocationId
    Quantity : int
    Resources : Set<Resource>
}

[<RequireQualifiedAccess>]
type StepType =
    | Allocate of allocationId: AllocationId * quantity: int * resources: Set<Resource>
    | Delay of timeSpan: TimeSpan
    | Free of allocationId: AllocationId
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
    | Free of procedureId: ProcedureId * allocationId: AllocationId
    | Proceed of procedureId: ProcedureId
    //| AttemptResume of procedureId: ProcedureId
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
    | Failure of resource: Resource

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
    | Allocated of procedureId:ProcedureId * allocationId:AllocationId * resources:Set<Resource>
    | AllocationRequested of allocationRequest:AllocationRequest
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
    Free : Set<Resource>
    Down : Set<Resource>
    Allocations : Map<ProcedureId * AllocationId, Set<Resource>>
    Assignments : Map<Resource, ProcedureId * AllocationId>
    Procedures : Map<ProcedureId, Procedure>
    Instants : Set<Instant>
    Possibilities : Set<Possibility>
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