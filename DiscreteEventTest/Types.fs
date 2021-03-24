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

type Model = {
    Resources : Set<Resource>
    Generators : Set<Generator>
}

type StateId = StateId of int64

type TimeStamp = TimeStamp of float

type TimeSpan = TimeSpan of float
    with

    static member (+) (TimeStamp stamp, TimeSpan span) =
        TimeStamp (stamp + span)

    static member (+) (span:TimeSpan, stamp:TimeStamp) =
        stamp + span

type Resource = Resource of string

[<RequireQualifiedAccess>]
type Availability =
    | Free
    | Allocated of allocationId:AllocationId

type AllocationId = AllocationId of int64

type Allocation = {
    AllocationId : AllocationId
    Quantity : int
    Resources : Set<Resource>
}

[<RequireQualifiedAccess>]
type StepType =
    | Allocate of allocation: Allocation
    | Free of allocationId: AllocationId
    | Delay of timeSpan: TimeSpan
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

type ProcedureState = {
    ProcedureId : ProcedureId
    StateId : StateId
    Processed : Step list
    Pending : Step list
}

type InstantId = InstantId of int64

[<RequireQualifiedAccess>]
type InstantType =
    | Free of procedureId: ProcedureId * allocationId: AllocationId
    | Increment of ProcedureId
    | ProcessNext of ProcedureId

type Instant = {
    InstantId : InstantId
    InstantType : InstantType
}

type PossibilityId = PossibilityId of int64

[<RequireQualifiedAccess>]
type PossibilityType = 
    | Delay of procedureId: ProcedureId * stateId: StateId
    | PlanArrival of plan: Plan

type Possibility =
    {
        PossibilityId : PossibilityId
        TimeStamp : TimeStamp
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

type ModelState = {
    Now : TimeStamp
    LastPossibilityId : PossibilityId
    LastProcedureId : ProcedureId
    LastInstantId : InstantId
    FreeResources : Set<Resource>
    Allocations : Map<ProcedureId * AllocationId, Set<Resource>>
    ProcedureStates : Map<ProcedureId, ProcedureState>
    Instants : Set<Instant>
    Possibilities : Set<Possibility>
    OpenRequests : Set<AllocationRequest>
}

[<RequireQualifiedAccess>]
type AllocationResult =
    | Success of modelState: ModelState
    | Failure of allocationRequest: AllocationRequest

[<RequireQualifiedAccess>]
type SimulationState =
    | Complete of modelState: ModelState
    | Processing of modelState: ModelState