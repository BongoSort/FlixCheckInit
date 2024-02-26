
    pub enum FlixCheckResult {
    case Success,
    case Failure(String)
    }

mod Prop {
    pub type alias Rand = Int64
    // should define the property type
    pub type alias Property = Gen[FlixCheckResult]

    pub def runProp(p : Property, seed: Rand): FlixCheckResult \ IO =
        p(seed)

    pub trait Testable[a] {
        // TODO
    }
}