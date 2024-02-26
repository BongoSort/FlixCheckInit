// Udsættes til senere
// mod Prop {
//         pub enum FlixCheckResult {
//     case Success,
//     case Failure(String)
//     }

//     pub type alias Rand = Int32
//     // should define the property type
//     pub type alias Property = Gen[FlixCheckResult]

//     pub def runProp(p : Property, seed: Rand): FlixCheckResult \ IO =
//         p(seed)

//     pub trait Testable[a] {
//         // TODO
//     }
// }