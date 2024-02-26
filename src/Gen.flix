// pub enum Gen[a] {
//     case Gen(Rand -> a \ IO)
// }

pub enum Gen[a](Rand -> a \ IO)

/// Forslag til nemmere overblik?
pub type alias GenTest[a] = Rand -> a \ IO

/// Seed for random generator
type alias Rand = Int64

type alias Genn[a] = {
    unGen = Int32 -> Rand -> a
}

mod Gen {
    /// Returns a Generator that uniformly generates values in the interval [lower, upper] both values included
    pub def chooseInterval(lower: Int32, upper: Int32) : Gen[Int32] = 
        let func = rand -> Random.nextNatWithMax(Random.newWithSeed(rand), (upper + 1) - lower) + lower;
        Gen.Gen(func)

    pub def chooseBool() : Gen[Bool] =
        let func = rand -> Random.nextBool(Random.newWithSeed(rand));
        Gen.Gen(func)

}