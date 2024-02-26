// pub enum Gen[a] {
//     case Gen(Rand -> a \ IO)
// }

pub enum GenTest[a](Rand -> a \ IO)

// Forslag til nemmere overblik?
pub type alias Gen[a] = Rand -> a \ IO

// Seed needed for random generator
pub type alias Rand = Int64

// type alias Genn[a] = {
//     unGen = Int32 -> Rand -> a
// }

mod Gen {
    // Returns a Generator that uniformly generates values in the interval [lower, upper]
    pub def chooseInterval(lower: Int32, upper: Int32): Gen[Int32] = 
        rand -> Random.nextNatWithMax(Random.newWithSeed(rand), (upper + 1) - lower) + lower

    pub def chooseBool() : Gen[Bool] =
        rand -> Random.nextBool(Random.newWithSeed(rand))

}