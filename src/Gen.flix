use Random.Random

enum Gen[a](Rand -> a \ IO)

mod Gen {

    // Returns a Generator that uniformly generates values in the interval [lower, upper]
    pub def chooseInterval(lower: Int32, upper: Int32) : Gen[Int32] = 
    let func = rand -> Random.nextNatWithMax(Random.newWithSeed(rand), (upper + 1) - lower) + lower;
    Gen.Gen(func)


}