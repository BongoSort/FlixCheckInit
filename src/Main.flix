use Random.Random
use Arbitrary.Arbitrary
use Gen.Gen

// The main entry point.
def main(): Unit \ IO =
    let generator : Gen[Int32] = Arbitrary.arbitrary();
    let rand : Random = Random.new();

    printWithRandomSeed(100, rand, generator);
    checkOdd(10)

/// Prints from an interval of numbers n amount of times
///
/// Uses the Generator with a random seed
pub def printWithRandomSeed(n: Int32, rng: Random, gen: Gen[Int32]): Unit \ IO =
    if (n >= 0)
        let genFunc = match gen {case Gen.Gen(f) => f};
        let seed = Random.nextInt64(rng);
        println(genFunc(seed));
        printWithRandomSeed(n - 1, rng, gen)
    else
        println("Done")

/// Initial function for checking isOdd "times" number of times
pub def checkOdd(times : Int32): Unit \ IO =
    if (times > 0)
        let gen = Gen.chooseInterval(-20, 20);
        let func = match gen {case Gen.Gen(f) => f};
        let seed = Random.nextInt64(Random.new());
        let number = func(seed);
        let res = OE.isOdd(number);
        println("Is the number ${number} Odd? : ${res}");
        checkOdd(times - 1)
    else
        println("Done checking isOdd")




