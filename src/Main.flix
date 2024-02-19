use Random.Random;
use Bob.caller;
def main(): Unit \ IO  =
    Bob.caller();
    region rc {
        let y = Random.new();
        let fruits = Array#{"Apple", "Pear","Mango"} @ rc;
        let z = Random.choose(y, fruits);
        println("Valgt frugt: ${z}")
    }

// Seed is a Int64
type alias Seed = Int64

// Generators using a Seed
enum Gen[a] {
    case Gen(Seed -> a)
}


pub trait Arbitrary[a] {
    pub def arbitrary(): Gen[a]
}

enum Property[a] {
    case Property(a -> Bool)
}

pub trait Testable[a] {
    pub def testable(x: a): Property[a]
}



// instance Arbitrary[Int32] {
//     pub def arbitrary(x: Int32): Gen[Int32] = 5
// }


// pub def abc() : Int64 -> Int32 = 
//     Seed -> 5

// { Random.choose(seed, a);}
//newtype Gen a = Gen (Rand -> a)
//choose :: (Int, Int) -> Gen Int

// PAUSE!!!
//nextNatWithMax(r: Random, m: Int32)

// pub def choose(x: Int32, y: Int32) : Gen[Int32] = 
//     Gen.Gen(seed -> Random.nextNatWithMax(Random.newWithSeed(seed), y))
    




//
//instance Arbitrary Int {
//    def arbitrary(): Int = Choose(-20, 20)
//}
