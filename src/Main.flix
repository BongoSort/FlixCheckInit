use Random.Random;

def main(): Unit \ IO  =
    println("BOB");
    region rc {
        let y = Random.new();
        let fruits = Array#{"Apple", "Pear", "Mango"} @ rc;
        let z = Random.choose(y, fruits);
        println("Funktionsvalg: ${z}")
    }
    // region rc {
    //     let y = Random.new();
    //     let fruits = Array#{"Apple", "Pear", "Mango"} @ rc;
    //     let z = Random.choose(y, fruits);
    //     println("Funktionsvalg: ${z}");
    //     
    // }


// pub trait Arbitrary[a] {
//     pub def arbitrary(x: a): Gen[a]
// }

// type alias Seed = Int64

// enum Gen[a] {
//     case Gen(Seed -> a)
// }


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
