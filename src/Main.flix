use Random.Random



// The main entry point.
def main(): Unit \ IO =
    let y = Random.new();
    region rc {
        let fruits = Array#{"Apple", "Pear", "Mango"} @ rc;
        let z = Random.choose(y, fruits);
        println("Funktionsvalg: ${z}")
    }

pub trait Arbitrary[a] {
    pub def arbitrary(x: a): Gen[a]
}

type alias Seed = Int64

// enum Gen[a] {
//     case Gen(Seed -> a)
// }

enum Gen[a](Seed -> a)




// { Random.choose(seed, a);}
//newtype Gen a = Gen (Rand -> a)
//choose :: (Int, Int) -> Gen Int

// PAUSE!!!
//nextNatWithMax(r: Random, m: Int32)
pub def choose(x: Int32, y: Int32) : Gen[Int32] = 
    Gen.Gen(seed -> Random.nextNatWithMax(Random.newWithSeed(seed), y))
    


instance Arbitrary[Int32] {
    pub def arbitrary(x: Int32): Gen[Int32] = 5

}


pub def abc() : Int64 -> Int32 = 
    seed -> 5

//
//instance Arbitrary Int {
//    def arbitrary(): Int = Choose(-20, 20)
//}
