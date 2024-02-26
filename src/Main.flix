use Random.Random

pub type alias Rand = Int32

pub enum Gen[a](Rand -> a)

// TODO: Assert safe bounds!
// Returns a Generator that uniformly generates values in the interval [lower, upper]
pub def chooseInterval(lower: Int32, upper: Int32): Gen[Int32] = 
    Gen.Gen(rand -> Int32.mod(rand, (upper + 1) - lower) + lower)

pub def chooseBool(): Gen[Bool] =
    Gen.Gen(
        rand -> match Int32.mod(rand, 2) {
            case 0 => true,
            case _ => false
    })

pub def chooseAlphabetChar(): Gen[Char] =
    Gen.Gen(
        rand -> match Char.forDigit(radix = 36, Int32.mod(rand, 26) + 10) {
            case Some(d) => d,
            case None => '\u0061' // FIX SENERE
        }
    )


pub def oneOf(xs: List[t]): Result[String, Gen[t]] \ IO =
    if (List.isEmpty(xs)) 
        Result.Err("Method oneOf called on empty list!")
     else 
        let size = List.length(xs);
        let i = Random.nextNatWithMax(Random.new(), size);
        let res = Vector.get(i, List.toVector(xs));
        Result.Ok(Gen.Gen(rand -> res))
    


// Arbitrary instances
pub trait Arbitrary[a] {
    pub def arbitrary(): Gen[a]
}

instance Arbitrary[Int32] {
    pub def arbitrary(): Gen[Int32] = chooseInterval(-20, 20) //Hardcoded interval, fix later?
}

instance Arbitrary[Bool] {
    pub def arbitrary(): Gen[Bool] = chooseBool()
}

instance Arbitrary[Char] {
    pub def arbitrary(): Gen[Char] = chooseAlphabetChar()
}

// instance Arbitrary[(Arbitrary[Int32], Arbitrary[Bool])] {
//     pub def arbitrary(): Gen[(a, b)] = (Arbitrary[a], Arbitrary[b])
// }

// The main entry point.
def main(): Unit \ IO =
    let seed = 27;
    let g = chooseAlphabetChar();
    let res = match g {case Gen.Gen(f) => f(seed)};

    println(res)




