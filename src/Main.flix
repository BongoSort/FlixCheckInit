use Random.Random

pub type alias Rand = Int32

pub enum Gen[a](Rand -> a)

/// Returns a Generator that uniformly generates values in the interval [lower, upper]
pub def chooseInt(lower: Int32, upper: Int32): Gen[Int32] = 
    Gen.Gen(rand -> Int32.mod(rand, (upper + 1) - lower) + lower)

/// Returns a generator that uniformly generates a boolean value.
pub def chooseBool(): Gen[Bool] =
    Gen.Gen(
        rand -> 
            match Int32.mod(rand, 2) {
                case 0 => true,
                case _ => false
    })

/// Returns a generator that uniformly generates a lower case character.
pub def chooseLowerChar(): Gen[Char] =
    Gen.Gen(
        rand -> 
            match Char.forDigit(radix = 36, Int32.mod(rand, 26) + 10) {
                case Some(d) => d,
                case None => unreachable!()
        }
    )

/// Returns a generator that uniformly generates an upper case character.
pub def chooseUpperChar(): Gen[Char] =
    let nelOpt = List.map(x -> return(x), 
        ('A' :: 'B' :: 'C' :: 'D' :: 'E' :: 'F' :: 'G' :: 'H' :: 
         'I' :: 'J' :: 'K' :: 'L' :: 'M' :: 'N' :: 'O' :: 'P' :: 
         'Q' :: 'R' :: 'S' :: 'T' :: 'U' :: 'V' :: 'W' :: 'X' :: 
         'Y' :: 'Z' :: Nil));
    let nel = 
        match List.toNel(nelOpt) {
            case Some(n) => n,
            case None => unreachable!()
    };
    oneOf(nel)

/// Returns a generator that generates the value `a`.
pub def return(a: a): Gen[a] = 
    Gen.Gen(_ -> a)


///
/// Binds a generator `gen` with a function `fun` to produce a new generator.
///
/// @param gen The generator to bind.
///
/// @param fun The function to apply to the generated value.
///
/// @return A new generator that applies `fun` to the generated value of `gen`.
///
pub def bind(gen: Gen[a], fun: a -> Gen[b]): Gen[b] =
    Gen.Gen(
        rand ->
            let Gen.Gen(aGen) = gen;
            let result: a = aGen(rand);
            let newGen: Gen[b]  = fun(result);
            let Gen.Gen(f) = newGen;
            f(rand)
    )

pub def liftM2(f: a -> b -> c, genA: Gen[a], genB: Gen[b]): Gen[c] =
    bind(genA, a -> bind(genB, b -> return(f(a)(b))))


/// TODO use nel instead of list
pub def oneOf(xs: Nel[Gen[a]]): Gen[a] =
    let v = Nel.toVector(xs);
    let size = Vector.length(v);
    bind(chooseInt(0, size - 1), i -> Vector.get(i, v))
    


// Arbitrary instances
pub trait Arbitrary[a] {
    pub def arbitrary(): Gen[a]
}


instance Arbitrary[Int32] {
    pub def arbitrary(): Gen[Int32] =
        chooseInt(-20, 20)
}


instance Arbitrary[Bool] {
    pub def arbitrary(): Gen[Bool] =
        chooseBool()
}

instance Arbitrary[Char] {
    pub def arbitrary(): Gen[Char] = 
        chooseLowerChar()
}

// def intToBinString(x: ##java.integer)
// def intToBinString(x: ##java.integer)
// def fromList(l: ##java.util.List): List[a]

pub def randomPair(x: Gen[a], y: Gen[b]): Gen[(a, b)] =
    let Gen.Gen(f) = x;
    let Gen.Gen(g) = y;
    // let (rand1, rand2) = (4, 5);
    // Split seed TODO
    Gen.Gen(rand -> (f(rand), g(rand)))


instance Arbitrary[(a, b)] with Arbitrary[a], Arbitrary[b] {
    pub def arbitrary(): Gen[(a, b)] =
        let aGen: Gen[a] = Arbitrary.arbitrary();
        let bGen: Gen[b] = Arbitrary.arbitrary();
        let func = a -> b -> (a, b);
        liftM2(func, aGen, bGen)
}



// The main entry point.
def main(): Unit \ IO =
    let seed =  Random.nextInt32(Random.new());
    let g = randomPair(chooseBool(), chooseBool());
    let res = match g {case Gen.Gen(f) => f(seed)}; 
    println(res)




