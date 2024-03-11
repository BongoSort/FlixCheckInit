use Random.Random

pub type alias Rand = Int32

pub enum Gen[a](Rand -> a)

/// Returns a Generator that uniformly generates values in the interval [lower, upper]
pub def chooseInt(lower: Int32, upper: Int32): Gen[Int32] = 
    Gen.Gen(rand -> Int32.modulo(rand, (upper + 1) - lower) + lower)

/// Returns a generator that uniformly generates a boolean value.
pub def chooseBool(): Gen[Bool] =
    Gen.Gen(
        rand -> 
            match Int32.modulo(rand, 2) {
                case 0 => true,
                case _ => false
    })

/// Returns a generator that uniformly generates a lower case character.
pub def chooseLowerChar(): Gen[Char] =
    Gen.Gen(
        rand -> 
            match Char.forDigit(radix = 36, Int32.modulo(rand, 26) + 10) {
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
/// `gen` The generator to bind.
///
/// `fun` The function to apply to the generated value.
///
/// Returns A new generator that applies `fun` to the generated value of `gen`.
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

pub def randomPairMonadic(x: Gen[a], y: Gen[b]): Gen[(a, b)] =
    let func = a -> b -> (a, b);
    liftM2(func, x, y)


instance Arbitrary[(a, b)] with Arbitrary[a], Arbitrary[b] {
    pub def arbitrary(): Gen[(a, b)] =
        randomPair(Arbitrary.arbitrary(), Arbitrary.arbitrary())
}

pub def randomList(x: Gen[a]): Gen[List[a]] =
    let Gen.Gen(f) = x;
    let emptyListGen = Nel.singleton(return(Nil));
    let listGen = Gen.Gen(rand -> f(rand) :: f(rand) :: Nil);
    oneOf(Nel.cons(listGen, emptyListGen))

instance Arbitrary[List[a]] with Arbitrary[a] {
    pub def arbitrary(): Gen[List[a]] =
        randomList(Arbitrary.arbitrary())
}



///
/// Converts an integer to its binary representation.
///
/// x is the integer to convert.
///
/// Returns the binary representation of the input integer.
///
def toBinaryString(x: Int32): String =
    import static java.lang.Integer.toBinaryString(Int32): ##java.lang.String \ {};
    toBinaryString(x)

/// Splits a seed in two halves.
///
/// `seed` The seed to split, must be Int32.
///
/// Returns a tuple with the two halves of the seed.
def splitSeed(seed: Int32): (Int32, Int32) =
    let s = toBinaryString(seed);
    let len = String.length(s);
    let (s1, s2) = String.splitAt(len / 2, s);

    // Pad with 0s to get original length of seed
    // TODO consider the padding, maybe 1 for s11, or maybe XOR one half?
    // TODO The first seed will always be equal :(
    let s11 = String.padRight(len/2, '0', s1);
    let s22 = String.padLeft(len/2, '0', s2);
    (parseSeed(s11), parseSeed(s22))

/// Generate an additional seed from a given seed.
///
/// `seed` The seed to split, must be Int32.
///
/// Returns a tuple with the original seed and the new seed.
def genSeedsFromSeed(seed: Int32): (Int32, Int32) =
    let s = toBinaryString(seed);
    let len = String.length(s);
    let (s1, s2) = String.splitAt(len / 2, s);
    let new_bin_seed = s2+s1;
    (seed, parseSeed(new_bin_seed))

/// Parses a seed string and returns an integer value.
///
/// `seed` The seed string to parse.
///
/// Returns: The parsed integer value if successful.
///
/// This function will panic if the seed string cannot be parsed as an integer.
def parseSeed(seed: String): Int32 =
    match Int32.parse(2, seed) {
        case Ok(value) => value,
        case Err(_) => unreachable!()
    }

/// The main entry point.
def main(): Unit \ IO =
    let seed =  Random.nextInt32(Random.fresh());

    let (s1, s2) = splitSeed(seed);
    println("seed\n${seed}");
    println("split seed s1\n${s1}");
    println("split seed s2\n${s2}");

    let (s3, s4) = genSeedsFromSeed(seed);
    println("getSeedsFromSeed s3\n${s3}");
    println("getSeedsFromSeed s4\n${s4}");


    let g = randomPair(chooseBool(), chooseBool());
    let res = match g {case Gen.Gen(f) => f(seed)}; 
    println(res)





