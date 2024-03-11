
pub type alias Rand = Int32

pub enum Gen[a](Rand -> a)

pub def applyGen(g: Gen[a], r: Rand): a =
    match g {
        case Gen.Gen(f) => f(r)
    }


/// Returns a Generator that uniformly generates values in the interval [lower, upper]
pub def chooseInt(lower: Int32, upper: Int32): Gen[Int32] = 
    Gen.Gen(rand -> Int32.mod(rand, (upper + 1) - lower) + lower)

/// Returns a generator that uniformly generates a boolean value.
pub def chooseBool(): Gen[Bool] =
    oneOf(Nel.Nel(return(true), return(false) :: Nil))

/// Returns a generator that uniformly generates a lower case character.
pub def chooseLowerChar(): Gen[Char] =
    let nelOfChar: Nel[Char] = Nel.Nel('a', 
    'b' :: 'c' :: 'd' :: 'e' :: 'f' :: 'g' :: 'h' :: 
    'i' :: 'j' :: 'k' :: 'l' :: 'm' :: 'n' :: 'o' :: 
    'p' :: 'q' :: 'r' :: 's' :: 't' :: 'u' :: 'v' :: 
    'w' :: 'x' :: 'y' :: 'z' :: Nil);
    oneOf(Nel.map(x -> return(x), nelOfChar))

/// Returns a generator that uniformly generates an upper case character.
pub def chooseUpperChar(): Gen[Char] =
    let nelOfChar: Nel[Char] = Nel.Nel('A', 
    'B' :: 'C' :: 'D' :: 'E' :: 'F' :: 'G' :: 'H' ::
    'I' :: 'J' :: 'K' :: 'L' :: 'M' :: 'N' :: 'O' ::
    'P' :: 'Q' :: 'R' :: 'S' :: 'T' :: 'U' :: 'V' ::
    'W' :: 'X' :: 'Y' :: 'Z' :: Nil);
    oneOf(Nel.map(x -> return(x), nelOfChar))

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
pub def bind(aGen: Gen[a], fun: a -> Gen[b]): Gen[b] =
    Gen.Gen(
        rand ->
            let newGen: Gen[b] = fun(applyGen(aGen, rand));
            applyGen(newGen, rand)
    )

pub def liftM2(f: a -> b -> c, aGen: Gen[a], bGen: Gen[b]): Gen[c] =
    bind(aGen, a -> bind(bGen, b -> return(f(a, b))))


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
        chooseInt(-10, 10)
}

instance Arbitrary[Bool] {
    pub def arbitrary(): Gen[Bool] =
        chooseBool()
}

instance Arbitrary[Char] {
    pub def arbitrary(): Gen[Char] = 
        chooseLowerChar()
}

pub def randomPair(aGen: Gen[a], bGen: Gen[b]): Gen[(a, b)] =
    Gen.Gen(rand -> 
        let (r1, r2) = splitSeed(rand);
        (applyGen(aGen, r1), applyGen(bGen, r2))
    )

pub def randomPairMonadic(x: Gen[a], y: Gen[b]): Gen[(a, b)] =
    let func = a -> b -> (a, b);
    liftM2(func, x, y)


instance Arbitrary[(a, b)] with Arbitrary[a], Arbitrary[b] {
    pub def arbitrary(): Gen[(a, b)] =
        randomPair(Arbitrary.arbitrary(), Arbitrary.arbitrary())
}

    
pub def randomList(aGen: Gen[a], r: Rand): List[a] =
    let (r1, r2) = split(r);
    match applyGen(chooseBool(), r2) {
        case true => applyGen(aGen, r1) :: randomList(aGen, r2),
        case false => Nil
    }


instance Arbitrary[List[a]] with Arbitrary[a] {
    pub def arbitrary(): Gen[List[a]] =
        Gen.Gen(rand -> randomList(Arbitrary.arbitrary(), rand))
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
def splitSeed1(seed: Int32): (Int32, Int32) =
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

def splitSeed(r: Rand): (Rand, Rand) =
    // let goldenGamma16 = 40503; // true 16 bit golden gamma value. Its uneven.
    let goldenGamma = 265443576; //Removed last digit from the golden ratio number. Even.
    let r1 = r + goldenGamma;
    let r2 = r1 + goldenGamma;
    (r1, r2)

/// Splitting a random value using Linear Congruential Generator.
def split(r: Rand): (Rand, Rand) =
    if (r == 0)
        split(1)
    else
        let r1 = Int32.mod(17364 * r, 65521);
        let r2 = Int32.mod(33285 * r, 65521);
        (r1, r2)

 def testNTimes(n: Int32, seed: Rand, gen: Gen[a]): Unit \ IO with Arbitrary[a], ToString[a] =
    match n {
        case 0 => 
            println("Test finished!")
        case _ => 
            let (r1, r2) = split(seed);
            println(applyGen(gen, r1));
            testNTimes(n - 1, r2, gen)
    }

/// The main entry point.
def main(): Unit \ IO =
    // let gen: Gen[(List[Int32])] = Arbitrary.arbitrary();
    let gen: Gen[(Bool, Int32)] = Arbitrary.arbitrary();
    testNTimes(10, 7, gen)