pub type alias Rand = Int32

pub enum Gen[a](Rand -> a)

///
/// Applies a generator `g` to a random number `r` and returns the generated value.
///
/// `g` the generator to apply.
///
/// `r` the random number.
///
/// returns the generated value.
///
pub def applyGen(g: Gen[a], r: Rand): a =
    match g {
        case Gen.Gen(f) => f(r)
    }


/// Returns a Generator that uniformly generates values in the interval [lower, upper]
pub def chooseInt(lower: Int32, upper: Int32): Gen[Int32] = 
    Gen.Gen(rand -> Int32.modulo(rand, (upper + 1) - lower) + lower)

pub def chooseIntNew(lower: Int32, upper: Int32): Gen[Int32] = 
    ???

/// Returns a generator that uniformly generates a boolean value.
pub def chooseBool(): Gen[Bool] =
    elements(Nel.Nel(true, false :: Nil))

    
pub def chooseFloat(lower: Int32, upper: Int32): Gen[Float64] =
    Gen.Gen(rand -> 
        let (r1, rsplit) = splitRand(rand);
        let (r2, r3) = splitRand(rsplit);
        let intGen = chooseInt(lower, upper - 1);
        
        let fracGen = chooseInt(0, Int32.maxValue() - 1000000); // very semi random number
        // debug("r2: ${r2}");
        // debug("fracGen:  ${applyGen(fracGen, r2)}");
        // debug(Int32.maxValue());
        debug("Int32.toFloat64(applyGen(intGen, r1)): ${Int32.toFloat64(applyGen(intGen, r1))}");
        debug("(Int32.toFloat64(applyGen(fracGen, r2)): ${Int32.toFloat64(applyGen(fracGen, r2))}");
        debug("((Int32.toFloat64(Int32.abs(applyGen(fracGen, r3)) + 1)): ${(Int32.toFloat64(Int32.abs(applyGen(fracGen, r3)) + 1))}");
        
        // fraction a b c = fromInteger a + (fromInteger b / (abs (fromInteger c) + 1))
        // TODO FIX
        let a = Int32.toFloat64(applyGen(intGen, r1));
        let b = (Int32.toFloat64(applyGen(fracGen, r2)));
        let c = (Int32.toFloat64(applyGen(fracGen, r3) + 1));
    
        
        let res = a + b / Float64.abs(c) ;
        res
    )

/// Returns a generator that uniformly generates a lower case character.
pub def chooseLowerChar(): Gen[Char] =
    elements(Nel.Nel('a', 
    'b' :: 'c' :: 'd' :: 'e' :: 'f' :: 'g' :: 'h' :: 
    'i' :: 'j' :: 'k' :: 'l' :: 'm' :: 'n' :: 'o' :: 
    'p' :: 'q' :: 'r' :: 's' :: 't' :: 'u' :: 'v' :: 
    'w' :: 'x' :: 'y' :: 'z' :: Nil))

/// Returns a generator that uniformly generates an upper case character.
pub def chooseUpperChar(): Gen[Char] =
    elements(Nel.Nel('A', 
    'B' :: 'C' :: 'D' :: 'E' :: 'F' :: 'G' :: 'H' ::
    'I' :: 'J' :: 'K' :: 'L' :: 'M' :: 'N' :: 'O' ::
    'P' :: 'Q' :: 'R' :: 'S' :: 'T' :: 'U' :: 'V' ::
    'W' :: 'X' :: 'Y' :: 'Z' :: Nil))

    

/// Returns a generator that generates the value `a`.
pub def return(x: a): Gen[a] = 
    Gen.Gen(_ -> x)


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


pub def elements(xs: Nel[a]): Gen[a] =
    let v = Nel.toVector(xs);
    let size = Vector.length(v);
    Gen.Gen(rand -> Vector.get(applyGen(chooseInt(0, size - 1), rand), v))

pub def oneOf(xs: Nel[Gen[a]]): Gen[a] =
    let v = Nel.toVector(xs);
    let size = Vector.length(v);
    Gen.Gen(rand -> applyGen(Vector.get(applyGen(chooseInt(0, size - 1), rand), v), rand))

pub def frequency(xs: Nel[(Int32, Gen[a])]): Gen[a] =
    Gen.Gen(rand ->
        let total = Nel.foldLeft((acc, x) -> acc + fst(x), 0, xs);
        let (r1, r2) = splitRand(rand);
        applyGen(pick(applyGen(chooseInt(1, total), r1), Nel.toList(xs)), r2)
    )
    

pub def pick(n: Int32, xs: List[(Int32, Gen[a])]): Gen[a] =
    match List.head(xs) {
        case None => unreachable!(),
        case Some((k, gen)) => 
            if (n <= k)
                gen
            else
                pick(n-k, List.drop(1, xs))
    }

/// Generates a random pair of values using two generators.
pub def randomPair(aGen: Gen[a], bGen: Gen[b]): Gen[(a, b)] =
    Gen.Gen(rand -> 
        let (r1, r2) = splitRand(rand);
        (applyGen(aGen, r1), applyGen(bGen, r2))
    )
    
/// Should produce a generator of a list of values
pub def randomList(aGen: Gen[a]): Gen[List[a]] =
    Gen.Gen(rand ->
        let (r1, r2) = splitRand(rand);
        match applyGen(chooseBool(), rand) {
            case true => applyGen(aGen, r1) :: applyGen(randomList(aGen), r2),
            case false => Nil
        }
    )

/// Should produce a generator that generates a list of values, evaluating lazily
pub def randomListLazy(aGen: Gen[a]): Gen[List[a]] =
    let cons : Gen[List[a]] = 
    Gen.Gen(r -> 
        let (r1, r2) = splitRand(r);
        applyGen(aGen, r1) :: applyGen(randomListLazy(aGen), r2));
    let ret: Gen[List[a]] = return(Nil);
    oneOf(Nel.Nel(ret, cons :: Nil))

/// Should generate a list using some frequency weights  
pub def randomListNew(aGen: Gen[a]): Gen[List[a]] =
    Gen.Gen(rand ->
        let (r1, r2) = splitRand(rand);
        let nel: Nel[(Int32, Gen[List[a]])] = Nel.Nel((1, return(Nil)), (10, return(applyGen(aGen, r1) :: applyGen(randomListNew(aGen), r2))) :: Nil);
        applyGen(frequency(nel), r1)
    )


// Arbitrary instances
pub trait Arbitrary[a] {
    pub def arbitrary(): Gen[a]
}

instance Arbitrary[Unit] {
    pub def arbitrary(): Gen[Unit] =
        return()
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

instance Arbitrary[Float64] {
    pub def arbitrary(): Gen[Float64] = 
        chooseFloat(0, 1)
}

instance Arbitrary[(a, b)] with Arbitrary[a], Arbitrary[b] {
    pub def arbitrary(): Gen[(a, b)] =
        randomPair(Arbitrary.arbitrary(), Arbitrary.arbitrary())
}

instance Arbitrary[List[a]] with Arbitrary[a] {
    pub def arbitrary(): Gen[List[a]] =
        randomList(Arbitrary.arbitrary())
}

/// Splitting a random value using Linear Congruential Generator.
def splitRand(r: Rand): (Rand, Rand) =
    if (r == 0)
        splitRand(1)
    else
        let r1 = Int32.modulo(17364 * r, 65521);
        let r2 = Int32.modulo(43165 * r, 131071);
        (r1, r2)


def testNTimes(n: Int32, r: Rand, gen: Gen[a], verbose: Bool): List[a] \ IO with Arbitrary[a], ToString[a] =
    match n {
        case 0 => 
            println("Test finished!");
            Nil
        case _ => 
            let (r1, r2) = splitRand(r);
            let res = applyGen(gen, r1);
            if (verbose) 
                println(res)
            else 
                ();
            res :: testNTimes(n - 1, r2, gen, verbose)
}

/// The main entry point.
def main(): Unit \ IO =
    let gen: Gen[List[Int32]] = randomListNew(chooseInt(0, 10));
    let _ = testNTimes(10, 70017, gen, true);
    println("\n")