pub type alias Rand = Int32

//pub enum Gen[a](Rand -> a)
pub enum Gen[a](Int32 -> Rand -> a)

///
/// Applies a generator `g` to a random number `r` and returns the generated value.
///
/// `g` the generator to apply.
///
/// `r` the random number.
///
/// returns the generated value.
///
pub def applyGen(g: Gen[a], size: Int32, r: Rand): a =
    match g {
        case Gen.Gen(f) => f(size, r)
    }


/// Returns a Generator that uniformly generates values in the interval [lower, upper]
pub def chooseInt(lower: Int32, upper: Int32): Gen[Int32] = 
    Gen.Gen(_ -> rand -> Int32.modulo(rand, (upper + 1) - lower) + lower)

/// Returns a generator that uniformly generates a boolean value.
pub def chooseBool(): Gen[Bool] =
    elements(Nel.Nel(true, false :: Nil))

    
// pub def chooseFloat(lower: Int32, upper: Int32): Gen[Float64] =
//     Gen.Gen(rand -> 
//         let (r1, rsplit) = splitRand(rand);
//         let (r2, r3) = splitRand(rsplit);
//         let intGen = chooseInt(lower, upper - 1);
        
//         let fracGen = chooseInt(0, Int32.maxValue() - 1000000); // very semi random number
//         // debug("r2: ${r2}");
//         // debug("fracGen:  ${applyGen(fracGen, r2)}");
//         // debug(Int32.maxValue());
//         debug("Int32.toFloat64(applyGen(intGen, r1)): ${Int32.toFloat64(applyGen(intGen, r1))}");
//         debug("(Int32.toFloat64(applyGen(fracGen, r2)): ${Int32.toFloat64(applyGen(fracGen, r2))}");
//         debug("((Int32.toFloat64(Int32.abs(applyGen(fracGen, r3)) + 1)): ${(Int32.toFloat64(Int32.abs(applyGen(fracGen, r3)) + 1))}");
        
//         // fraction a b c = fromInteger a + (fromInteger b / (abs (fromInteger c) + 1))
//         // TODO FIX
//         let a = Int32.toFloat64(applyGen(intGen, r1));
//         let b = (Int32.toFloat64(applyGen(fracGen, r2)));
//         let c = (Int32.toFloat64(applyGen(fracGen, r3) + 1));
    
        
//         let res = a + b / Float64.abs(c) ;
//         res
//     )

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
    Gen.Gen(_ -> _ -> x)


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
        size -> rand ->
            let newGen: Gen[b] = fun(applyGen(aGen, size, rand));
            applyGen(newGen, size, rand)
    )

/// Temporary lift monadic function with two arguments
pub def liftM2(f: a -> b -> c, aGen: Gen[a], bGen: Gen[b]): Gen[c] =
    bind(aGen, a -> bind(bGen, b -> return(f(a, b))))


pub def sized(f: Int32 -> Gen[a]): Gen[a] =
    Gen.Gen(
        size -> rand ->
        let gen: Gen[a] = f(size);
        applyGen(gen, size, rand)
    )


pub def elements(xs: Nel[a]): Gen[a] =
    let v = Nel.toVector(xs);
    let vsize = Vector.length(v);
    Gen.Gen(size -> rand -> Vector.get(applyGen(chooseInt(0, vsize - 1), size, rand), v))

pub def oneOf(xs: Nel[Gen[a]]): Gen[a] =
    let v = Nel.toVector(xs);
    let vsize = Vector.length(v);
    Gen.Gen(size -> rand -> applyGen(Vector.get(applyGen(chooseInt(0, vsize - 1), size, rand), v), size, rand))

pub def frequency(xs: Nel[(Int32, Gen[a])]): Gen[a] =
    Gen.Gen(size -> rand ->
        let total = Nel.foldLeft((acc, x) -> acc + fst(x), 0, xs);
        let (r1, r2) = splitRand(rand);
        applyGen(pick(applyGen(chooseInt(1, total), size, r1), Nel.toList(xs)), size, r2)
    )
    
pub def pick(n: Int32, xs: List[(Int32, Gen[a])]): Gen[a] =
    match List.head(xs) {
        case None => unreachable!(),
        case Some((k, gen)) if (n <= k) => 
            gen,
        case Some((k, _)) =>
            pick(n-k, List.drop(1, xs))
    }

/// Generates a random pair of values using two generators.
pub def randomPair(aGen: Gen[a], bGen: Gen[b]): Gen[(a, b)] =
    Gen.Gen(size -> rand -> 
        let (r1, r2) = splitRand(rand);
        (applyGen(aGen, size, r1), applyGen(bGen, size, r2))
    )
/// Generates a random triple of values using three generators.
pub def randomTriple(aGen: Gen[a], bGen: Gen[b], cGen: Gen[c]): Gen[(a, b, c)] =
    Gen.Gen(size -> rand -> 
        let (r1, r2) = splitRand(rand);
        let (r3, r4) = splitRand(r1);
        (applyGen(aGen, size, r2), applyGen(bGen, size, r3), applyGen(cGen, size, r4))
    )
/// Generates a random quadrouple of values using four generators.
pub def randomQuadruple(aGen: Gen[a], bGen: Gen[b], cGen: Gen[c], dGen: Gen[d]): Gen[(a, b, c, d)] =
    Gen.Gen(size -> rand -> 
        let (r1, r2) = splitRand(rand);
        let (r3, r4) = splitRand(r1);
        let (r5, r6) = splitRand(r2);
        (applyGen(aGen, size, r3), applyGen(bGen, size, r4), applyGen(cGen, size, r5), applyGen(dGen, size, r6))
    )
    
/// Should produce a generator of a list of values with average length 2
pub def randomList(aGen: Gen[a]): Gen[List[a]] =
    Gen.Gen(size -> rand ->
        let (r1, r2) = splitRand(rand);
        match applyGen(chooseBool(), size, rand) {
            case true => applyGen(aGen, size, r1) :: applyGen(randomList(aGen), size, r2),
            case false => Nil
        }
    )

pub def randomListSized(aGen: Gen[a], length: Int32): Gen[List[a]] =
    Gen.Gen(size -> rand -> 
        let (r1, r2) = splitRand(rand);
        if (length <= 0) 
            Nil
        else
            applyGen(aGen, size, r1) :: applyGen(randomListSized(aGen, length-1), size, r2)
    )

/// Should generate a list using average length for controlling the generated list size.
pub def randomListSizedAvg(aGen: Gen[a], avgLength: Int32, bound: Int32): Gen[List[a]] =
    Gen.Gen(size -> rand ->
        let (r1, r2) = splitRand(rand);
        if (bound <= 0) 
            Nil
        else
            match applyGen(chooseInt(0, avgLength), size, rand) {
                case 0 => Nil,
                case _ => applyGen(aGen, size, r1) :: applyGen(randomListSizedAvg(aGen, avgLength, bound-1), size, r2)
            }
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
        sized(size -> chooseInt(-size, size))
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
        ???
        // chooseFloat(0, 1)
}

instance Arbitrary[(a, b)] with Arbitrary[a], Arbitrary[b] {
    pub def arbitrary(): Gen[(a, b)] =
        randomPair(Arbitrary.arbitrary(), Arbitrary.arbitrary())
}

instance Arbitrary[(a, b, c)] with Arbitrary[a], Arbitrary[b], Arbitrary[c] {
    pub def arbitrary(): Gen[(a, b, c)] =
        randomTriple(Arbitrary.arbitrary(), Arbitrary.arbitrary(), Arbitrary.arbitrary())
}

instance Arbitrary[(a, b, c, d)] with Arbitrary[a], Arbitrary[b], Arbitrary[c], Arbitrary[d] {
    pub def arbitrary(): Gen[(a, b, c, d)] =
        randomQuadruple(Arbitrary.arbitrary(), Arbitrary.arbitrary(), Arbitrary.arbitrary(), Arbitrary.arbitrary())
}

instance Arbitrary[List[a]] with Arbitrary[a] {
    pub def arbitrary(): Gen[List[a]] =
        sized(size -> randomListSized(Arbitrary.arbitrary(), size))
}

/// Splitting a random value using Linear Congruential Generator.
def splitRand(r: Rand): (Rand, Rand) =
    if (r == 0)
        splitRand(1)
    else
        let r1 = Int32.modulo(17364 * r, 65521);
        let r2 = Int32.modulo(43165 * r, 131071);
        (r1, r2)




/// The main entry point.
def main(): Unit \ IO =
    let gen: Gen[List[Int32]] = Arbitrary.arbitrary();
    let ll = testNTimes(100, 100, 70052, gen, true);
    let lengthsum = List.foldLeft((acc, x) -> acc + List.length(x), 0, ll);

    println(lengthsum)
    // let gen: Gen[Int32] = Arbitrary.arbitrary();
    // let _ = testNTimes(100, 100, 70035, gen, true);
    // println("\n")