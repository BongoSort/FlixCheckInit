use Random.Random

pub type alias Rand = Int32

pub enum Gen[a](Rand -> a)

// TODO: Assert safe bounds!
/// Returns a Generator that uniformly generates values in the interval [lower, upper]
pub def chooseInt(lower: Int32, upper: Int32): Gen[Int32] = 
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

pub def return(a: a): Gen[a] = 
    Gen.Gen(_ -> a)

pub def bind(gen: Gen[a], fun: a -> Gen[b]): Gen[b] =
    Gen.Gen(
        rand ->
            match gen {
                case Gen.Gen(g) => 
                    let res: a = g(rand);
                    let newGen: Gen[b]  = fun(res);
                    match newGen {
                        case Gen.Gen(f) => f(rand)
                    }
        }
    )

pub def liftM2(f: a -> b -> c, genA: Gen[a], genB: Gen[b]): Gen[c] =
    bind(genA, a -> bind(genB, b -> return(f(a)(b))))


pub def oneOf(xs: List[Gen[a]]): Gen[a] =
    match xs {
        case Nil => bug!("Method oneOf called on empty list!") //Placeholder errorhandling
        case _ :: _ => 
            let v = List.toVector(xs);
            let size = Vector.length(v);
            bind(chooseInt(0, size - 1), i -> Vector.get(i, v))
    }
    


// Arbitrary instances
pub trait Arbitrary[a] {
    pub def arbitrary(): Gen[a]
}
pub trait Coarbitrary[a] {
    pub def coarbitrary(): Gen[(a, b)]
}

instance Arbitrary[Int32] {
    pub def arbitrary(): Gen[Int32] = chooseInt(-20, 20)
}

// instance Arbitrary[(a, b)] {
//     pub def arbitrary(): Gen[(a,b)] = chooseInt2(-20, 20)

// }


instance Arbitrary[Bool] {
    pub def arbitrary(): Gen[Bool] =
        oneOf(return(true) :: return(false) :: Nil)
}

instance Arbitrary[Char] {
    pub def arbitrary(): Gen[Char] = 
        oneOf(
            List.map(x -> return(x), 
            ('a' :: 'b' :: 'c' :: 'd' :: 'e' :: 
             'f' :: 'g' :: 'h' :: 'i' :: 'j' :: 
             'k' :: 'l' :: 'm' :: 'n' :: 'o' :: 
             'p' :: 'q' :: 'r' :: 's' :: 't' :: 
             'u' :: 'v' :: 'w' :: 'x' :: 'y' :: 
             'z' :: Nil))
             )
}


instance Arbitrary[(a, b)] {
    pub def arbitrary(): Gen[(a, b)] =
        let aGen: Gen[a] = Arbitrary.arbitrary();
        let bGen: Gen[b] = Arbitrary.arbitrary();
        let func = a -> b -> (a, b);
        liftM2(func, aGen, bGen)
}

// instance Arbitrary[a] {
//     pub def arbitrary(): Gen[List[a]] =
//         let aGen: Gen[a] = Arbitrary.arbitrary();
//         oneOf(
//             return(Nil) :: Nil
//         )
// }

// The main entry point.
def main(): Unit \ IO =
    let seed = 0;
    let g: Gen[Int32] = Arbitrary.arbitrary();
    let res = match g {case Gen.Gen(f) => f(seed)};
    println(res)




