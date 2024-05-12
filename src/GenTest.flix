mod GenTest {
    use SplittableRandom.{split, nextBool, nextInt32, nextFloat64}

    pub type alias Rand = SplittableRandom

    pub enum Gen[r: Region, a: Type](
        Int32 -> Region[r] -> Rand[r] -> a \ r
    )

    instance Functor[Gen[r]] {
        pub def map(f: a -> b \ ef, aGen: Gen[r, a]): Gen[r, b] \ ef = 
            checked_ecast(Gen.Gen(size -> rc -> rand -> 
                unchecked_cast(f(applyGen(aGen, size, rc, rand)) as _ \ r)))
    }

    instance Applicative[Gen[r]] {
        pub def ap(fGen: Gen[r, a -> b \ ef], aGen: Gen[r, a]): Gen[r, b] \ ef = 
            checked_ecast(Gen.Gen(size -> rc -> rand ->
                let (r1, r2) = split(rand); 
                let func = applyGen(fGen, size, rc, r1);
                unchecked_cast(func(applyGen(aGen, size, rc, r2)) as _ \ r)))

        pub def point(x: a): Gen[r, a] = 
            return(x)
    }

    instance Monad[Gen[r]] {
        pub def flatMap(f: a -> Gen[r, b] \ ef, aGen: Gen[r, a]): Gen[r, b] \ ef = 
            checked_ecast(Gen.Gen(size -> rc -> rand -> 
                let (r1, r2) = split(rand);
                unchecked_cast(
                    let Gen.Gen(func) = f(applyGen(aGen, size, rc, r1));
                    func(size, rc, r2) as _ \ r)
            )
            )
    }

    pub def return(x: a): Gen[r, a] = 
        Gen.Gen(_ -> _ -> _ -> unchecked_cast(x as _ \ r))

    pub def applyGen(gen: Gen[r, a], size: Int32, rc: Region[r], rand: Rand[r]): a \ {r} =
        match gen {
            case Gen.Gen(g) => g(size, rc, rand)
        }

    pub trait Arbitrary[a] {
        pub def arbitrary(): Gen[r, a]
    }

    instance Arbitrary[Int32] {
        pub def arbitrary(): Gen[r, Int32] = 
            Gen.Gen(_ -> _ -> rand -> SplittableRandom.nextInt32(rand, 5, 6))
    }

    instance Arbitrary[Array[a, r1]] with Arbitrary[a] {
        pub def arbitrary(): Gen[r, Array[a, r]] = 
            Gen.Gen(_ -> rc -> _ -> Array.empty(rc, 5))
    }
}