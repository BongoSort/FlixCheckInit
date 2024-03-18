@Test
def test01(): Bool = 1 + 1 == 2

def test02(): Bool = true

def seedParseTest(): Bool \ IO =
    let seed = Random.nextInt32(Random.fresh());
    seed == parseSeed(toBinaryString(seed))

// Testing the statistical distribution of the random number generator
// The sums should be close to 0, but for now they are not. This is a known issue.
def testListWithBoolsAndInts(): Unit \ IO =
    let seeds = List#{12345, 54321, 97531, 8642, 55555};
    foreach(seed <- seeds)
        let gen: Gen[(Bool, Int32)] = Arbitrary.arbitrary();
        
        let l: List[(Bool, Int32)] = testNTimes(2000, seed, gen, false);
        
        let intlist : List[Int32] = List.map(x -> snd(x), l);
        let boollist : List[Bool] = List.map(x -> fst(x), l);
        let sumForInt = List.foldLeft((acc, x) -> acc + x, 0, intlist);
        let sumForBool = List.foldLeft((acc, x) -> if (x) acc + 1 else acc - 1, 0, boollist);
        println(sumForInt);
        println(sumForBool)

/// testing the distribution of the random number generator, but with the new verbose printing
def testGenIntListVerbose(): Unit \ IO =
    let gen: Gen[List[Int32]] = Arbitrary.arbitrary();
    let _ = testNTimes(10, 70000, gen, true);
    println("Done")