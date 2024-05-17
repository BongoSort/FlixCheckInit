// Definitions for List.*
pub def runAllTests(): Unit \ IO =
    testAp();
    testAppend();
    testCount();
    testDistinct();
    testReverse()
// ap
pub def testAp(): Unit \ IO =
    // Adding two lists element-wise in two different ways should give the same result
    def propAp(xs: List[Int32], ys: List[Int32]) = List.ap(List.map(x -> y -> x + y)(xs), ys) == List.flatMap(x -> List.map(y -> x + y)(ys))(xs);
    let propList = forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propAp) :: Nil;
    foreach (prop <- propList) {
        flixCheck(prop)
    }
// append
pub def testAppend(): Unit \ IO =
    // Testing the ++ operator
    def propAppend(xs: List[Int32], ys: List[Int32]) = List.append(xs, ys) == xs ++ ys;
    // Testing the infix operator
    def propAppend2(xs: List[Int32], ys: List[Int32]) = List.append(xs, ys) == xs ::: ys;
    // Appending should give the same result as folding over the list with the append function
    def propAppend3(xs: List[Int32], ys: List[Int32]) = List.append(xs, ys) == List.foldLeft((acc, x) -> List.append(acc, List.point(x)), List.empty(), xs ++ ys);
    let propList = TFunc2.TFunc2(propAppend) :: TFunc2.TFunc2(propAppend2) :: TFunc2.TFunc2(propAppend3) :: Nil;
    foreach (prop <- propList) {
        flixCheck(prop)
    }
// count
pub def testCount(): Unit \ IO =
    // Property that checks if the count of all elements in a list is the same as the length of the list
    def propCount(xs: List[Int32]) = List.count(x -> x == x, xs) == List.length(xs);
    let propList = TFunc.TFunc(propCount) :: Nil;
    foreach (prop <- propList) {
        flixCheck(prop)
    }
// distinct
pub def testDistinct(): Unit \ IO =
    // Property that checks if the distinct of a list is the same as the list with duplicates removed
    def propDistinct(xs: List[Int32]) = List.distinct(xs) == List.reverse(List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs));
    def propDistinct2(xs: List[Float64]) = List.distinct(xs) == List.reverse(List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs));
    def propDistinct3(xs: List[String]) = List.distinct(xs) == List.reverse(List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs));
    def propDistinct4(xs: List[Bool]) = List.distinct(xs) == List.reverse(List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs));
    def propDistinct5(xs: List[Char]) = List.distinct(xs) == List.reverse(List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs));
    
    // let propList = TFunc.TFunc(propDistinct):: Nil; // TODO vi kan kun tage Ints i TFunc
    
    let propList = forAll(Arbitrary.arbitrary(), propDistinct) :: 
                    forAll(Arbitrary.arbitrary(), propDistinct2) :: 
                    forAll(Arbitrary.arbitrary(), propDistinct3) :: 
                    forAll(Arbitrary.arbitrary(), propDistinct4) :: 
                    forAll(Arbitrary.arbitrary(), propDistinct5) :: Nil;
    foreach (prop <- propList) {
        flixCheck(prop)
    }
pub def testTest(): Unit \ IO =
    let xs = List#{"hej", "med", "dig", "hej"};
    // let ys = 4 :: 5 :: 6 :: Nil;
    println(List.reverse(List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs)));
    println(List.distinct(xs))
// distinctWith
// drop
// dropWhile
// empty
// enumerator
// exists
// filter
// filterMap
// find
// findLeft
// findMap
// findRight
// flatMap
// flatten
// fold
// fold2
// foldLeft
// foldLeft2
// foldMap
// foldRight
// foldRight2
// foldRightWithCont
// forAll
// forEach
// forEachWithIndex
// frequency
// groupBy
// head
// indexOf
// init
// intercalate
// intersperse
// isEmpty
// isInfixOf
// isPrefixOf
// isSuffixOf
// iterator
// join
// joinWith
// last
// length
// map
// map2
// map3
// map4
// map5
// mapWithIndex
// maximum
// maximumBy
// memberOf
// merge
// minimum
// minimumBy
// nonEmpty
// partition
// patch
// permutations
// point
// range
// reduceLeft
// reduceRight
// repeat
// replace
// reverse
def testReverse(): Unit \ IO =
    // Property that checks if the reverse of a list is the same as the original list
    def propRevRev(xs: List[Char]) = List.reverse(List.reverse(xs)) == xs;
    // Property that checks if the reverse of a single element list is the same as the original list
    def propRev(x: Int32) = (x :: Nil) == List.reverse(x :: Nil);
    // Property that checks if the reverse of the concatenation of two lists is the same as the concatenation of the reversed lists
    def propRevApp(pair: (List[Int32], List[Int32])) = List.append(List.reverse(fst(pair)), List.reverse(snd(pair))) == List.reverse(List.append(snd(pair), fst(pair)));
    let propList = forAll(Arbitrary.arbitrary(), propRevRev) :: forAll(Arbitrary.arbitrary(), propRev) :: forAll(Arbitrary.arbitrary(), propRevApp) :: Nil;
    foreach (prop <- propList) {
        flixCheck(prop)
    }
// rotateLeft
// rotateRight
// scan
// scanLeft
// scanRight
// sequence
// shuffle
// slice
// sort
// sortBy
// sortWith
// span
// splitAt
// subsequences
// sum
// sumWith
// take
// takeWhile
// toArray
// toChain
// toDelayList
// toDelayMap
// toMap
// toMapWith
// toMutDeque
// toMutList
// toNec
// toNel
// toSet
// toString
// toVector
// transpose
// traverse
// unfold
// unfoldWithIter
// unfoldWithOkIter
// unzip
// unzip3
// update
// zip
// zip3
// zipWith
// zipWith3
// zipWithA
// zipWithIndex
