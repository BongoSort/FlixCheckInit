// Definitions for List.*
pub def runAllTests(): Unit \ IO =
    testAp();
    testAppend();
    testCount();
    testDistinct();
    testDestinctWith();
    testDrop();
    testDropWhile();
    testReverse()

// ap
pub def testAp(): Unit \ IO =
    // Adding two lists element-wise in two different ways should give the same result
    def propAp(xs: List[Int32], ys: List[Int32]) = List.ap(List.map(x -> y -> x + y)(xs), ys) == List.flatMap(x -> List.map(y -> x + y)(ys))(xs);
    def propAp2(xs: List[Float64], ys: List[Float64]) = List.ap(List.map(x -> y -> x + y)(xs), ys) == List.flatMap(x -> List.map(y -> x + y)(ys))(xs);
    def propAp3(xs: List[Bool], ys: List[Bool]) = List.ap(List.map(x -> y -> x and y)(xs), ys) == List.flatMap(x -> List.map(y -> x and y)(ys))(xs);
    def propAp4(xs: List[String], ys: List[String]) = List.ap(List.map(x -> y -> x + y)(xs), ys) == List.flatMap(x -> List.map(y -> x + y)(ys))(xs);
    // Property for Char is slightly different, as we can't add chars
    def propAp5(xs: List[Char], ys: List[Char]) = List.ap(List.map(x -> y -> x == y)(xs), ys) == List.flatMap(x -> List.map(y -> x == y)(ys))(xs);
    let propList = forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propAp) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propAp2) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propAp3) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propAp4) ::
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propAp5) :: Nil;
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
    // testing with different types
    def propAppend4(xs: List[Char], ys: List[Char]) = List.append(xs, ys) == List.foldLeft((acc, x) -> List.append(acc, List.point(x)), List.empty(), xs ++ ys);
    def propAppend5(xs: List[String], ys: List[String]) = List.append(xs, ys) == List.foldLeft((acc, x) -> List.append(acc, List.point(x)), List.empty(), xs ++ ys);
    def propAppend6(xs: List[Bool], ys: List[Bool]) = List.append(xs, ys) == List.foldLeft((acc, x) -> List.append(acc, List.point(x)), List.empty(), xs ++ ys);
    def propAppend7(xs: List[Float64], ys: List[Float64]) = List.append(xs, ys) == List.foldLeft((acc, x) -> List.append(acc, List.point(x)), List.empty(), xs ++ ys);
    // Slight bug/ inconvenience : we can only combine TFuncs of the same type.
    let propList : List[TFunc2[List[Int32], List[Int32], Bool]]= TFunc2.TFunc2(propAppend) :: TFunc2.TFunc2(propAppend2) :: TFunc2.TFunc2(propAppend3) :: Nil;
    let otherProps = forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propAppend4) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propAppend5) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propAppend6) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propAppend7) :: Nil;
    foreach (prop <- propList) {
        flixCheck(prop)
    };
    foreach (oProp <- otherProps) {
        flixCheck(oProp)
    }
    
// count
pub def testCount(): Unit \ IO =
    // Property that checks if the count of all elements in a list is the same as the length of the list
    def propCount(xs: List[Bool]) = List.count(x -> x == x, xs) == List.length(xs);
    def propCount2(xs: List[Char]) = List.count(x -> x == x, xs) == List.length(xs);
    def propCount3(xs: List[Float64]) = List.count(x -> x == x, xs) == List.length(xs);
    def propCount4(xs: List[Int32]) = List.count(x -> x == x, xs) == List.length(xs);
    def propCount5(xs: List[String]) = List.count(x -> x == x, xs) == List.length(xs);
    let propList: List[Property] = forAll(Arbitrary.arbitrary(), propCount) :: 
                    forAll(Arbitrary.arbitrary(), propCount2) :: 
                    forAll(Arbitrary.arbitrary(), propCount3) :: 
                    forAll(Arbitrary.arbitrary(), propCount4) :: 
                    forAll(Arbitrary.arbitrary(), propCount5) :: Nil;
    foreach (prop <- propList) {
        flixCheck(prop)
    }

// distinct
pub def testDistinct(): Unit \ IO =
    // Property that checks if the distinct of a list is the same as the list with duplicates removed
    def propDistinct(xs: List[Bool]) = List.distinct(xs) == List.reverse(List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs));
    def propDistinct2(xs: List[Char]) = List.distinct(xs) == List.reverse(List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs));
    def propDistinct3(xs: List[Float64]) = List.distinct(xs) == List.reverse(List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs));
    def propDistinct4(xs: List[Int32]) = List.distinct(xs) == List.reverse(List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs));
    def propDistinct5(xs: List[String]) = List.distinct(xs) == List.reverse(List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs));
    let propList = forAll(Arbitrary.arbitrary(), propDistinct) :: 
                    forAll(Arbitrary.arbitrary(), propDistinct2) :: 
                    forAll(Arbitrary.arbitrary(), propDistinct3) :: 
                    forAll(Arbitrary.arbitrary(), propDistinct4) :: 
                    forAll(Arbitrary.arbitrary(), propDistinct5) :: Nil;
    foreach (prop <- propList) {
        flixCheck(prop)
    }

// distinctWith
pub def testDestinctWith():Unit \ IO =
    // distinctWith Returns the list l with duplicates removed using the supplied function f for comparison. 
    // The first occurrence of an element is kept and except for the removal of subsequent duplicates the order of l is preserved.
    // Property that checks if the distinct of a list is the same as the list with duplicates removed
    def propDistinctWith(xs: List[Int32]) = List.distinctWith((x, y) -> x == y, xs) == List.reverse(List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs));
    let propList = forAll(Arbitrary.arbitrary(), propDistinctWith) :: Nil;
    foreach (prop <- propList) {
        flixCheck(prop)
    }

// drop
pub def testDrop(): Unit \ IO =
    // drop Returns l without the first n elements.
    def propDrop(n : Int32, xs: List[Int32]) = 
    match (n, List.length(xs)) {
            case (_, len) if n > len => List.drop(n, xs) == Nil
            case (_, _) if n < 0 => List.drop(n, xs) == xs
            case (_, len) => List.length(List.drop(n, xs)) == len - n
        };
    let propList = forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propDrop) :: Nil;
    foreach (prop <- propList) {
        flixCheck(prop)
    }

// dropWhile
pub def testDropWhile(): Unit \ IO =
    // dropWhile Returns the longest prefix of l whose elements satisfy the predicate f.
    def dropWhileTester(xs: List[Int32]): List[Int32] =
        match xs {
            case Nil => Nil
            case h :: t => if (h < 5) dropWhileTester(t) else xs
    };
    def propDropWhile(xs: List[Int32]) = List.dropWhile(x -> x < 5, xs) == dropWhileTester(xs);
    def propDropWhile2(xs: List[Int32]) = List.length(List.dropWhile(x -> x < 0, xs)) <= List.length(xs); 
    let propList = forAll(Arbitrary.arbitrary(), propDropWhile) :: forAll(Arbitrary.arbitrary(), propDropWhile2) :: Nil;
    foreach (prop <- propList) {
        flixCheck(prop)
    }

// empty
pub def testEmpty(): Unit \ IO =
    // Property that checks if the empty list is indeed empty
    def propEmpty() = List.isEmpty(List.empty()) == true;
    def propEmpty2() = List.length(List.empty()) == 0;
    let propList = propEmpty() :: propEmpty2() :: Nil;
    foreach (prop <- propList) {
        flixCheck(prop)
    }

// enumerator
// TODO ##########################
//  Returns an iterator over l zipped with the indices of the elements.
// Implementation:
// pub def enumerator(rc: Region[r], l: List[a]): Iterator[(Int32, a), r, r] \ r =
//     iterator(rc, l) |> Iterator.zipWithIndex

pub def testT(): Unit \ IO  =
    let l = 1 :: 5 :: 3 :: 1 :: Nil;
    println(l)
   

// exists
pub def testExists(): Unit \ IO =
    // Property that checks if the exists function returns true if the predicate is true for at least one element
    def propExists(xs: List[Int32]) = List.exists(x -> x == 0, xs) == List.foldLeft((acc, x) -> acc or x == 0, false, xs);
    def propExists2(xs: List[Char]) = List.exists(x -> x == 'a', xs) == List.foldLeft((acc, x) -> acc or x == 'a', false, xs);
    def propExists3(xs: List[Float64]) = List.exists(x -> x == 0.0, xs) == List.foldLeft((acc, x) -> acc or x == 0.0, false, xs);
    def propExists4(xs: List[String]) = List.exists(x -> x == "", xs) == List.foldLeft((acc, x) -> acc or x == "", false, xs);
    def propExists5(xs: List[Bool]) = List.exists(x -> x == true, xs) == List.foldLeft((acc, x) -> acc or x == true, false, xs);
    let propList = forAll(Arbitrary.arbitrary(), propExists) :: 
                    forAll(Arbitrary.arbitrary(), propExists2) :: 
                    forAll(Arbitrary.arbitrary(), propExists3) :: 
                    forAll(Arbitrary.arbitrary(), propExists4) :: 
                    forAll(Arbitrary.arbitrary(), propExists5) :: Nil;
    foreach (prop <- propList) {
        flixCheck(prop)
    }
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
