// Definitions for List.*
// ap
// append
// count
// distinct
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