
use SplittableRandom.{freshWithSeed, fresh};
use Monad.{>>=}

pub enum Tree[a] {
    case Leaf(a),
    case Branch(Tree[a], Tree[a])
}

pub def chooseTreeLazy(gen: Gen[a], maxNodes: Int32): Gen[Tree[a]] =
    if (maxNodes <= 1) {
        Functor.map(Tree.Leaf, gen)
    } else {
        lazyFrequency(
            Nel.Nel(
                (1, lazy Functor.map(Tree.Leaf, gen)),
                (2, lazy Applicative.map2((x, y) -> Tree.Branch(x, y), chooseTreeLazy(gen, maxNodes/2), chooseTreeLazy(gen, maxNodes/2))) :: Nil
            )
        )
    }

// branchRate is the expected number of branches for each leaf
pub def chooseTree(gen: Gen[a], maxNodes: Int32): Gen[Tree[a]] =
    if (maxNodes <= 1) {
        Functor.map(Tree.Leaf, gen)
    } else {
        frequency(
            Nel.Nel(
                (1, Functor.map(Tree.Leaf, gen)),
                (2, Applicative.map2((x, y) -> Tree.Branch(x, y), chooseTree(gen, maxNodes/2), chooseTree(gen, maxNodes/2))) :: Nil
            )
        )
    }

instance Arbitrary[Tree[a]] with Arbitrary[a] {
    pub def arbitrary(): Gen[Tree[a]] = 
        // sized(size -> chooseTree(Arbitrary.arbitrary(), size))
        sized(size -> chooseTreeLazy(Arbitrary.arbitrary(), size))
}


pub def countNodes(t: Tree[a]): Int32 = 
    match t {
        case Tree.Leaf(_) => 1
        case Tree.Branch(x, y) => countNodes(x) + countNodes(y)
    }

pub def mmm(): Unit \ IO = 
    let treeGen: Gen[Tree[Int32]] = Arbitrary.arbitrary();
    let res = applyGen(treeGen, 100, fresh());
    println(countNodes(res))
