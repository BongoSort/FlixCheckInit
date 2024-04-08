
use Testable.{Testable, Result}
use Monadic2.{Gen}

type alias Config = {
    maxTest = Int32,
    maxFail = Int32,
    size = Int32 -> Int32,
    every = Int32 -> List[String] -> String
}

/// A default setting for the config
pub def default(): Config = { 
    maxTest = 100,
    maxFail = 1000,
    size = n -> n/2 + 3,
    every = n -> _ -> 
        "${n}" + String.repeat(String.length("${n}"), "\\b")
}

pub def verbose(): Config = 
    let func = (n -> args -> 
        "${n}" + String.intercalateChar('\n', args)
    );
    {every = func | default()}

pub def flixCheck(): a -> Unit \ IO with Testable[a] = 
    ???

pub def check(config: Config, x: a): Unit \ IO with Testable[a] = 
    println("asd");
    ???

pub def tests(config: Config, gen: Gen[Result], rng: SplittableRandom, nTest: Int32, nFail: Int32): Unit \ IO = 
    println("?x");
    ???

pub def done(msg: String, nTest: Int32, stamps: List[List[String]]): Unit \ IO = 
    // Helper functions
    def percentage(n: Int32, m: Int32): String =
        "${(100 * n) / m}" + "%";
    def entry(pair: (Int32, List[String])): String =
        percentage(fst(pair), nTest) + " " + String.intercalate(", ", snd(pair));
    def pairLength(xs: List[List[String]]): (Int32, List[String]) =
        match xs {
            case Nil => (0, Nil),
            case head :: _ => (List.length(head), head)
        };

    let table = (
        List.filter(x -> not List.isEmpty(x), stamps) |>
        List.sort |>
        List.groupBy(Eq.eq) |>
        List.map(pairLength) |>
        List.sort |>
        List.reverse |>
        List.map(entry) |>
        display
    );

    println(msg + " " + "${nTest}" + " tests" + table)

// Overvej om outputtet skal se anderledes ud
pub def display(l: List[String]): String =
    match l {
        case Nil => ".\\n",
        case x :: Nil => " " + "(${x}).\\n",
        case xs => ".\\n" + String.intercalateChar('\n', List.map(s -> s + ".", xs))
    }
