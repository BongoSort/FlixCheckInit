// String Splitting function
// EXAMPLE USAGE:
// split ' ', "Hello, my name is Peter!" = Hello, :: my :: name :: is :: Peter! :: Nil
// split '@', "name@address.dk" = name :: address.dk :: Nil
def split(c: Char, str: String): List[String] =
    match str {
        case "" => Nil,
        case _ => 
            let prefix = String.takeWhile((x -> x != c), str);
            let rest = String.drop(1, String.dropWhile((x -> x != c), str));
            prefix :: split(c, rest)
    }

def split2(c: Char, str: String): List[String] =
    match str {
        case "" => "" :: Nil
        case _ => 
            let prefix = String.takeWhile((x -> x != c), str);
            let rest = String.drop(1, String.dropWhile((x -> x != c), str));
            if (prefix == str) {
                prefix :: Nil
            } else {
                prefix :: split2(c, rest)
            }
    }

def merge(c: Char, l: List[String]): String =
    String.intercalateChar(c, l)

@Test
def testSplit1(): Bool =
    Assert.eq(split('@', "name@address.dk"), "name" :: "address.dk" :: Nil)

@Test
def testSplit2(): Bool =
    Assert.eq(split('/',"C:/User/Username/Documents"), "C:" :: "User" :: "Username" :: "Documents" :: Nil)

@Test
def testSplit3(): Bool =
    Assert.eq(split('%', "#coding#haskell#flix"), "#coding#haskell#flix" :: Nil)

def examples(): List[(Char, String)] =
   ('#', "#coding#haskell#flix") :: 
   ('@', "name@address.dk") :: 
   ('/',"C:/User/Username/Documents") :: 
   ('@', "asd@@bbe") ::
   ('a', "hla") ::
   Nil

def prop_splitInverse(c: Char, s: String): Bool = 
    merge(c, split(c, s)) == s

def prop_splitInverse2(s: String): Property = 
    let charGen = 
        match List.toNel(String.toList(s)) {
            case None => Arbitrary.arbitrary(),
            case Some(nel) => elements(nel)
        };
    forAll(charGen, c -> merge(c, split(c, s)) == s)

// Pretty print a list of strings
def prettyList(l: List[String]): String = 
    "[\"" + String.intercalate("\", \"", l) + "\"]"


def printExamples(): Unit \ IO = 
    foreach(e <- examples()) {
        let (c, s) = e;
        println(prettyList(split2(c, s)))
    }

def main(): Unit \ IO =
    flixCheck(TFunc2.TFunc2(prop_splitInverse))
    // flixCheck(forAll(Arbitrary.arbitrary(), prop_splitInverse2))
