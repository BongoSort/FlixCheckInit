// The main entry point.
def main(): Unit \ IO =
    let x = (1 :: 2 :: 3:: Nil);
    println(x);
    println(List.reverse(x));
    println("Hello World!");
    println("1 plus 2 gives ${add(1,2)}")

def add(a: Int32, b: Int32): Int32 =
    a + b

def isSingleton(l: List[a]): Bool =
    List.length(l) == 1