mod OE {    
    pub def caller(): Unit \ IO =
        oddOrEven(12345) |> println;
        oddOrEven(1234) |> println;
        oddOrEven(0) |> println;
        oddOrEven(1) |> println;
        oddOrEven(42) |> println;
        oddOrEven(-1) |> println


    pub def isOdd(n: Int32): Bool =
        if (n == 0) false else if (n < 0) isEvn(n + 1) else isEvn(n - 1)

    pub def isEvn(n: Int32): Bool =
        if (n == 0) true else if (n < 0) isOdd(n + 1) else isOdd(n - 1)

    pub def oddOrEven(n: Int32): Unit \ IO = 
        let res = isOdd(n);
        if (res) println("The number ${n} is Odd") 
            else println("The number ${n} is Even") 
}