trait Dividable[t] {
    type Aef: Eff
    pub def div(x: t, y: t): t \ Dividable.Aef[t]
}

pub eff DivByZero {
    pub def throw(): Void
}

pub enum Unit1 {
    case Unit1
}

pub enum Void1 {
}

instance Dividable[Float32] {
    type Aef = {Pure}
    pub def div(x: Float32, y: Float32): Float32 = x / y
}

instance Dividable[Int32] {
    type Aef = {DivByZero}
    pub def div(x: Int32, y: Int32): Int32 \ DivByZero = 
        if (y == 0) {
            do DivByZero.throw()
        } else {
            x / y
        }
}

// pub def testDiv(x: Int32, y: Int32): Unit \ {IO} =
//     def prop(x: Int32, y: Int32): Property \ DivByZero = 
//         (y == 0) ==> do DivByZero.throw();
//     flixCheck(TFunc2.TFunc2(prop))


pub def testMain(): Unit \ IO =
    let l = 1 :: 2 :: 3 :: Nil;
    region rc {
        let a = List.toArray(rc, l);
        println(Array.toString(a))
    }
