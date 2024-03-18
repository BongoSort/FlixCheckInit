pub enum GGen[a: Type] {
    case GGen(Rand -> a)
}

pub enum Bottle[a] {
    case Something(a)
}

// instance Functor[GGen] {
//     pub def map(f: a -> b \ ef, x: GGen[a]): GGen[b] \ ef =
//         GGen.GGen(rand -> 
//             let y = applyGGen(x, rand);
//             let res = f(y);
//             res
//     )
// }

pub def applyGGen(gen: GGen[a], rand: Rand): a = 
    let GGen.GGen(f) = gen;
    f(rand)



// instance Applicative[GGen] {

//     pub def pure(x: a): GGen[a] = GGen.GGen(rand -> x)

//     pub def ap(f: GGen[a -> b], x: GGen[a]): GGen[b] = 
//         GGen.GGen(rand -> 
//             let (r1, r2) = splitRand(rand);
//             let GGen.GGen(func) = f;
//             let GGen.GGen(arg) = x;
//             func(r1)(arg(r2))
//         )

// }

// instance Monad[GGen] {

//     pub def flatMap(f: a -> GGen[b] \ ef, x: GGen[a]): GGen[b] \ ef = 
//         GGen.GGen(rand -> 
//             let (r1, r2) = splitRand(rand);
//             let GGen.GGen(func) = x;
//             let newGen = f(func(r1));
//             let GGen.GGen(newFunc) = newGen;
//             newFunc(r2)
//         )

// }