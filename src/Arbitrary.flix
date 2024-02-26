mod Arbitrary {
    pub trait Arbitrary[a] {
        pub def arbitrary(): Gen[a]
    }

    instance Arbitrary[Int32] {
        pub def arbitrary(): Gen[Int32] = Gen.chooseInterval(-20, 20) //Hardcoded interval, fix later?
    }

    instance Arbitrary[Bool] {
        pub def arbitrary(): Gen[Bool] = Gen.chooseBool()
    }

    // instance Arbitrary[(Arbitrary[Int32], Arbitrary[Bool])] {
    //     pub def arbitrary(): Gen[(a, b)] = (Arbitrary[a], Arbitrary[b])
    // }
}