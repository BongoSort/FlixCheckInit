

mod Arbitrary {
    pub trait Arbitrary[a] {
        pub def arbitrary(): Gen[a]
    }

    instance Arbitrary[Int32] {
        pub def arbitrary(): Gen[Int32] = Gen.chooseInterval(-20, 20)
    }

    pub enum Color {
            case Red, Green, Blue
        }
}