
/// Represents a splittable pseudorandom number.
pub enum SplittableRandom[_r: Region](##java.util.SplittableRandom)


mod SplittableRandom {

    /// Returns a new SplittableRandom instance that is initialized with a random seed.
    pub def fresh(_rc: Region[r], s: Int64): SplittableRandom[r] \ r + IO =
        import java_new java.util.SplittableRandom(): ##java.util.SplittableRandom \ r + IO as newSplittableRandom;
        SplittableRandom(newSplittableRandom())

    /// Returns a new SplittableRandom instance that is initialized with the specified seed.
    pub def freshWithSeed(_rc: Region[r], seed: Int64): SplittableRandom[r] \ r =
        import java_new java.util.SplittableRandom(Int64): ##java.util.SplittableRandom \ r as newSplittableRandom;
        SplittableRandom(newSplittableRandom(seed))

    /// Returns a new SplittableRandom instance that is independent of the current instance.
    pub def split(sr: SplittableRandom[r]): (SplittableRandom[r], SplittableRandom[r]) \ r =
        import java.util.SplittableRandom.split(): ##java.util.SplittableRandom \ r;
        let SplittableRandom(object) = sr;
        (sr, SplittableRandom(split(object)))

    /// Returns a pseudorandom boolean value.
    pub def nextBool(sr: SplittableRandom[r]): Bool \ r =
        import java.util.SplittableRandom.nextBoolean(): Bool \ r;
        let SplittableRandom(object) = sr;
        nextBoolean(object)

    /// Returns a pseudorandom int32 value between the specified origin (inclusive) and the specified bound (exclusive).
    pub def nextInt32(sr: SplittableRandom[r], origin: Int32, bound: Int32): Int32 \ r =
        import java.util.SplittableRandom.nextInt(Int32, Int32): Int32 \ r;
        let SplittableRandom(object) = sr;
        nextInt(object, origin, bound)

    /// Returns a pseudorandom Int64 value between the specified origin (inclusive) and the specified bound (exclusive).
    pub def nextInt64(sr: SplittableRandom[r], origin: Int64, bound: Int64): Int64 \ r =
        import java.util.SplittableRandom.nextLong(Int64, Int64): Int64 \ r;
        let SplittableRandom(object) = sr;
        nextLong(object, origin, bound)

    /// Returns a pseudorandom Float64 value between the specified origin (inclusive) and the specified bound (exclusive).
    pub def nextFloat64(sr: SplittableRandom[r], origin: Float64, bound: Float64): Float64 \ r =
        import java.util.SplittableRandom.nextDouble(Float64, Float64): Float64 \ r;
        let SplittableRandom(object) = sr;
        nextDouble(object, origin, bound)

}