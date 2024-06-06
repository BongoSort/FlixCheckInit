# FlixCheckInit

This is the initial entrypoint for implementing Haskells QuickCheck for the Flix programming language

## What is FlixCheck

The purpose of FlixCheck is to be a tool for which a Flix programmer can automatically test some user-defined properties of programs.

FlixCheck was developed based on the QuickCheck implementation in Haskell, and thus follows many of the same principles.

## How to use FlixCheck

To test a program, you first have to formulate a property that must hold. As an example, a double reversed list should be the same as the original list:

```java
def prop_revRev(xs: List[Char]) = List.reverse(List.reverse(xs)) == xs;
```

Here we have defined, that the List of Chars given, should be the same as the double reversed list.

Once the property has been defined, the desired function can be tested. For this, we need a generator for the random input. In this case, the generator generates random lists containing random (uppercase) chars.

```java
Arbitrary.arbitrary()
```

A generator "Arbitrary.arbitrary()" and the property "prop_revRev" are combined with the forAll function, and run through FlixCheck.

```java
flixCheck(forAll(Arbitrary.arbitrary(), prop_revRev))
```

Another way we can do the same thing, without specifying the generator, is to wrap the property in a testable function, a TFunc:

```java
flixCheck(TFunc.TFunc(prop_revRev))
```

The output we get is:

```cmd
OK, passed 100 tests.
```

If we want more information about the tests, we can run verboseCheck:

```java
verboseCheck(TFunc.TFunc(prop_revRev))
```

Which returns the arguments, for which the program is run

```cmd
0:  Nil
1:  F :: N :: Nil
.
.
.
97:  N :: U :: N :: W :: O :: E :: K :: D :: U :: Nil
98:  K :: B :: R :: X :: D :: Nil
99:  Q :: V :: W :: K :: H :: K :: Y :: B :: D :: O :: K :: H :: H :: J :: Q :: R :: I :: O :: C :: Z :: G :: V :: Y :: N :: E :: H :: M :: C :: Nil
OK, passed 100 tests.
```

A third option, is where we define the configuration of the property check ourselves, instead of using the predefined flixCheck and verboseCheck.
A default config looks like this:

```java
check({maxTest = 100,maxFail = 1000,size = n -> n/2 + 3,every = _ -> _ -> ""}, prop_revRev)
```

But if we only want to do 42 test, and keep the rest of the default settings, we can modify it like this:

```java
check({maxTest = 42 | default(), TFunc.TFunc(prop_revRev))
```

Maybe we want it to be verbose, and use 42 tests:

```java
check({maxTest = 42| verbose()}, TFunc.TFunc(prop_revRev))
```

Contributors are Svend, Kasper and Rune
