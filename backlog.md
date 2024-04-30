# Backlog

* Gen for flere typer: (Int64, strenge, bool, List, generiske typer?? osv)
* Unit OK uden monad
* Bool OK uden monad
* Char OK uden monad
* Float32	OK uden monad
* Float64	
* Int8
* Int16
* Int32 OK Testet uden monad
* Int64 - Samme som 32?
* String 
* BigInt
* BigDecimal
* Tuples OK uden monad
* Triples OK uden monad
* Quadruples OK uden monad
* Lists OK uden monad
* Nel OK uden monad
* Vector OK uden monad
* Option OK uden monad
* Result OK uden monad
* Chain OK uden monad
* Nec OK uden monad
* Set?
* Map?
* MultiMap?

* Gen skal være smart! (for Int32 tjek gerne 0,1,-1, min-tal, max-tal)

* Type class Testable som benytter typen Property
* Properties
* Properties med flere regler 

* Shrink - reducér problemet til det mindste

* Arbitrary for flere typer

* QuickCheck for Flix...

## TODO

Lav bachelor paper
Introduktion f.eks.

Læs om Shrink / Reduce

Skriv hvorfor vi ikke tester mange properties på een gang; Smart at specifikt få at vide at én property fejler. Mindre smart at få at vide at en eller anden property i en liste af properties har fejlet. prop1 $\cap$ prop2 $\cap$ ... $\cap$ propn = allprops, faktisk mindre smart end hver enkelt. 

Evt Implementer en smartere måde at lave properties på. Er der en smartere måde? Eller er det OK

Overvej om vi skal lave lidt om på Collect og Classify i Testable - Vi vil måske gerne have at de ikke returnerer en funktion.

Lav README.md med grundlæggende forklaring til hvordan man bruger flixCheck.

Sørg for at public og ikke public metoder er lavet korrekt. Vi vil gerne styre hvilke metoder der er tilgængelige for brugeren.

## Designovervejelser

Imports - hent kun ting hvis de bliver brugt flere gange, ellers kald f.eks. Monadic2.generate

Øh - Har vi brug for point(x: a) i Gen?

Lav generatoren således at den spytter en liste ud med ting der kan afprøves, i stedet for specifikke tal, strenge mm. (?)
gem resultatet / print resultatet så vi kan reducere/shrink det vi allerede har testet

Er chooseInt et godt valg til at håndtere forskellige opgaver? F.eks som valg af specifik Int interval, men også som definition af vektorstørrelse, men så med parametrene (0, size)

chooseInt bruger modulo, hvilket ikke rigtigt kan håndtere meget store tal. Skal vi i stedet importere noget smart mht Randomm eller evt bruge remainder i stedet?

Overvej om måden at lave properties på kan være smartere (for brugeren).

VerboseCheck printer argumenterne til en test, så man kan se hvad der blev brugt til de fejlede/succefulde cases. Spørgsmålet er om vi også vil printe resultatet, for bedre gennemsigtighed? Tester man modulo(24654,25226) vil man måske gerne have det beregnede resultat med, så man kan se hvorfor den property ikke er overholdt.

## Spørgsmål til Magnus

`pub enum TFunc[a, b](a -> b)` Er dette en god løsning, ift associated types / complex instance type

### Svar

Matthew says that if you want to define trait instances for functions you must make them effect polymorphic
So instance[a -> b \ ef]
and not just instance [a -> b].