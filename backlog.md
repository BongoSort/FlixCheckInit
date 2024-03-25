# Backlog

* Gen for flere typer: (Int64, strenge, bool, List, generiske typer?? osv)
* Unit
* Bool OK
* Char OK
* Float32	
* Float64	
* Int8
* Int16
* Int32 OK Testet
* Int64 - Samme som 32?
* String 
* BigInt
* BigDecimal
* Tuples
* Lists
* Vector

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




## Designovervejelser

Lav generatoren således at den spytter en liste ud med ting der kan afprøves, i stedet for specifikke tal, strenge mm. (?)
"noget.noget.noget" er træls
gem resultatet / print resultatet så vi kan reducere/shrink det vi allerede har testet

Er chooseInt et godt valg til at håndtere forskellige opgaver? F.eks som valg af specifik Int interval, men også som definition af vektorstørrelse, men så med parametrene (0, size)

chooseInt bruger modulo, hvilket ikke rigtigt kan håndtere meget store tal. Skal vi i stedet importere noget smart mht Randomm eller evt bruge remainder i stedet?

## Spørgsmål til Magnus
lazy bind
monadic effectful

overordnet plan

Split random generator (pure)
modulo
næste uge

(fraction implementer korrekt)

### Svar

// fx Int= 42
/// f: Int -> a
// g: Int -> b
// x -> (f(x), g(x)) ->(f.eks true, true, men gerne noget andet også)
// Int ->(a,b)