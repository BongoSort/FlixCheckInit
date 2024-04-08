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




## Designovervejelser

Lav generatoren således at den spytter en liste ud med ting der kan afprøves, i stedet for specifikke tal, strenge mm. (?)
gem resultatet / print resultatet så vi kan reducere/shrink det vi allerede har testet

Er chooseInt et godt valg til at håndtere forskellige opgaver? F.eks som valg af specifik Int interval, men også som definition af vektorstørrelse, men så med parametrene (0, size)

chooseInt bruger modulo, hvilket ikke rigtigt kan håndtere meget store tal. Skal vi i stedet importere noget smart mht Randomm eller evt bruge remainder i stedet?

## Spørgsmål til Magnus

`pub enum TestableFunc[a, b](a -> b)` Er dette en god løsning, ift associated types / complex instance type

### Svar
