## Backlog

* Gen for flere typer: (Int64, strenge, bool, List, generiske typer?? osv)
* Unit
* Bool OK
* Char
* Float32	
* Float64	
* Int8
* Int16
* Int32 OK Testet
* Int64
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



## Spørgsmål til Magnus

Hvordan renamer man en mod?
11.2 Docs siger:
A qualified use with rename: use A.B.Color => AColor
Men det virker ikke.


Hvorfor kan f.eks. Arbitrary finde Gen uden at Gen specifikt er blevet brugt (use) i Arbitrary?

### Svar
// fx Int= 42
/// f: Int -> a
// g: Int -> b
// x -> (f(x), g(x)) ->(f.eks true, true, men gerne noget andet også)
// Int ->(a,b)