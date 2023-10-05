package fun.gravax.fbase.fstore

object RunFactPacker {
	def main(args: Array[String]): Unit = {
		println("RunFactPacker says HI")
		val fis = new FactItemStuff{}
		fis.okWow()
		println("RunFactPacker says BYE")
	}
}
trait FactItemStuff {

	trait Thingo
	case class Woodgle(name : String) extends Thingo
	/*
	implicit conversions have some limitations:

	They cannot take multiple non-implicit arguments
	They cannot chain multiple implicit conversions
	 */

	implicit def toWoo(nm : String) : Woodgle = Woodgle(nm)

	/*
	https://www.baeldung.com/scala/implicit-classes
	Unfortunately, not all classes can be implicit classes because

	They cannot be defined as top-level objects
	They cannot take multiple non-implicit arguments in their constructor
	We cannot use implicit classes with case classes
	There cannot be any member or object in scope with the same name as the implicit class
	 */
	// "implicit class must have a primary constructor with exactly one argument in first parameter list"
	implicit class Numbery(num: Number) extends  Thingo {
		override def toString: String = s"Numbery(num=${num})"
	}

	def okWow() : Unit = {
		val goodNm = "namely"
		def t : Thingo = goodNm
		println(s"t=${t}")
		val nn = 8
		val nnn = Integer.valueOf(nn)
		val nt : Thingo = nnn
		println(s"nt=${nt}")
	}
}

/*

Implicits



DSL with combinators
interpreter

pattern match / unapply

Using case without match creates a ... partial function, which may be used as argument to map/flatMap etc
...because it is implicitly converted to a function that will throw MatchError if the partialFunction is not definedAt

Using partialFunction with .collect

lazy collection, view / stream

by name args

Rules about constructors, memoization, self type

.curried, .tupled, .uncurried, .untupled

List operators, seq operators

"Any Scala method that ends with a : character is evaluated from right to left. "
Therefore, in these examples, the methods actually come from the variable a on the far right side of the expression:

Note that :-ending operators are right associative (see example).
A mnemonic for +: vs. :+ is: the COLon goes on the COLlection side.

To append or prepend one or more elements to a Vector or Seq, use these methods:
to append one item, use :+
to append multiple items, use ++
to prepend one item, use +:
to prepend multiple items, use ++:

Chaining with compose/andThen

==  .equals   eq/ne

boxing/unboxing

sbt

unit tests

variance


You normally use ==, it routes to equals, except that it treats nulls properly. Reference equality (rarely used) is eq.

Use eq method to check if both arguments are EXACTLY the same reference.
Recommended not to use unless you understand how this works and often equals will work for what you need instead. And make sure to only use this with AnyRef arguments, not just Any

 */