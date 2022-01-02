package fun.gravax.gravnum

private trait PureNumSymStuff

/*
PureNum types are limited to subtypes of the Reals, generally expressed as algebraic numbers.
Complex numbers are not accounted for in this framework.
Hmmm.
...because our main purposes are
	1) computation-about-computation, such as structure sizes and time durations
	2) computation about knowledge
	3) limited computation about algebra, topology, geometry
	4) Push the boundary on type-refinement
and NOT:  Signal processing, fourier transforms, machine learning, quantum computing

...and because we are focused on interoperability of data, functions, types.

// These Interface traits establish interoperability for basic tests and arithmetic.
// These ops are not quite proven safe, but they are at least in a functional and somewhat inductive pattern.
// Note that all methods take 0 or 1 args, and all args+results are PN, Boolean, or Option.

// Implementation relies on platform plugin to supply a very limited number of features, primarily based
// on arithmetic over positive integers (i.e. naturals)

When all methods take zero or one args, and the type constructors are 'monadic'...
 */

trait UnaryInquiryPN {
	// These are mellow, since we don't have to supply input/output number types
	val isZeroPN : Boolean
	val isPositivePN : Boolean
	val isNegativePN : Boolean
	val isIntegerPN : Boolean
	final val isPosIntPN: Boolean = isPositivePN && isIntegerPN
	final val isNegIntPN: Boolean = isNegativePN && isIntegerPN
}
trait UnaryArithPN {
	type ReciprocalType <: PureNum 	// Type of the multiplicative inverse of this inst - seems knowable
	type ComplementType <: PureNum 	// Type of the additive inverse of this inst - seems knowable
	type ReducedFracType <: PureNum // Type of the reduced fraction of this inst - known how?
	def safeReciprocalPN : Option[ReciprocalType]
	def negatePN : ComplementType
	def reduceFractionPN : ReducedFracType
}
trait BinaryInquiryPN {
	// These comparison operators all accept the general type of PureNum

	// All three methods must be given by implementor, due to varying perspectives on how to best answer these
	def isEqPN(otherPN : PureNum) : Boolean
	def isGtPN(otherPN : PureNum) : Boolean
	def isGteqPN(otherPN : PureNum) : Boolean
	// Less-than forms defined by negation
	def isLtPN(otherPN : PureNum) : Boolean // = !isGteqPN(otherPN)
	def isLteqPN(otherPN : PureNum) : Boolean // = !isGtPN(otherPN)
}
trait CommutArithPN {
	// Here is where we get a bit stuck, because we don't know how to usefully confine the type of result based
	// on our self type+value and the type+value of input.
	// Computationally these types will work, but the output type is weak.
	// Scala 3 has a sort-of dependent function type that builds on path-dependent types
	// https://docs.scala-lang.org/scala3/reference/new-types/dependent-function-types.html
	// Some notes on refinement types,
	// https://contributors.scala-lang.org/t/poor-or-rich-mans-refinement-types-in-scala-3-x/4647
	// based on Scala 3 "opaque" types
	// https://docs.scala-lang.org/scala3/book/types-opaque-types.html

	def plusPN(otherPN : PureNum) : PureNum
	def timesPN(otherPN : PureNum) : PureNum
}
trait NoncommutArithPN  {
	def minusPN(otherPN : PureNum) : PureNum
	def safelyDivideByPN(otherPN : PureNum) : Option[PureNum]
	def divideByNonzeroPN(otherPN : NonzeroPN) : PureNum
}
// For users, PureNums are not guaranteed unique in any way, but all inquiry and arith should always work correctly,
// and execute eagerly (to produce strong-typed result as data) to ensure fail-fast.  However fractions may be left
// unreduced in the results (use .reduceFractionPN to ensure reduced).
// Note that the result type is the broad computational type PureNum, rather than a more restrictive
// (hence more powerful) subtype, which might be discovered after execution (e.g. by case-switch,
// and/or checking "isX" flags -- all the conventional tricks).
// More restrictive (hence powerful) API may then be offered by specific subtypes.
// Nothing original in the above, just ordinary scala subtyping.
// All of this is protective, fallback typing that guards our "compute an explicit result" world.
// Our intensional typing happens in the author's axLam world of algebraic types and functions.
// These two worlds are intended to be compatible, just as black and white stones are compatible in Go.

sealed trait PureNum extends UnaryInquiryPN
		with BinaryInquiryPN with UnaryArithPN with CommutArithPN with NoncommutArithPN

trait NonzeroPN extends PureNum {
	override val isZeroPN: Boolean = false
}
trait PositivePN extends NonzeroPN {
	type ComplementType <: NegativePN
	override val isPositivePN: Boolean = true
	override val isNegativePN: Boolean = false
}
trait NegativePN extends NonzeroPN {
	type ComplementType <: PositivePN
	override val isPositivePN: Boolean = false
	override val isNegativePN: Boolean = true
}


// Elements form an abelian additive group (with inverse), also closed under times,
// thus yielding algebraic ring of IntegerPNs, which are embedded in the algebraic field of RationalPNs.
trait IntegerPN extends PureNum with YaflIntNum {
	type ComplementType <: IntegerPN
	override val isIntegerPN: Boolean = true
	// Integers are a commutative ring.
	// When we know we are adding integers to integers, then we know we are within this abelian group.
	def plusIPN(otherIPN : IntegerPN) : IntegerPN
	// When we know we are multiplying integers times integers, then we know we are within this commutative ring.
	def timesIPN(otherIPN : IntegerPN) : IntegerPN

}
trait ZeroPN extends PureNum with IntegerPN {
	override type ComplementType = ZeroPN
	override type ReciprocalType = Nothing
	override type ReducedFracType = ZeroPN

	override val isZeroPN: Boolean = true
	override val isPositivePN: Boolean = false
	override val isNegativePN: Boolean = false
}

trait WholeIntPN extends IntegerPN {
	// WholeInts ( >= 0) are closed under plus and times.
	// WholeInts are a semigroup under each operation,
	//  a commutative monoid under plus (with additive identity 0)
	//  a commutative monoid under times (with mult identity 1)
	// https://en.wikipedia.org/wiki/Semigroup has a good chart of
	// "A monoid is an algebraic structure intermediate between groups and semigroups, and is a semigroup having
	// an identity element, thus obeying all but one of the axioms of a group: existence of inverses is not required
	// of a monoid."
	// Whole plus Whole is Whole


	def plusWIPN(otherWIPN: WholeIntPN): WholeIntPN = {
		if (otherWIPN.isZeroPN) this
		else {
			// Hmm this cast must work, given certain assumptions about how numbers are constructed
			val posOther = otherWIPN.asInstanceOf[PosIntPN]
			val posResult: PosIntPN = plusPIPN(posOther)
			// We know now the result is PosIntPN, but our signature type is just Whole
			posResult
		}
	}

	// Whole times Whole is Whole
	def timesWIPN (otherWIPN : WholeIntPN) : WholeIntPN

	// Whole plus Pos is Pos
	def plusPIPN (otherPIPN : PosIntPN) : PosIntPN
}
trait PosIntPN extends WholeIntPN with PositivePN {

	override type ComplementType <: NegIntPN

	// PosInts ( >= 1) are closed under plus and times.
	// WholeInts are a semigroup under each operation,
	//	 a commutative monoid under times (with mult identity 1)
	// Positive integer corresponds to a constructible natural number

	def timesPIPN (otherPIPN : PosIntPN) : PosIntPN

	override def timesWIPN (otherWIPN : WholeIntPN) : WholeIntPN = {
		if (otherWIPN.isZeroPN) {
			// We don't have to construct a different zero, or explicitly downcast, unless we are expecting
			// more type magic
			otherWIPN
		}
		else {
			// Crazy downcast as in plusWIPN above
			val posOther = otherWIPN.asInstanceOf[PosIntPN]
			timesPIPN(posOther)
		}
	}
}
trait NegIntPN extends IntegerPN with NegativePN {
	override type ComplementType <: PosIntPN
}
trait PureNumBaseImpl extends PureNum {
	override def isLtPN(otherPN: PureNum): Boolean = !isGteqPN(otherPN)

	override def isLteqPN(otherPN: PureNum): Boolean = !isGtPN(otherPN)

	override def minusPN(otherPN: PureNum): PureNum = {
		val negOth = otherPN.negatePN
		plusPN(negOth)
	}
	override def safelyDivideByPN(otherPN: PureNum): Option[PureNum] = {
		val recipOth = otherPN.safeReciprocalPN
		// Fraction gets reduced by default, but this is not specified by API, yet.
		recipOth.map(ro => this.timesPN(ro).reduceFractionPN)
	}
}
/*
trait NegPureNumImplByCompl extends NegativePN {
	val myComplementaryPosPN : PositivePN
	def negateToPos : PositivePN = myComplementaryPosPN
	override def negatePN: PureNum = negateToPos
}
 */
trait NegNumBaseImpl extends PureNumBaseImpl with NegativePN {
	// Could use the ComplementType here
	val myComplement : ComplementType

	override def negatePN : ComplementType = myComplement
}


/*

private case class OnePN() extends PosIntPN {

}
// A natural number two or greater
private case class BigPosNat() extends PosIntPN {
}
trait PNUtils {
}
 */