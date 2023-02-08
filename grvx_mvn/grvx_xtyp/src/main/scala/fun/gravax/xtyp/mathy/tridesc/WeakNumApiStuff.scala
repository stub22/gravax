package fun.gravax.xtyp.mathy.tridesc

private trait WeakNumApiStuff

/*
Here we try defining a taxonomy of numeric types using traits, type members, typer parameters, and inheritance.

Here we define a narrow set of core numeric operations for real number types
(and so far just a skeleton  for an integer/natural CountNum type).
Composite records like TriShapes can use this API to delay binding to specific numeric constructs.

This form keeps a lot of flexibility for implementors, but makes things tricky for the user-client code.
How does client know our MyArgTy and MyRsltTy? ?
 */
trait ArithmeticNum {
	// Logically, our general idea is to use (almost) algebraically closed (almost) rings (rigs?) as subtypes.
	// This separation of Arg+Rslt numeric bound-types is for technical software factors e.g. memoization.
	type MyArgTy <: ArithmeticNum
	type MyRsltTy <: ArithmeticNum
	def plus(otherRN : MyArgTy) : MyRsltTy
	// def otherPlus (otherN : this.type) : this.type
	def negate : MyRsltTy
	def times(otherRN : MyArgTy) : MyRsltTy

	// We can ask if another real number is equal, but we may not always get an answer.
	// https://math.stackexchange.com/questions/143727/determining-the-equality-of-two-real-numbers
	// We expect our software to quickly know whether it can quickly determine the answer.
	// When it cannot, it should return None.
	def fastWeakEqualsCheck (otherRN : MyArgTy) : Option[Boolean]
	// Similarly we expect to quickly know whether or not a zero-check is feasible.
	// If not, then this number cannot be used as a divisor.
	def fastWeakZeroCheck : Option[Boolean]
}
// A numeric type with a known(+exposed) Arg/Rslt type, over which we expect all operations to be closed.
trait ClosedWRN[CWRNT <: ArithmeticNum] extends ArithmeticNum {
	override type MyArgTy = CWRNT
	override type MyRsltTy = CWRNT
}

trait DividableNum[DNT <: ArithmeticNum] extends ArithmeticNum {
	def divide(otherRN : MyArgTy) : Option[DNT]
}

// This type does not take a param, because it does not support logical constraints.
trait WeakRealNum extends ArithmeticNum with DividableNum[WeakRealNum] with ClosedWRN[WeakRealNum] {
}

trait ExactNum extends ArithmeticNum {
	// Exact numbers can reliably check for equality
	def reliableZeroCheck : Boolean
	def reliableEqualsCheck(otherEN : MyArgTy) : Boolean

	override def fastWeakZeroCheck: Option[Boolean] = Some(reliableZeroCheck)
	override def fastWeakEqualsCheck(otherEN: MyArgTy): Option[Boolean] = Some(reliableEqualsCheck(otherEN))

	def asRealNum : WeakRealNum
}

// Logical constraints may be enforced through WANT, which should actually be an extension of WeakAlgebraicNum.
trait WeakAlgebraicNum[WANT <: ExactNum] extends ExactNum with DividableNum[WANT] with ClosedWRN[WANT] {
	def squareRootAbs : MyRsltTy
}

trait WeakRationalNum[WRNT <: ExactNum] extends ExactNum with DividableNum[WRNT] with ClosedWRN[WRNT] {
	def asAlgebraicNum : WeakAlgebraicNum[WRNT]
}

trait WeakIntegerNum[WINT <: ExactNum] extends ExactNum with ClosedWRN[WINT] {
	// Signed int
	def asRationalNum : WeakRationalNum[WINT]
}

trait WeakCountNum extends ExactNum {
	// Int >= 0
}
trait SignedCountNum extends WeakCountNum {
	// So far used only for winding number of an unbounded angle
}