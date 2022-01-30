package fun.gravax.xtyp.mathy.tridesc

private trait WeakNumApiStuff

/* Here we define a narrow set of core numeric operations for real number types
(and so far just a skeleton  for an integer/natural CountNum type).
Composite records like TriShapes can use this API to delay binding to specific numeric constructs.
 */

trait CountNum {
}
trait SignedCountNum extends CountNum {
	// So far used only for winding number of an unbounded angle
}
trait WeakRealNum {
	// This separation of Arg+Rslt numeric bound-types is for technical software factors e.g. memoization
	type MyArgTy <: WeakRealNum
	type MyRsltTy <: WeakRealNum
	def plus(otherRN : MyArgTy) : MyRsltTy
	def negate : MyRsltTy
	def times(otherRN : MyArgTy) : MyRsltTy
	def divide(otherRN : MyArgTy) : Option[MyRsltTy]

	// We can ask if another real number is equal, but we may not always get an answer.
	// https://math.stackexchange.com/questions/143727/determining-the-equality-of-two-real-numbers
	// We expect our software to quickly know whether it can quickly determine the answer.
	// When it cannot, it should return None.
	def fastWeakEqualsCheck (otherRN : MyArgTy) : Option[Boolean]
	// Similarly we expect to quickly know whether or not a zero-check is feasible.
	// If not, then this number cannot be used as a divisor.
	def fastWeakZeroCheck : Option[Boolean]

}

