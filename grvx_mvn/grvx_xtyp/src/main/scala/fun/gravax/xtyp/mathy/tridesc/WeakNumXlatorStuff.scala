package fun.gravax.xtyp.mathy.tridesc

import spire.math.{Algebraic => SpAlgebraic, Rational => SpRat, Real => SpReal}

private trait WeakNumXlatorStuff

/*
Hmm so we want to take advantage of the spire types, but keep control over our API exposure.

Also want to explore the boundary between our unfashionable trait-as-semantic-type and the fashionable
functors-first typing style
 */

trait SpireNumsToWeakNums {
	def convertToArithNum[SAN](spireArithNum: SAN) : ArithmeticNum
	def convertToCountNum[SCN](spireCntNum: SCN) : WeakCountNum
}

trait WeakNumsToSpireNums {
	def toSpireReal(weakRealNum : WeakRealNum) : SpReal
	def convertToSpireReal[WRlN](weakRealNum : WRlN) : SpReal
}

trait DoSomeMath {

}