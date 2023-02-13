package fun.gravax.xtyp.mathy.tridesc

import spire.algebra.{AdditiveGroup, Field, MultiplicativeGroup, MultiplicativeSemigroup, NRoot}
import spire.math.{Algebraic => SpAlg, Number => SpNum}

private trait TriShapeBetterStuff

// Xactish == All properties are exact except the Angles, which may be exact under special conditions.
trait TriShapeXactish extends TriShapeP[SpAlg, SpNum, SpAlg, SpAlg]

trait OurMathOps[Num] {
	def getAddGroup : AdditiveGroup[Num] = getField
	// def getMultGroup : MultiplicativeSemigroup = getField.
	def getField : Field[Num]
	def getNRoot : NRoot[Num]
	def plus(n1 : Num, n2 : Num) = getAddGroup.plus(n1, n2)


	def heronsAreaEasy(semiPerim: Num, sideA : Num, sideB : Num, sideC : Num) = {
		// This "Easy" version is numerically unstable for triangles that are close-to-degenerate.
		// TODO: Add the stable version
		// https://en.wikipedia.org/wiki/Heron%27s_formula
		val addGrp = getAddGroup
		val aDiff = addGrp.minus(semiPerim, sideA)
		val bDiff = addGrp.minus(semiPerim, sideB)
		val cDiff = addGrp.minus(semiPerim, sideC)
		val fld = getField
		// TODO:  Find cheaper way to construct traversable, or do the product another way.
		val prod = fld.product(List(semiPerim, aDiff, bDiff, cDiff))
		val nroot = getNRoot
		val area = nroot.sqrt(prod)
		area
	}
}

trait MakesTSX {
	private val myAlgMathOps = new OurMathOps[SpAlg] {
		override def getField: Field[SpAlg] = SpAlg.AlgebraicAlgebra
		override def getNRoot: NRoot[SpAlg] = SpAlg.AlgebraicAlgebra
	}
	private val flg_doUnsafeDebug = false
	def mkFromSidesIncreasing(shortSide : SpAlg, medSide : SpAlg, lngSide : SpAlg) : TriShapeXactish = {
		val debugBase : String = s"Short[${shortSide}], Med[${medSide}], Long[${lngSide}]"
		// The ordering constraints on short, med, lng are enforced here by Assert.
		// Ideally, we would make them compile time constraints.
		// assert will throw java.lang.AssertionError.  assert checks may be disabled with -Xelide-below ASSERTION
		// https://www.baeldung.com/scala/assert-vs-require
		assert(shortSide.<=(medSide), s"Failed for ${debugBase} because short must be less-or-eq than med")
		assert(medSide.<=(lngSide), s"Failed for ${debugBase} because med must be less-or-eq than lng")
		val shortPlusMed = shortSide.+(medSide)
		// Treat triangle inequality as a REQUIRE condition, which throws IllegalArgumentException.
		// These cannot be disabled by compile flags.  Client might choose to handle this exception.
		require(lngSide.<=(shortPlusMed), s"Failed for ${debugBase} because lng must be less-or-eq than shrt+med")
		val tsx = new TriShapeXactish {
			override def sidesIncreasing: (SpAlg, SpAlg, SpAlg) = (shortSide, medSide, lngSide)

			override def anglesOpposedIncr: (SpNum, SpNum, SpNum) = ???

			override def perimeter = shortSide + medSide + lngSide

			// TODO:  Make result memoizable using Eval
			private val flg_debugArea = flg_doUnsafeDebug
			override def area: SpAlg = {
				val perim: SpAlg = perimeter
				val semiPerim: SpAlg = perim./(2)
				val sides: (SpAlg, SpAlg, SpAlg) = sidesIncreasing

				val arOut = myAlgMathOps.heronsAreaEasy(semiPerim, sides._1, sides._2, sides._3)
				if (flg_debugArea)
					println(s"UNSAFE DEBUG: area=${arOut} based on perim=${perim}, semiP=${semiPerim}, sides=${sides}")
				arOut
			}

		}
		if (flg_doUnsafeDebug)
			println(s"UNSAFE DEBUG: mkFromSidesIncreasing made tsx=${tsx}")
		tsx
	}
}

