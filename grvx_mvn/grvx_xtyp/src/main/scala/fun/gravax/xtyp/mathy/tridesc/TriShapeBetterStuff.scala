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
	def mkFromSidesIncreasing(shortSide : SpAlg, medSide : SpAlg, lngSide : SpAlg) : TriShapeXactish = {
		assert(shortSide.<=(medSide), s"ShortSide ${shortSide} must be less than MedSide ${medSide}")
		assert(medSide.<=(lngSide))
		assert(lngSide.<=(shortSide.+(medSide)))
		val tsx = new TriShapeXactish {
			override def sidesIncreasing: (SpAlg, SpAlg, SpAlg) = (shortSide, medSide, lngSide)

			override def anglesIncreasng: (SpNum, SpNum, SpNum) = ???

			override def perimeter = shortSide + medSide + lngSide

			// TODO:  Make result optionally moemoizable using Eval
			override def area: SpAlg = {
				val perim = perimeter
				val semiPerim = perim./(2)
				val sides = sidesIncreasing
				myAlgMathOps.heronsAreaEasy(semiPerim, sides._1, sides._2, sides._3)
			}

		}
		tsx
	}
}

