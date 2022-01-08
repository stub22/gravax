package test.gravax

import fun.gravax.axlp.core.num.{GenIntFactory, NegativePN, GeneralPureNumberFactory, PosIntPN, PositivePN, ProofPositive, PureNum, PureNumBaseImpl, PurePosIntFactory, WholeIntPN}
import fun.gravax.axlp.core.struct.{FinListN, ListOfBoundedLen}

private trait TriangleStuff

/*
Study Example:  Triangles
// Triangle has 3 side-lengths in some order
// Here "index" is 1-based
//   	sideA at index==1
//  	sideB at index==2
//		sideC at index==3
 */

// Anyref which knows it is a NatNum (Positive Int Num) between one and three, satisfying PosIntPN.
// This trait is still a marker/placeholder as of y22-m01-d08
trait NatNumBetweenOneAndThree extends PosIntPN
trait WholeNumBetweenZeroAndTwo extends WholeIntPN

trait KnowsTriSideNums {
	type SideNum <: NatNumBetweenOneAndThree
	def otherSidesInOrder(someSide : SideNum) : FinListN[SideNum] = ??? // List of length 2
	def chooseRandomSide : SideNum = ???
	val mySide01 : SideNum
	val mySide02 : SideNum
	val mySide03 : SideNum
}
// Our version of an Enum is based on "Flavor" Tags.  (Not "Kind", not "Type", not "Category", not "Class")
case class TriLegitimacyFlavor(flavorName : String)
object TriLegitFlavorTags {
	val TLFN_ORDINARY 	= TriLegitimacyFlavor("ORDINARY")	// Interior point of legitimacy, has positive area
	val TLFN_DEGEN 		= TriLegitimacyFlavor("DEGENERATE") 	// On boundary of legitimacy, has zero area
	val TLFN_ILLEGAL	= TriLegitimacyFlavor("ILLEGAL")		// Outside of legitimacy boundary, not a planar triangle
}
trait HasTriSideLengths extends KnowsTriSideNums {

	type SideLen <: PositivePN
	// type NegSideLen <: NegativePN
	// If we allow for degen-tri then Area could be Zero, so we actually want NonnegPN
	type PlanarArea <: PositivePN

	def getSideLengthsTuple: (SideLen, SideLen, SideLen)

	// "List" indexes are 0-based
	def getSideLengthsList: FinListN[SideLen] = ???

	def getSideLength(snum: SideNum): SideLen = {

		val idxInList = snum.minusPN(mySide01).asInstanceOf[WholeNumBetweenZeroAndTwo]
		getSideLengthsList.getItem(idxInList)
	}
}
trait TriFindSides extends HasTriSideLengths {
	/*
	These are
	 */
	def findAnyLongestSide : SideNum = ???
	def findAllLongestSides : ListOfBoundedLen[SideNum] = ???  // Between one and 3 sides
	def findAnyShortestSide : SideNum = ???
	def findAllShortestSides : ListOfBoundedLen[SideNum] = ??? // Between one and 3 sides
	def reorderIncreasing : HasTriSideLengths = ???
	def reorderDecreasing : HasTriSideLengths = ???
}
// functions that check for certain features triangle MAY have, results are disjunctions: booleans, enums, options
trait TriCheckFeatures extends TriFindSides {
	def checkLegitimacy : TriLegitimacyFlavor = {
		// Suppose the triangle
		val	aLongestSideNum = findAnyLongestSide
		val aLongestSideLen = getSideLength(aLongestSideNum)
		???
	}
	/*
	We may sometimes abbreviate triSideLength as triSide or just side
	To determine if two TriSideLengths tsl1 and tsl2 are equivalent, we want to know if their side lengths are
	the same under some permutation.  If so then tsl1 and tsl2 are members of an equivalence class.
	We may also see them as members of a permutation group <-- Make this more precise and go to town.
	*/
	def hasSameSidesSameOrder(other : HasTriSideLengths) : Boolean = ???
	def hasSameSidesAnyOrder(other : HasTriSideLengths) : Boolean = ???
	def hasScaledSidesSameOrder(other : HasTriSideLengths) : Boolean = ???
	def hasScaledSidesAnyOrder(other : HasTriSideLengths) : Boolean = ???

	def areAllSidesEqual : Boolean = ???
	def areAtLeastTwoSidesEqual : Boolean = ???
	// areSidesPythagorean returns true iff findHypotenuse returns Some(hsn)
	def areSidesPythagorean : Boolean = ???  // i.e. is this a "right triangle" with a "right angle" of 90 deg.
	def findHypotenuseSide : Option[SideNum] = ??? // Index of the longest side IF this is a right triangle, else None
}
trait TriComputeMeasures extends HasTriSideLengths {
	val myComputeNumberFactory : GeneralPureNumberFactory
	val myFreeIntFactory = myComputeNumberFactory.getFreeIntFactory
	val pos02: PosIntPN = myFreeIntFactory.getPos02

	def computePerimeter() : PositivePN = {
		val (sa, sb, sc) = getSideLengthsTuple
		val perim: PureNum = sa.plusPN(sb).plusPN(sc)
		perim.asInstanceOf[SideLen]
	}
	def computeArea() : PlanarArea = {
		// First choose a base side.  We know it shouldn't matter.
		// For testing purposes we could choose randomly, sequentially, by longest, shortest, ...
		val someSideIndex = chooseRandomSide
		computeAreaFromBaseSide(someSideIndex)
	}
	def computeAreaFromHeronsFormula() : PlanarArea = {
		// https://en.wikipedia.org/wiki/Heron%27s_formula
		// A = \sqrt{s(s-a)(s-b)(s-c)},
		val perimPN = computePerimeter()

		val semiPerimPN = perimPN.divideByNonzeroPN(pos02)
		val sideLensList = getSideLengthsList
		//  (s-a) (s-b) (s-c) are each nonnegative, and can be zero only if triangle is degenerate
		// val diffs = sideLensList.map()
		// val aDif
		???
	}
	def computeAreaFromBaseSide(baseSideIndex : SideNum) : PlanarArea = {
		// Using:   area = base * height / 2
		val heightPPN: PositivePN = computeHeightAboveBaseSide(baseSideIndex)
		val basePPN: PositivePN = getSideLength(baseSideIndex)
		// We are multiplying (commut.) two positive numbers, then (assoc.) dividing by a pos Int
		val area = basePPN.timesPN(heightPPN).divideByNonzeroPN(pos02).reduceFractionPN
		area.asInstanceOf[PlanarArea]
	}

	def computeHeightAboveBaseSide(baseSideIndex : SideNum) : PlanarArea = {
		???
	}

	// Number we have to multiply this triangle's sides by to get a tri equiv to other
	def computeScaleOfSimilarTSL(other : HasTriSideLengths) : Option[PositivePN] = ???
	def scaleToSimilarTSL(scale : PositivePN) : HasTriSideLengths = ???

}
// Do we insist that these side lengths be in reduced form?
abstract class TriSideLengths[SN <: NatNumBetweenOneAndThree, SL <: PositivePN](sideA : SL, sideB : SL, sideC : SL) extends HasTriSideLengths {
	override type SideNum = SN
	override type SideLen = SL
	override type PlanarArea = PositivePN
	override def getSideLengthsTuple = (sideA, sideB, sideC)

	val mySideNumFactory : PurePosIntFactory[SN]

	override val mySide01: SN = mySideNumFactory.getPos01
	override val mySide02: SN = mySideNumFactory.getPos02
	override val mySide03: SN = mySideNumFactory.getPos03
}


class TSL_Factory[SN <: NatNumBetweenOneAndThree, SL <: PositivePN](sideNumFactory : PurePosIntFactory[SN]) {
	def mkTSL(sideA : SL, sideB : SL, sideC : SL) : HasTriSideLengths = {
		new TriSideLengths[SN, SL](sideA, sideB, sideC) {
			override val mySideNumFactory: PurePosIntFactory[SN] = sideNumFactory
		}
	}

	// def mkCandidateTSL = ???
}

sealed trait PracticeSideNum extends PureNumBaseImpl with NatNumBetweenOneAndThree

class PracticeFactoryFactory {
	val mySideNumFact = new PurePosIntFactory[PracticeSideNum] {
		override def fromPosScalaBigInt(posBI: BigInt, proofPos: ProofPositive): PracticeSideNum = ???
	}
	val myTslFact  = new TSL_Factory[PracticeSideNum, PositivePN](mySideNumFact)

}



// case class PSN_One() extends PracticeSideNum
//case class PSN_Two extends PracticeSideNum
