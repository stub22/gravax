package test.gravax

import fun.gravax.gravnum.{FinListN, GenIntFactory, ListOfBoundedLen, PosIntPN, PositivePN, PureNum, WholeIntPN, ZeroPN}

private trait TriangleStuff

/*
Study Example:  Triangles
// Triangle has 3 side-lengths in some order
// Here "index" is 1-based
//   	sideA at index==1
//  	sideB at index==2
//		sideC at index==3
 */

trait NatNumBetweenOneAndThree extends PosIntPN
trait WholeNumBetweenZeroAndTwo extends WholeIntPN

trait KnowsTriSideIndex {
	type SideNum = NatNumBetweenOneAndThree
	def otherSidesInOrder(someSide : SideNum) : FinListN[SideNum] = ??? // List of length 2
	def chooseRandomSide : SideNum = ???
}
// Our version of an Enum is based on Flavor Tags.  (Not "Kind", not "Type", not "Category", not "Class")
case class TriLegitimacyFlavorTag(flavorName : String)
object TriLegitFlavorTags {
	val TLFN_ORDINARY 	= TriLegitimacyFlavorTag("ORDINARY")	// Interior point of legitimacy, has positive area
	val TLFN_DEGEN 		= TriLegitimacyFlavorTag("DEGENERATE") 	// On boundary of legitimacy, has zero area
	val TLFN_ILLEGAL	= TriLegitimacyFlavorTag("ILLEGAL")		// Outside boundary of legitimacy, not a planar triangle
}
trait HasTriSideLengths extends KnowsTriSideIndex {
	val myNumFactory : GenIntFactory = ???

	type SideLen <: PositivePN
	// If we allow for degen-tri then Area could be Zero, so we actually want NonnegPN
	type PlanarArea <: PositivePN

	def getSideLengthsTuple : (SideLen, SideLen, SideLen)

	// "List" indexes are 0-based
	def getSideLengthsList : FinListN[SideLen] = ???

	def getSideLength(snum : SideNum) : SideLen = {
		val pos01 = myNumFactory.getPos01
		//
		val idxInList = snum.minusPN(pos01).asInstanceOf[WholeNumBetweenZeroAndTwo]
		getSideLengthsList.getItem(idxInList)
	}


	def computeArea() : PlanarArea = {
		// First choose a base side.  We know it shouldn't matter.
		// For testing purposes we could choose randomly, sequentially, by longest, shortest, ...
		val someSideIndex = chooseRandomSide
		computeAreaFromBaseSide(someSideIndex)
	}
	def computeAreaFromBaseSide(baseSideIndex : SideNum) : PlanarArea = {
		// Using:   area = base * height / 2
		val heightPPN: PositivePN = computeHeightAboveBaseSide(baseSideIndex)
		val basePPN: PositivePN = getSideLength(baseSideIndex)
		val pos02: PosIntPN = myNumFactory.getPos02
		// We are multiplying (commut.) two positive numbers, then (assoc.) dividing by a pos Int
		val area = basePPN.timesPN(heightPPN).divideByNonzeroPN(pos02).reduceFractionPN
		area.asInstanceOf[PlanarArea]
	}
	def computeHeightAboveBaseSide(baseSideIndex : SideNum) : PlanarArea = {
		???
	}
	def findAnyLongestSide : SideNum = ???
	def findAllLongestSides : ListOfBoundedLen[SideNum] = ???  // Between one and 3 sides
	def findAnyShortestSide : SideNum = ???
	def findAllShortestSides : ListOfBoundedLen[SideNum] = ??? // Between one and 3 sides

	def checkLegitTriangle : Boolean = {
		// Suppose the triangle
		val	aLongestSideNum = findAnyLongestSide
		val aLongestSideLen = getSideLength(aLongestSideNum)
		???
	}

	def areAllSidesEqual : Boolean = ???
	def areAtLeastTwoSidesEqual : Boolean = ???
	def areSidesPythagorean : Boolean = ???  // i.e. is this a "right triangle" with a "right angle" of 90 deg.
	def findHypotenuseIndex : Option[SideNum] = ??? // Index of the longest side IF this is a right triangle, else None

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

	// Number we have to multiply this triangle's sides by to get a tri equiv to other
	def computeScaleOfSimilarTSL(other : HasTriSideLengths) : Option[PositivePN] = ???
	def scaleToSimilarTSL(scale : PositivePN) : HasTriSideLengths = ???

	def reorderIncreasing : HasTriSideLengths = ???
	def reorderDecreasing : HasTriSideLengths = ???
}
// Do we insist that these side lengths be in reduced form?
case class TriSideLengths[SL <: PositivePN](sideA : SL, sideB : SL, sideC : SL) extends HasTriSideLengths {
	override type SideLen = SL
	override type PlanarArea = PositivePN
	override def getSideLengthsTuple = (sideA, sideB, sideC)
}

class TSL_Factory {
	def mkTSL(sideA : PositivePN, sideB : PositivePN, sideC : PositivePN) : HasTriSideLengths
		= TriSideLengths(sideA, sideB, sideC)

	def mkCandidateTSL = ???
}

