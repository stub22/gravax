package fun.gravax.xtyp.mathy

import spire.math.{Rational => SpRat}

private trait TriSideRecStuff

// SideLen must be an exact number type (or at least have explicit precision)
trait TriSide[SideLen] {
	def getSideLen : SideLen
	def hasSameLengthAs(otherSide : TriSide[SideLen]) : Boolean = {
		val otherLen = otherSide.getSideLen
		val ourLen = getSideLen
		/*
		Scala 2.13 sez: "comparing values of types SideLen and SideLen using `equals` unsafely bypasses
		cooperative equality; use `==` instead
		 */
		ourLen == otherLen
	}
	def isGluableTo (otherSide : TriSide[SideLen]) : Boolean = {
		hasSameLengthAs(otherSide)
	}

}
trait TriSideRec {
	type ScaleNum	// The type of number I can be scaled by
	type SideLenNum	// The type of number used in my side lengths
	def reorderSidesIncreasing : IncrTSR
	def areSidesIncreasing : Boolean

	def isIdenticalTo (other : TriSideRec) : Boolean
	def isCongruentTo (other : TriSideRec) : Boolean
	def isSimilarTo (other : TriSideRec) : Boolean
	def findScaleTo (other : TriSideRec) : Option[ScaleNum]

	def scaleBy(scl : ScaleNum) : TriSideRec // But what do we know about the type-params of the result?


	def countGluingOpportunities (other : TriSideRec) : Boolean

	def getSidesTuple : Tuple3[TriSide[SideLenNum], TriSide[SideLenNum], TriSide[SideLenNum]]
}

trait ChecksTSR_Flavors {
	def isMalformed: Boolean
	def isInconsistent: Boolean
	def isDegenerate: Boolean
	def isOrdinary: Boolean
	def isRight: Boolean
	def isIsosceles: Boolean
	def isEquilateral: Boolean
	def isScalene: Boolean
}
abstract class TSRFlavorTag(tagName : String) {
	def getTagName : String = tagName
	def checkTagFor(fc : ChecksTSR_Flavors) : Boolean		// Does fc have our flavor?
}
object TSRFlavorTags {
	val FT_MALFORMED = new TSRFlavorTag("MALFORMED") {
		override def checkTagFor(fc: ChecksTSR_Flavors): Boolean = fc.isMalformed
	}
	val allTags = List(FT_MALFORMED)
}
/*
case class TSRFT_MALFORMED() extends TSRFlavorTag {
	override def checkTagFor(fc: ChecksTSR_Flavors): Boolean = fc.isMalformed
}
case class TSRFT_INCONSISTENT() extends TSRFlavorTag {
	override def checkTagFor(fc: ChecksTSR_Flavors): Boolean = fc.isInconsistent
}
case class TSRFT_ORDINARY() extends TSRFlavorTag {
	override def checkTagFor(fc: ChecksTSR_Flavors): Boolean = fc.isOrdinary
}
case class TSRFT_ISOSCELES() extends TSRFlavorTag {
	override def checkTagFor(fc: ChecksTSR_Flavors): Boolean = fc.isOrdinary
}
*/

trait FlavorCollector {

	val checkableTags: Seq[TSRFlavorTag] = TSRFlavorTags.allTags //  List(TSRFT_MALFORMED(), TSRFT_INCONSISTENT(), TSRFT_ORDINARY(), TSRFT_ISOSCELES())
	def collectFlavorOpts(flavorChkr : ChecksTSR_Flavors) : Seq[Option[TSRFlavorTag]] = {
		checkableTags.map(tag => {
			if (tag.checkTagFor(flavorChkr)) Some(tag) else None
		})
	}
	def collectKnownFlavors(flavorChkr : ChecksTSR_Flavors) : Seq[TSRFlavorTag] = collectFlavorOpts(flavorChkr).flatten
}

trait IncrTSR {

}

trait DecrTSR

private class TriSideImpl[SL](sideLen : SL) extends TriSide[SL] {
	override def getSideLen: SL = sideLen
}
protected abstract class TSR_Bland[SL](slA : SL, slB : SL, slC : SL) extends TriSideRec with ChecksTSR_Flavors {
	override type SideLenNum = SL
	private val mySidesTuple = (new TriSideImpl(slA), new TriSideImpl(slB), new TriSideImpl(slC))

	override def getSidesTuple: (TriSide[SL], TriSide[SL], TriSide[SL]) = mySidesTuple
}

class TSR_Rat (slenA : SpRat, slenB : SpRat, slenC : SpRat) extends TSR_Bland[SpRat](slenA, slenB, slenC) {
	override type ScaleNum = SpRat

	override def reorderSidesIncreasing: IncrTSR = ???

	override def areSidesIncreasing: Boolean = ???

	override def isIdenticalTo(other: TriSideRec): Boolean = ???

	override def isCongruentTo(other: TriSideRec): Boolean = ???

	override def isSimilarTo(other: TriSideRec): Boolean = ???

	override def findScaleTo(other: TriSideRec): Option[SpRat] = ???

	override def scaleBy(scl: SpRat): TriSideRec = ???

	override def countGluingOpportunities(other: TriSideRec): Boolean = ???

	override def isMalformed: Boolean = ???

	override def isInconsistent: Boolean = ???

	override def isDegenerate: Boolean = ???

	override def isOrdinary: Boolean = ???

	override def isRight: Boolean = ???

	override def isIsosceles: Boolean = ???

	override def isEquilateral: Boolean = ???

	override def isScalene: Boolean = ???
}