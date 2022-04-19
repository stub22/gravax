package fun.gravax.xtyp.mathy

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
	type ScaleNum
	type SideLenNum
	def reorderSidesIncreasing : IncrTSR

	def isIdenticalTo (other : TriSideRec) : Boolean
	def isCongruentTo (other : TriSideRec) : Boolean
	def isSimilarTo (other : TriSideRec) : Boolean

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

class TriSideImpl[SL](sideLen : SL) extends TriSide[SL] {
	override def getSideLen: SL = sideLen
}
abstract class TSR_Bland[SL](slA : SL, slB : SL, slC : SL) extends TriSideRec with ChecksTSR_Flavors {
	override type SideLenNum = SL
	private val mySidesTuple = (new TriSideImpl(slA), new TriSideImpl(slB), new TriSideImpl(slC))

	override def getSidesTuple: (TriSide[SL], TriSide[SL], TriSide[SL]) = mySidesTuple
}