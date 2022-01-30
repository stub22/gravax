package fun.gravax.xtyp.catty
import cats.{Monoid, Semigroup}

private trait MonoidStuff

trait NuttyMonoids {
	def someSemigroupAntics : String = {
		val x: String = Semigroup[String].combine("Hi ", "there")
		x
	}
}

trait HistoBin[BinKey, Count] {

}

abstract class HistoBinImpl[BinKey, Count](key : BinKey, count :Count)  extends HistoBin[BinKey, Count] {
	protected def getCountMonoid : Monoid[Count] // = Monoid[Count]
	def combineCounts (cntA : Count, cntB : Count) = getCountMonoid.combine(cntA, cntB)
}
class HistoBinEasy[BinKey](key : BinKey, count : Int) extends HistoBinImpl[BinKey, Int](key, count) {
	override protected def getCountMonoid: Monoid[Int] = Monoid[Int]
}

/*
This HistoGram is an immutable data structure.

Perhaps worthwhile to see a HistoGram as ordered by BinKey?
 */
trait HistoGram[BinKey, Count]  {
	type HB = HistoBin[BinKey, Count]
	type OurHG = HistoGram[BinKey, Count]
	def getBins : Seq[HB]
	def getBinKeySet : Set[BinKey]
	def findBin(bk : BinKey) : Option[HB]
	def totalCount : Count
	def combineWith(otherHG : OurHG) : OurHG
}

class HGImpl[BinKey, Count](bins : Seq[HistoBin[BinKey, Count]]) extends HistoGram[BinKey, Count] {
	override def getBins: Seq[HB] = ???

	override def findBin(bk: BinKey): Option[HB] = ???

	override def totalCount: Count = ???

	override def getBinKeySet: Set[BinKey] = {
		???
	}

	override def combineWith(otherHG: OurHG) : OurHG = {
		// Let's reuse bins for the keys that are not overlapping.
		// We need to make new immutable bins for the overlapping keys (to hold new bin totals)
		val ourBinKeys = getBinKeySet
		val otherBinKeys = otherHG.getBinKeySet
		val comboBinKeys = ourBinKeys.union(otherBinKeys)
		val x = comboBinKeys.toSeq
		???
	}


}
trait HistoHelper[BinKey, Count] {
	type HG = HistoGram[BinKey, Count]
	// Could use MonoidK
	val histoComboMonoid = new Monoid[HG] {
		override def empty: HG = ???

		override def combine(x: HG, y: HG): HG = ???
	}
	def okSo : Unit = {
		val emptyHisto = histoComboMonoid.empty

	}
}