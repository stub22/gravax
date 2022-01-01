package fun.gravax.gravnum

import scala.collection.mutable.{Seq => MutSeq}

private trait GoodDataStuff

// GoodData is known to be immutable and to not contain any nulls.
// GoodData is known to have reliable .hashCode and .equals.
// We attempt to make fixed-sized collections with dependent types.
sealed trait GoodData {
	val verifiedClean : Boolean = verifyIngredients // Should always be true for a successfully constructed instance
	protected def verifyIngredients : Boolean
}
case class GoodText(myTxt : String) extends GoodData with YaflTxtDat {
	/*
	override val verifiedClean: Boolean = {
		if (txt == null) {
			throw new Exception("CleanText constructed with null input!")
		}
	}
	 */
	override protected def verifyIngredients: Boolean = {
		// https://www.baeldung.com/scala/assert-vs-require
		require(myTxt != null) // Require throws IllegalArgumentException, may not be disabled.
		myTxt != null // Should not be able to return false.
	}
}
// import scala.collection.immutable.ArraySeq  added in 2.13
import scala.collection.immutable.{Seq => ImmSeq, ArraySeq => ImmArraySeq, Map => ImmMap}

trait GoodSeq[GD <: GoodData] extends GoodData {}

case class GoodArr[GD <: GoodData](inSeq : ImmSeq[GD]) extends GoodSeq[GD] {
	override protected def verifyIngredients: Boolean = {
		val seqLen = inSeq.length
		var verCount = 0 	// Added this silly count just to make SURE foreach can't silently skip nulls (unless "length" also does!)
		inSeq.foreach(goodEl => {
			require(goodEl != null)
			require(goodEl.verifiedClean)
			verCount += 1
		})
		require(verCount == seqLen)
		true
	}
}

trait CleanDataFactory {
	val flg_deepCheckInput : Boolean
	def captureMutSeq[GD <: GoodData](mutSeq : MutSeq[GD]) : GoodSeq[GD] = {
		// Input seq may be mutable, in which case we *must* copy it.
		// For now we always copy
		val immSeq = mutSeq.toSeq
		GoodArr(immSeq)
	}
}

