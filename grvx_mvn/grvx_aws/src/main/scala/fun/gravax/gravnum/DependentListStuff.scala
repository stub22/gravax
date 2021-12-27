package fun.gravax.gravnum

import scala.collection.mutable.{Seq => MutSeq}
private trait DependentListStuff

// ListN is immutable, like all other DataThings.
// A ListN may *not* contain java-null!
trait ListN[X] extends DataThing {
	val fixedListLength : WholeIntPN
	// We must have proof that idx is in bounds!
	def getItem(idxLteqLen : WholeIntPN) : X
}
abstract class ListImplN[X](len : WholeIntPN) extends ListN[X] {
	override val fixedListLength: WholeIntPN = len
	override val getTypeURI = UriData("uri:axtyp:LIST_" + len)

	// override def getItem(idxLteqLen: WholeIntPN): X = ???
}
trait ListMaxN[X] extends DataThing {
	// List of AT MOST N elements has N+1 incarnations, as List0[T]==EmptyData, List1[T]..ListN[T]
	// possible to convert to a Union of Products
	val fixedMaxLength : WholeIntPN
}
trait ListFactory[X] {
	def verifyNoNulls(suspectArrayN : Array[X]) : Boolean = {
		val nlX : X = null.asInstanceOf[X]
		val nIdxFound = suspectArrayN.indexOf(nlX)
		return (nIdxFound == -1)
	}
	// Build an immutable ListN (guaranteed to not have nulls) from regular Java/Scala array of length N.
	// If any array value is null, the result will be None
	def makeListN(inArrayN : Array[X]) : Option[ListN[X]] = {
		val inArrLen = inArrayN.length
		val inputFreeOfNulls = verifyNoNulls(inArrayN)
		val lenAsPN : WholeIntPN = ???
		// TODO:  Think about what deep-copy would mean here
		val arrCopy: Array[X] = inArrayN.clone()
		val outFreeOfNulls = verifyNoNulls(arrCopy)
		val implList = new ListImplN[X](lenAsPN) {
			override def getItem(idxLteqLen: WholeIntPN): X = {
				// TODO:  Ensure idx in-range
				// arrCopy.apply(idxLteqLen)
				arrCopy.head
			}
		}
		Some(implList)
	}

}


// GoodData is known to be immutable and to not contain any nulls.
// GoodData is known to have reliable .hashCode and .equals.
// We attempt to make fixed-sized collections with dependent types.
sealed trait GoodData {
	val verifiedClean : Boolean = verifyIngredients // Should always be true for a successfully constructed instance
	protected def verifyIngredients : Boolean
}
case class GoodText(myTxt : String) extends GoodData {
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
trait GoodSeq[GD <: GoodData] extends GoodData {

}
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



