package fun.gravax.gravnum

import scala.collection.immutable.{Seq => ImmSeq, ArraySeq => ImmArraySeq, Map => ImmMap}

private trait DependentListStuff

// ListN is immutable, like all other DataThings.
// A ListN may *not* contain java-null!
// ListN corresponds to list-of-N-items-of-type-X types treated by AxLam as having known finite length
// ListN is empty iff N is 0.
// List0[X] is the empty list (with dangling type trauma we seek to erase)

// ListMaxN is type of a list which may be any length from 0 to N.
trait ListMaxN[Item] extends DataThing {
	// List of AT MOST N elements has N+1 incarnations, as List0[T]==EmptyData, List1[T]..ListN[T]
	// possible to convert to a Union of Products
	val fixedMaxLength : WholeIntPN

	def toImmSeqMaxN : ImmSeq[Item] // Will be of some unknown len s.t.  0 <= len <= N

}
trait ListMinM[Item] extends DataThing {
	val fixedMinLength : WholeIntPN
	def toImmSeqMinM : ImmSeq[Item] // Will be of some unknown len s.t.  M <= len
}

trait ListOfBoundedLen[Item] extends ListMaxN[Item] with ListMinM[Item] {
	def toImmSeqOfBoundedLen : ImmSeq[Item] // Will be of some unknown len st.  M <= len <= N

	override def toImmSeqMinM: ImmSeq[Item] = toImmSeqOfBoundedLen

	override def toImmSeqMaxN: ImmSeq[Item] = toImmSeqOfBoundedLen
}

trait FinListN[Item] extends ListOfBoundedLen[Item] with YaflFinList {
	val fixedListLength : WholeIntPN
	// We must have proof that idx is in bounds!
	def getItem(idxLteqLen : WholeIntPN) : Item
	def toImmSeqOfKnownLen : ImmSeq[Item]
	override def toImmSeqOfBoundedLen: ImmSeq[Item] = toImmSeqOfKnownLen

	override val fixedMinLength: WholeIntPN = fixedListLength

	override val fixedMaxLength: WholeIntPN = fixedListLength

	def concat(other : FinListN[Item]) : FinListN[Item]
}
abstract class ListImplN[Item](len : WholeIntPN) extends FinListN[Item] {
	override val fixedListLength: WholeIntPN = len
	override val instTypeURI = UriData("uri:axtyp:LIST_" + len)

	// override def getItem(idxLteqLen: WholeIntPN): X = ???
}

trait ListFactory[Item] {

	def verifyNoNulls(suspectArrayN : Array[Item]) : Boolean = {
		// Note this method is not safe against race conditions, if some parallel thread is writing nulls into array.
		val nlX : Item = null.asInstanceOf[Item]
		val nIdxFound = suspectArrayN.indexOf(nlX)
		return (nIdxFound == -1)
	}
	val flag_checkPrelim : Boolean = true
	// Build an immutable ListN (guaranteed to not have nulls) from regular Java/Scala array of length N.
	// If any array value is null, the result will be None
	def makeListN(inArrayN : Array[Item]) : Option[FinListN[Item]] = {
		// flag_checkPrelim => attempt to fail-fast by preliminary check before we copy.
		val candv : Boolean =  !flag_checkPrelim || verifyNoNulls(inArrayN)
		if (candv) copyAndVerify(inArrayN) else None
	}
	def tmp_mkWIPN (n : Int) : WholeIntPN = ???
	def copyAndVerify(inArrayN : Array[Item]) : Option[FinListN[Item]] = {
		// TODO:  Think about what deep-copy would mean here
		val arrCopy: Array[Item] = inArrayN.clone()
		val inArrLen = inArrayN.length
		val lenWIPN = tmp_mkWIPN(inArrLen)
		// Now we have a private copy, which we should be able to reliably check for nulls.
		val outFreeOfNulls = verifyNoNulls(arrCopy)
		val implList = new ListImplN[Item](lenWIPN) {
			override def getItem(idxLteqLen: WholeIntPN): Item = {
				// TODO:  Ensure idx in-range
				// arrCopy.apply(idxLteqLen)
				arrCopy.head
			}

			override def toImmSeqOfKnownLen: ImmSeq[Item] = ???

			override def concat(other: FinListN[Item]): FinListN[Item] = ???
		}
		Some(implList)
	}

}





