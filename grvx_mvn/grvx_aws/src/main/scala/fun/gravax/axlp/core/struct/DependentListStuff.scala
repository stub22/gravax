package fun.gravax.axlp.core.struct

import fun.gravax.aa.yafl.core.YaflFinList
import fun.gravax.axlp.core.num.WholeIntPN
import fun.gravax.axlp.full.{DataThing, UriData}

import scala.collection.immutable.{Seq => ImmSeq}

private trait DependentListStuff

// TODO:  Contrast with HList (Shapeless for Scala2) and Scala 3 constructs

// ListXyzN could equally well be called VectorXyzN or VecXyzN or SeqXyzN.  But we choose list for alignment
// (in this spot) with traditional computer science.  We wish to save word "Vector" for spatial meanings.
// Seq is also a good word but ImmSeq is our inner implementation of List, so we like to isolate that name.
// ListXyzN types are immutable pseudo-dependent types functionally equivalent to ImmSeq, with
// extension to flow as typed YaflCoreData (like all other axLam DataThings).
// A ListXyzN may *not* contain java-null!
// ListN corresponds to list-of-N-items-of-type-X types treated by AxLam as having known finite length
// ListN is empty iff N is 0.
// List0[X] is the empty list (with dangling type trauma we seek to erase)

trait DataThingKnowsLLTy extends DataThing {
	type ListLenTy <: WholeIntPN  // In Scala 3 may be set to value type e.g. "7", union type e.g. "7 | 9", ...
}
// ListMaxN is type of a list which may be any length from 0 to N.
trait ListMaxN[Item]  extends DataThingKnowsLLTy {

	// List of AT MOST N elements has N+1 incarnations, as List0[T]==EmptyData, List1[T]..ListN[T]
	// possible to convert to a Union of Products
	val fixedMaxLength : ListLenTy

	def toImmSeqMaxN : ImmSeq[Item] // Will be of some unknown len s.t.  0 <= len <= N

}
trait ListMinM[Item] extends DataThingKnowsLLTy {
	val fixedMinLength : ListLenTy
	def toImmSeqMinM : ImmSeq[Item] // Will be of some unknown len s.t.  M <= len
}

trait ListOfBoundedLen[Item] extends ListMaxN[Item] with ListMinM[Item]  {
	def toImmSeqOfBoundedLen : ImmSeq[Item] // Will be of some unknown len st.  M <= len <= N

	override def toImmSeqMinM: ImmSeq[Item] = toImmSeqOfBoundedLen

	override def toImmSeqMaxN: ImmSeq[Item] = toImmSeqOfBoundedLen
}

trait FinListN[Item] extends ListOfBoundedLen[Item] with YaflFinList {
	val fixedListLength :  ListLenTy
	// We must have proof that idx is in bounds!
	def getItem(idxLteqLen : WholeIntPN) : Item
	def toImmSeqOfKnownLen : ImmSeq[Item]
	override def toImmSeqOfBoundedLen: ImmSeq[Item] = toImmSeqOfKnownLen

	override val fixedMinLength: ListLenTy = fixedListLength

	override val fixedMaxLength: ListLenTy = fixedListLength

	def concat(other : FinListN[Item]) : FinListN[Item]

	// So...now we have to define map?!

	def map[TgtItem](f : Function[Item,TgtItem]) : FinListN[TgtItem]

}
abstract class ListImplN[T_Itm, T_LstLn <: WholeIntPN](len : T_LstLn) extends FinListN[T_Itm] {
	override type ListLenTy = T_LstLn
	override val fixedListLength: ListLenTy = len
	override val instTypeURI = UriData("uri:axtyp:LIST_" + len)

	// override def getItem(idxLteqLen: WholeIntPN): X = ???
}

trait ListFactory[T_Itm] {

	def verifyNoNulls(suspectArrayN : Array[T_Itm]) : Boolean = {
		// Note this method is not safe against race conditions, if some parallel thread is writing nulls into array.
		val nlX : T_Itm = null.asInstanceOf[T_Itm]
		val nIdxFound = suspectArrayN.indexOf(nlX)
		return (nIdxFound == -1)
	}
	val flag_checkPrelim : Boolean = true
	// Build an immutable ListN (guaranteed to not have nulls) from regular Java/Scala array of length N.
	// If any array value is null, the result will be None
	def makeListN(inArrayN : Array[T_Itm]) : Option[FinListN[T_Itm]] = {
		// flag_checkPrelim => attempt to fail-fast by preliminary check before we copy.
		val candv : Boolean =  !flag_checkPrelim || verifyNoNulls(inArrayN)
		if (candv) copyAndVerify(inArrayN) else None
	}
	// Here we decide the actual number-type of the list length.
	// This decision may be pushed closer to the input data, perhaps to ultimately yield Scala 3 value types,
	// or something like Refine+Shapeless setup, or a proposition type.  Regarding refinement types vs. dependent
	// types, J. Carette has warned regarding shifting between type and proposition space as being unhelpful
	// sometimes to an author brain.
	type T_LLen <: WholeIntPN
	def tmp_mkWIPN (n : Int) : T_LLen = ???

	def copyAndVerify(inArrayN : Array[T_Itm]) : Option[FinListN[T_Itm]] = {
		// TODO:  Think about what deep-copy would mean here
		val arrCopy: Array[T_Itm] = inArrayN.clone()
		val inArrLen = inArrayN.length
		val lenWIPN : T_LLen = tmp_mkWIPN(inArrLen)
		// Now we have a private copy, which we should be able to reliably check for nulls.
		val outFreeOfNulls = verifyNoNulls(arrCopy)
		val implList = new ListImplN[T_Itm, T_LLen](lenWIPN) {
			override def getItem(idxLteqLen: WholeIntPN): T_Itm = {
				// TODO:  Ensure idx in-range
				// arrCopy.apply(idxLteqLen)
				arrCopy.head
			}

			override def toImmSeqOfKnownLen: ImmSeq[T_Itm] = ???

			override def concat(other: FinListN[T_Itm]): FinListN[T_Itm] = ???

			override def map[TgtItem](f: Function[T_Itm, TgtItem]): FinListN[TgtItem] = {
				???
			}
		}
		Some(implList)
	}

}





