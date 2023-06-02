package fun.gravax.zaxlam.xform

import fun.gravax.distrib.gen.{KnowsGenTypes}

import java.util.{List => JList, Map => JMap}
import scala.collection.immutable.{Map => SMap}
import scala.jdk.CollectionConverters._

private trait  PortStatTransformStuff

trait StatMapKeys {
	val MAPKEY_SYM = "ENTRY_SYM"
	val MAPKEY_MEAN = "ENTRY_MEAN"
	val MAPKEY_VAR = "ENTRY_VAR"
	val MAPKEY_COV = "ENTRY_COV"
}

trait PortStatXforms extends ZaxTypes with  KnowsGenTypes with StatMapKeys {

	type NumTxt = String

	type PerEntryField = String

	def matrixToSMap(stm : StatTriMatrix) : SMap[EntryKey, AnyRef]  = {
		stm.map(pair => {
			val ekey = pair._1._1
			val entrySMap = entryPairToSMap(pair)
			val entryJMap = entrySMap.asJava
			(ekey -> entryJMap)
		}).toMap
	}

	def entryPairToSMap(entryPair : (StatEntry, VwtCovRow)) : SMap[PerEntryField, AnyRef] = {
		val (statEnt, covRow) = entryPair
		val (ekey, emean, evar) = statEnt
		val emeanTxt : NumTxt = emean.toString()
		val evarTxt : NumTxt = evar.toString()
		val covRowSMap = covRowToSMap(covRow)
		// FIXME: Unjigger this Map-conversion mess!
		// Here the entryKey is the key of the *partner* entry that we share the covariance with.
		val covRowJMap: JMap[EntryKey, NumTxt] = covRowSMap.asJava
		val entryResultSMap = SMap[PerEntryField, AnyRef](MAPKEY_SYM -> ekey, MAPKEY_MEAN -> emeanTxt,
			MAPKEY_VAR -> evarTxt, MAPKEY_COV -> covRowJMap)
		entryResultSMap
	}

	def covRowToSMap(covRow: VwtCovRow) : SMap[EntryKey, NumTxt] = {
		val covMap: SMap[EntryKey, NumTxt] = covRow.map(covTup => {
			val (outerKey, innerKey, cov) = covTup
			val covTxt = cov.toString()
			(innerKey -> covTxt)
		}).toMap
		covMap
	}
	// type StatTriMatrix = IndexedSeq[(StatEntry, VwtCovRow)]
	// type StatEntry = (EntryKey, EntryMean, EntryVar)
	// type VwtCovRow = IndexedSeq[VwtCovTup]
	// type VwtCovTup = (EntryKey, EntryKey, VwtCov)
	// type VwtCov = BigDecimal
	// type EntryKey = String

	// type ZaxSMap = SMap[String, AnyRef]
	// type ZaxFlag = Boolean
	// type ZaxErr = (AnyRef,String)
	// type ZaxResult=Either[ZaxErr,ZaxSMap]
	// TODO:  Make number encoding more flexible.
	def mkZaxResultForStatMatrix(stm : StatTriMatrix) : ZaxResult = {
		val outSMap : SMap[EntryKey, AnyRef] = matrixToSMap(stm)
		// val outJMap = outSMap.asJava
		Right(outSMap)
	}
}
