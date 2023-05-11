package fun.gravax.zdynamo


import fun.gravax.zdynamo.RunZioDynamoTrial.{myGenBD, myGenTN}
import zio.dynamodb.{Item, PrimaryKey, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.{Chunk, NonEmptyChunk, RIO, UIO, ZIO, Random => ZRandom}
import zio.stream.{UStream, ZStream}

import java.math.{MathContext, RoundingMode}
import scala.collection.immutable.{Queue, Map => SMap}
import scala.math.BigDecimal

private trait GenBinStuff

trait KnowsGenTypes extends KnowsBinItem with KnowsDistribTypes {
	// Pure-data generated based on rules.  These tuples are used as stream records that do not need as much concreteness.
	// type BaseMassyMeatRow = (BinTagInfo, BinNumInfo, (BigDecimal, BinMeatInfo))
	type BinSpec = (BinTagInfo, BinNumInfo, BinMassInfo, BinMeatInfo) // Final agg of pure-data based on gen-rules
	type BinStoreCmdRow = (BinSpec, Item, PrimaryKey, RIO[ZDynDBExec, Option[Item]])
	type BinStoreRslt = (BinSpec, PrimaryKey, Option[Item])
	val zeroBD = BigDecimal("0.0")

	type LevelNum = Int
	type LevelTagNumChnk = NonEmptyChunk[(BinTagInfo, BinNumInfo)]

	type VirtRsltRow = (BinTagInfo, BinNumInfo, DBinWt, StatRow)
}

trait GenBinData extends KnowsGenTypes {

	val myTBI : ToBinItem

	// Combine the finite tree structure of the tagNumChnk (known number of records) with the stream of bin data (often random).
	// Presume that mmStrm.size >= baseTagNumChunk.size.
	def joinMassyMeatRows(baseTagNumChunk : NonEmptyChunk[(BinTagInfo, BinNumInfo)], mmStrm : UStream[(BinMassInfo, BinMeatInfo)]) : UStream[BinSpec] = {
		val btnStrm = ZStream.fromChunk(baseTagNumChunk)
		btnStrm.zipWith(mmStrm)((tagNumPair, massMeatPair) => (tagNumPair._1, tagNumPair._2, massMeatPair._1, massMeatPair._2))
	}

	// Absolute-weight field is the sticking point.  We need to know the total mass (of the distribution, == sum of all leaf bins)
	// before we write the first bin to DB.  Otherwise we have to make a second pass to store absWeight.
	// We have not yet written any app code that uses absolute weight.
	// 2023-05-03 :  AbsoluteWeight is disabled until further notice.
	def makeBaseBinStoreCmds(tblNm : String, scenID : String, timeInf : BinTimeInfo)(baseBinSpecStrm : UStream[BinSpec]) : UStream[BinStoreCmdRow] = {
		val skelBintem: Item = myTBI.mkBinItemSkel(scenID, timeInf)
		val binLevelStoreTupStrm: UStream[BinStoreCmdRow] = baseBinSpecStrm.map(bbSpec => {
			val (tagInfo, numInfo, massInfo, binMeat) = bbSpec
			val baseBinItem = buildBinItem(skelBintem, tagInfo, massInfo, binMeat)
			val ourPK: PrimaryKey = myTBI.getFBI.getPKfromFullBinItem(baseBinItem)
			val putDynQry: ZDynDBQry[Any, Option[Item]] = ZDynDBQry.putItem(tblNm, baseBinItem)
			val putDynZIO: RIO[ZDynDBExec,Option[Item]] = putDynQry.execute
			(bbSpec, baseBinItem, ourPK, putDynZIO)
		})
		binLevelStoreTupStrm
	}

	def buildBinItem(skelBinItem : Item, tagInfo: BinTagInfo,  massInfo : BinMassInfo, binMeat : BinMeatInfo) : Item = {
		val binItemWithTags = myTBI.addTagsToBinItem(skelBinItem, tagInfo)
		val binItemWithMass = myTBI.addMassInfoToBinItem(binItemWithTags, massInfo)
		val binItemWithMeat = myTBI.addMeatToBinItem(binItemWithMass, binMeat)

		val fullBI = myTBI.fillBinSortKey(binItemWithMeat)
		fullBI
	}

	// To process a Stream-of-ZIO we can use mapZIO, or more awkwardly runFoldZIO.
	def compileBinLevelStoreOp(storeCmdStrm : UStream[BinStoreCmdRow]) : RIO[ZDynDBExec, Chunk[BinStoreRslt]] = {
		val wovenCmdStream: ZStream[ZDynDBExec, Throwable, BinStoreRslt] = storeCmdStrm.mapZIO(cmdRow => {
			val (binSpec, binItem, binPK, binCmd) = cmdRow
			val enhCmd: RIO[ZDynDBExec, BinStoreRslt] = binCmd.map(rsltOptItm => (binSpec, binPK, rsltOptItm))
			enhCmd
		})
		val chnky: RIO[ZDynDBExec, Chunk[BinStoreRslt]] = wovenCmdStream.runCollect
		chnky
	}

	def OLDE_computeParentMasses(baseRsltChnk : Chunk[BinStoreRslt]): SMap[String, BigDecimal] = {
		val emptyParentMassMap = SMap[String,BigDecimal]()
		val parentMasses: SMap[String, BigDecimal] = baseRsltChnk.foldLeft(emptyParentMassMap)((prevMassMap, nxtBGRR) => {
			val ptag : String = nxtBGRR._1._1.parentTag
			val rowMass: BigDecimal = nxtBGRR._1._3.binMass
			prevMassMap.updatedWith(ptag)(prevMass_opt => {
				val updatedTotalMassForParent: BigDecimal = prevMass_opt.fold(rowMass)(_.+(rowMass))
				Some(updatedTotalMassForParent)
			})
		})
		println(s"Computed parentMassMap: ${parentMasses}")
		val massGrndTot = parentMasses.foldLeft(zeroBD)((prevTot, nxtKV) => prevTot.+(nxtKV._2))
		println(s"Computed grand total mass: ${massGrndTot}")
		parentMasses
	}
}