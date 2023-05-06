package fun.gravax.zdynamo


import zio.dynamodb.{Item, PrimaryKey, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.{Chunk, NonEmptyChunk, RIO, UIO, ZIO, Random => ZRandom}
import zio.stream.{UStream, ZStream}

import java.math.{MathContext, RoundingMode}
import scala.collection.immutable.{Queue, Map => SMap}
import scala.math.BigDecimal

private trait GenBinStuff

trait GenBinData extends KnowsBinItem {

	val myTBI : ToBinItem

	// Pure-data generated based on rules.  These tuples are used as stream records that do not need as much concreteness.
	// type BaseMassyMeatRow = (BinTagInfo, BinNumInfo, (BigDecimal, BinMeatInfo))
	type BaseBinSpec = (BinTagInfo, BinNumInfo, (BigDecimal, BinMeatInfo)) // Final agg of pure-data based on gen-rules
	type BaseBinStoreCmdRow = (BaseBinSpec, Item, PrimaryKey, RIO[ZDynDBExec, Option[Item]])
	type BaseGenRsltRec = (BaseBinSpec, PrimaryKey, Option[Item])


	// Combine the finite tree structure of the tagNumChnk (known number of records) with the stream of bin data (often random).
	// Presume that mmStrm.size >= baseTagNumChunk.size.
	def joinMassyMeatRows(baseTagNumChunk : NonEmptyChunk[(BinTagInfo, BinNumInfo)], mmStrm : UStream[(BigDecimal, BinMeatInfo)]) : UStream[BaseBinSpec] = {
		val btnStrm = ZStream.fromChunk(baseTagNumChunk)
		btnStrm.zip(mmStrm)
	}

	// Absolute-weight field is the sticking point.  We need to know the total mass (of the distribution, == sum of all leaf bins)
	// before we write the first bin to DB.  Otherwise we have to make a second pass to store absWeight.
	// We have not yet written any app code that uses absolute weight.
	// 2023-05-03 :  AbsoluteWeight is disabled until further notice.


	def makeBaseBinStoreCmds(tblNm : String, scenID : String, timeInf : BinTimeInfo)(baseBinSpecStrm : UStream[BaseBinSpec]) : UStream[BaseBinStoreCmdRow] = {
		val skelBintem: Item = myTBI.mkBinItemSkel(scenID, timeInf)
		val binLevelStoreTupStrm: UStream[BaseBinStoreCmdRow] = baseBinSpecStrm.map(mmRow => {
			val (tagInfo, numInfo, (binMass, binMeat)) = mmRow
			val baseBinItem = buildBaseBinItem(skelBintem, tagInfo, numInfo, binMass, binMeat)
			val ourPK: PrimaryKey = myTBI.getFBI.getPKfromFullBinItem(baseBinItem)
			val putDynQry: ZDynDBQry[Any, Option[Item]] = ZDynDBQry.putItem(tblNm, baseBinItem)
			val putDynZIO: RIO[ZDynDBExec,Option[Item]] = putDynQry.execute
			(mmRow, baseBinItem, ourPK, putDynZIO)
		})
		binLevelStoreTupStrm
	}

	// numInfo is not used currently.  binMass is passed as bare scalar, since we don't have any rel-weights yet.
	// Would be OK and more general to pass binMassInfo with None in the relWeight_opt.
	def buildBaseBinItem(skelBinItem : Item, tagInfo: BinTagInfo, numInfo: BinNumInfo, binMass : BigDecimal, binMeat : BinMeatInfo) : Item = {
		val binItemWithTags = myTBI.addTagsToBinItem(skelBinItem, tagInfo)
		val binItemWithMass = myTBI.addMassToBinItem(binItemWithTags, binMass)
		val binItemWithMeat = myTBI.addMeatToBinItem(binItemWithMass, binMeat)

		val fullBI = myTBI.fillBinSortKey(binItemWithMeat)
		fullBI
	}


}