package fun.gravax.distrib.gen

import fun.gravax.distrib.binstore.{BinStoreCmdBuilder, KeyedCmdMaker, KnowsBinItem}
import fun.gravax.distrib.calc.KnowsDistribTypes
import fun.gravax.distrib.struct.{BinFullKeyInfo, BinMassInfo, BinMeatInfo, BinNumInfo, BinTagInfo, BinTimeInfo}
import fun.gravax.zdynamo._
import zio.cache.Cache
import zio.dynamodb.{Item, PrimaryKey, DynamoDBExecutor => ZDynDBExec}
import zio.stream.{UStream, ZStream}
import zio.{Chunk, NonEmptyChunk, RIO, ZIO}

import scala.collection.immutable.{Map => SMap}

private trait GenBinStuff

trait KnowsBinTupTupTypes {
	type BinSpec = (BinTagInfo, BinNumInfo, BinMassInfo, BinMeatInfo) // Final agg of pure-data based on gen-rules

	type BinScalarInfoTup = (BinTimeInfo, BinTagInfo, BinMassInfo)

	type MeatyItemCache = Cache[BinFullKeyInfo, Throwable, Option[BinMeatInfo]]
}
trait KnowsGenTypes extends KnowsBinItem with KnowsDistribTypes with KnowsBinTupTupTypes {
	// Pure-data generated based on rules.  These tuples are used as stream records that do not need as much concreteness.

	type BinStoreCmdRow = (BinSpec, Item, PrimaryKey, RIO[ZDynDBExec, Option[Item]])
	type BinStoreRslt = (BinSpec, PrimaryKey, Option[Item])
	val zeroBD = BigDecimal("0.0")

	type LevelNum = Int
	type LevelTagNumChnk = NonEmptyChunk[(BinTagInfo, BinNumInfo)]

	type VirtRsltRow = (BinTagInfo, BinNumInfo, DBinWt, StatRow)

	type BaseRsltPair = (GoodTagNumBlk, Chunk[BinStoreRslt])
}


trait GenBinData extends KnowsGenTypes {

	// val myTBI : ToBinItem

	// Combine the finite tree structure of the tagNumChnk (known number of records) with the stream of bin data (often random).
	// Presume that mmStrm.size >= baseTagNumChunk.size.
	def joinMassyMeatRows(baseTagNumChunk : NonEmptyChunk[(BinTagInfo, BinNumInfo)], mmStrm : UStream[(BinMassInfo, BinMeatInfo)]) : UStream[BinSpec] = {
		val btnStrm = ZStream.fromChunk(baseTagNumChunk)
		btnStrm.zipWith(mmStrm)((tagNumPair, massMeatPair) => (tagNumPair._1, tagNumPair._2, massMeatPair._1, massMeatPair._2))
	}

	// We use this as an independent debug check.  Is not used in our main sequence.
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
		println(s"println: OLDE_computeParentMasses: Computed parentMassMap: ${parentMasses}")
		val massGrndTot = parentMasses.foldLeft(zeroBD)((prevTot, nxtKV) => prevTot.+(nxtKV._2))
		println(s"println: OLDE_computeParentMasses: Computed grand total mass: ${massGrndTot}")
		parentMasses
	}
}
// These integer class-constructor parameters determine the shape of this generator setup.
abstract class BlockBaseGen(rootTagNum : Int, rootKidsCnt : Int, baseBinLevel : Int) extends KnowsGenTypes {
	protected def getGenBD : GenBinData
	protected def getGenTN : GenTagNumData
	protected def getBSCB : BinStoreCmdBuilder

	private lazy val myBSCB = getBSCB
	private lazy val myGenBD = getGenBD
	private lazy val myGenTN = getGenTN

	def genAndStoreBaseLevelOnly(keyedCmdMaker: KeyedCmdMaker, massyMeatStrm : UStream[(BinMassInfo, BinMeatInfo)]):
										RIO[ZDynDBExec, BaseRsltPair] = {
		val baseGenOp: RIO[ZDynDBExec, (myGenTN.BinTagNumBlock, Chunk[BinStoreRslt])] = for {
			bntgnmBlk <- myGenTN.genBinTagNumBlock(rootTagNum, rootKidsCnt, baseBinLevel)
			_ <- ZIO.log(s"genAndStoreBaseSqnc .genBinTagNumBlock produced: ${bntgnmBlk.describe}")
			binSpecStrm <- ZIO.succeed(myGenBD.joinMassyMeatRows(bntgnmBlk.baseLevel, massyMeatStrm))
			binStoreCmdStrm <- ZIO.succeed(keyedCmdMaker.mkBaseLevCmds(binSpecStrm))
			levStoreRslt <- myBSCB.compileBinLevelStoreOp(binStoreCmdStrm)
			_ <- ZIO.log(s"Got levStoreRslt: ${levStoreRslt.toString().substring(0,200)} [TRUNCATED]")
		} yield(bntgnmBlk, levStoreRslt)
		baseGenOp
	}
}