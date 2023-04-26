package fun.gravax.zdynamo

import zio.dynamodb.{AttributeValue, DynamoDBError, Item, PrimaryKey, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.{dynamodb => ZDyn}
import scala.collection.immutable.{Map => SMap}

private trait BinItemStuff

trait KnowsBinItem {
	// Scenario ID/URI is our partition key.  But we might want to add more data to this key to scale computation
	// within a scenario.
	val FLDNM_SCEN = "scenario"

	// Sort-key is made out of timestamps and sequence-numbers
	// calc_obs_fut_seqNum
	//
	// We want to use some combination of these time fields as dynamo-db sort key.
	// Dynamo wants that sort key to be a single attribute, so if there is going to be concatenation we have to manage it.
	val FLDNM_TIME_OBS = "time-obs"		// Time of the last observation the prediction is based on, e.g. last price quote.
	val FLDNM_TIME_CALC = "time-calc"	// Time the prediction was calculated.
	val FLDNM_TIME_PRED = "time-pred"		// Time of the predicted future observation, e.g. asset return time horizon.
	val FLDNM_BINSEQ = "bin-seq"
	val FLDNM_PARENT_BINSEQ = "parent-bin-seq"

	val FLDNM_BIN_REL_WEIGHT = "bin-rel-weight" // What is our mass's fraction of the parent bin mass?
	val FLDNM_BIN_ABS_WEIGHT = "bin-abs-weight" // What is our mass's fraction of the root bin mass? (optional)
	val FLDNM_BIN_MASS = "bin-obs-mass" // How many items have been observed by this bin?  (optional).  Should be the sum of child masses, if any.

	val FLDNM_BIN_FLAVOR = "bin-flavor"	// Enum telling us what kind of value-map this bin holds
	val BFLV_ANN_RET_MEAN = "ANN_RET_MEAN"
	// val BFLV_

	val FLDNM_ANN_RET_MEAN = "ann-ret-mean"

	val KEYNM_SORT_BIN = "sort-key"		// Some string-concat of the above times.  Could be scenario specific!
	val FLDSEQ_SORT_BIN = List(FLDNM_TIME_OBS, FLDNM_TIME_PRED, FLDNM_TIME_CALC, FLDNM_BINSEQ)
	val binKeySchm = ZDyn.KeySchema(FLDNM_SCEN, KEYNM_SORT_BIN)  // partition-key, sort-key

	// It seems we only need AttributeDefinitions for attributes that are used in keys ... or indices?
	val scenAttr = ZDyn.AttributeDefinition.attrDefnString(FLDNM_SCEN)
	// Examples seen so far use Strings for date values.
	val sortKeyAttr = ZDyn.AttributeDefinition.attrDefnString(KEYNM_SORT_BIN)

}

trait FromBinItem extends FromItem with KnowsBinItem {
	def getPKfromFullBinItem(fullBI : Item) : PrimaryKey = {
		val partitionKeyVal = fetchOrThrow[String](fullBI, FLDNM_SCEN)
		val sortKeyVal = fetchOrThrow[String](fullBI, KEYNM_SORT_BIN)
		PrimaryKey(FLDNM_SCEN	-> partitionKeyVal,	KEYNM_SORT_BIN -> sortKeyVal)
	}
}


trait ToBinItem extends ToItem with KnowsBinItem {
	def mkBinItemSkel(scen : String, timeObs : String, timePred : String, timeCalc : String) : Item = {
		Item(
			FLDNM_SCEN 		-> 	scen,
			FLDNM_TIME_OBS 	->	timeObs,
			FLDNM_TIME_PRED -> 	timePred,
			FLDNM_TIME_CALC -> 	timeCalc
		)
	}
	def fleshOutBinItem(binWithSceneAndTimes : Item, binSeqNum : String, parentBinSeqNum : String,
						binRelWeight : BigDecimal, binAbsWeight : BigDecimal, binMass : BigDecimal,
						binFlavor : String, annRetMeans : SMap[String, BigDecimal]): Item = {
		val addMap = Map[String, AttributeValue](
			FLDNM_BINSEQ -> AttributeValue(binSeqNum),
			FLDNM_PARENT_BINSEQ -> AttributeValue(parentBinSeqNum),
			FLDNM_BIN_REL_WEIGHT -> AttributeValue(binRelWeight),
			FLDNM_BIN_ABS_WEIGHT -> AttributeValue(binAbsWeight),
			FLDNM_BIN_MASS -> AttributeValue(binMass),
			FLDNM_BIN_FLAVOR -> AttributeValue(binFlavor),
			FLDNM_ANN_RET_MEAN -> AttributeValue(annRetMeans)
		)
		val comboMap = binWithSceneAndTimes.map ++ addMap
		val comboItem = Item(comboMap)
		comboItem
	}
	def fillBinSortKey(partBinItem : Item) : Item = fillSortKey(partBinItem, KEYNM_SORT_BIN, FLDSEQ_SORT_BIN, "#")

}