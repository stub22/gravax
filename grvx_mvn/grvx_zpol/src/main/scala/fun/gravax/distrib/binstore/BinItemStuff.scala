package fun.gravax.distrib.binstore

import fun.gravax.distrib.gen.{FromItem, KnowsBinTupTupTypes, ToItem}
import fun.gravax.distrib.struct.{BinData, BinMassInfo, BinMeatInfo, BinTagInfo, BinTimeInfo, BinTypes, EzBinData}
import zio.dynamodb.{AttributeValue, Item, PrimaryKey}
import zio.{Chunk, dynamodb => ZDyn}

import scala.collection.immutable.{Map => SMap}

private trait BinItemStuff

trait KnowsBinItem {
	// Scenario ID/URI is our partition key.  But we might want to add more data to this key to scale computation
	// within a scenario.
	val KEYNM_PART_SCENARIO = "scenKey"
	val KEYNM_SORT_COMPOUND = "sortKey"

	// Sort-key is made out of timestamps and sequence-numbers
	// calc_obs_fut_seqNum
	//
	// We want to use some combination of these time fields as dynamo-db sort key.
	// Dynamo wants that sort key to be a single attribute, so if there is going to be concatenation we have to manage it.
	val FLDNM_TIME_OBS = "timeObs"		// Time of the last observation the prediction is based on, e.g. last price quote.
	val FLDNM_TIME_CALC = "timeCalc"	// Time the prediction was calculated.
	val FLDNM_TIME_PRED = "timePred"		// Time of the predicted future observation, e.g. asset return time horizon.
	val FLDNM_BIN_TAG = "binTag"
	val FLDNM_PARENT_TAG = "parentBinTag"

	val FLDNM_BIN_REL_WEIGHT = "binRelWt" // What is our mass's fraction of the parent bin mass?
	val FLDNM_BIN_ABS_WEIGHT = "binAbsWt" // What is our mass's fraction of the root bin mass? (optional)
	val FLDNM_BIN_MASS = "binObsMass" // How many items have been observed by this bin?  (optional).  Should be the sum of child masses, if any.

	val FLDNM_BIN_FLAVOR = "binFlavor"	// Enum telling us what kind of value-map this bin holds
	val BFLV_ANN_RET_MEAN_VAR = "ANN_RET_MEAN_VAR" // FLAVOR for annual return x (mean, marginal variance)
	// val BFLV_

	// val FLDNM_ANN_RET_MEAN = "ann-ret-mean"

	val FLDNM_BROKED_MEAT_MAP = "broked-map-of-lists"
	val FLDNM_STRINGY_MEAT_MAP = "stringy-myMeatInf-map"
	val FLDNM_DOBLE_MAP = "meatMapOfMap" // Double-map structure containing the data of type ${bin-flavor}.
	val SUBFLDNM_MEAN = "mean"
	val SUBFLDNM_VAR = "var"

	// Some string-concat of the above times and tags.  Could possibly be scenario specific.
	// Currently we expect the distribution to be located using 3 concatenated datetime strings.
	// Within the distribution the bin is located at FLDNM_BIN_TAG, which we assume is a sufficient
	// ordering WITHIN a given distribution.  We don't need the parent tag in the sort key, because the
	// mechanism writing the bins is assumed to be orderly enough.  We generally expect to see the bin tags
	// reflect a breadth first traversal of the bin tree levels: (root, kids, gkids, ggkids, ...).
	val FLDSEQ_SORT_BIN = List(FLDNM_TIME_OBS, FLDNM_TIME_PRED, FLDNM_TIME_CALC, FLDNM_BIN_TAG)
	val binKeySchm = ZDyn.KeySchema(KEYNM_PART_SCENARIO, KEYNM_SORT_COMPOUND)  // partition-key, sort-key

	// It seems we only need AttributeDefinitions for attributes that are used in keys ... or indices?
	val scenAttr = ZDyn.AttributeDefinition.attrDefnString(KEYNM_PART_SCENARIO)
	// Examples seen so far use Strings for date values.
	val sortKeyAttr = ZDyn.AttributeDefinition.attrDefnString(KEYNM_SORT_COMPOUND)

}

trait FromBinItem extends FromItem with KnowsBinItem with KnowsBinTupTupTypes {
	def getPKfromFullBinItem(fullBI : Item) : PrimaryKey = {
		val partitionKeyVal = extractSceneID(fullBI)
		val sortKeyVal = fetchOrThrow[String](fullBI, KEYNM_SORT_COMPOUND)
		PrimaryKey(KEYNM_PART_SCENARIO	-> partitionKeyVal,	KEYNM_SORT_COMPOUND -> sortKeyVal)
	}
	def extractBinData(itm : Item) : BinData = {
		val sceneID = extractSceneID(itm)
		val timeData = extractTimeInfo(itm)
		val seqInfo = extractSeqInfo(itm)
		val massInfo = extractMassInfo(itm)
		val meatInfo = extractMeat(itm)
		val binDat = EzBinData(sceneID, timeData, seqInfo, massInfo, meatInfo)
		binDat
	}
	def extractBinScalars(itm : Item) : BinScalarInfoTup = {
		val timeData = extractTimeInfo(itm)
		val seqInfo = extractSeqInfo(itm)
		val massInfo = extractMassInfo(itm)
		(timeData, seqInfo, massInfo)
	}

	def extractSceneID (itm : Item) : String = fetchOrThrow[String](itm, KEYNM_PART_SCENARIO)

	def extractTimeInfo(itm : Item) : BinTimeInfo = {
		val timeObs = fetchOrThrow[String](itm, FLDNM_TIME_OBS)
		val timePred = fetchOrThrow[String](itm, FLDNM_TIME_PRED)
		val timeCalc = fetchOrThrow[String](itm, FLDNM_TIME_CALC)
		val timeInfo = BinTimeInfo(timeObs, timePred, timeCalc)
		timeInfo
	}

	def extractSeqInfo(itm: Item) : BinTagInfo = {
		val binSeqNum =  fetchOrThrow[String](itm, FLDNM_BIN_TAG)
		val parentSeqNum =  fetchOrThrow[String](itm, FLDNM_PARENT_TAG)
		val binFlavor = fetchOrThrow[String](itm, FLDNM_BIN_FLAVOR)
		val seqInfo = BinTagInfo(binSeqNum, parentSeqNum, binFlavor)
		seqInfo
	}

	def extractMassInfo(itm: Item) : BinMassInfo = {
		val binMass = fetchOrThrow[BigDecimal](itm, FLDNM_BIN_MASS)
		val relWt_opt = fetchOptional[BigDecimal](itm, FLDNM_BIN_REL_WEIGHT)
		val absWt_opt = fetchOptional[BigDecimal](itm,	FLDNM_BIN_ABS_WEIGHT)
		val massInfo = BinMassInfo(binMass, relWt_opt, absWt_opt)
		massInfo
	}

	def extractMeat(itm: Item) : BinMeatInfo = {

		println(s"extractMeat.println: butchering item: ${itm}")
		// Empirically thru IDE found this apparent way to extract a Map of Lists that is compliant with FromAttributeValue
		//	val meatMap: SMap[String, Iterable[BigDecimal]] = fetchOrThrow[SMap[String,Iterable[BigDecimal]]](itm, FLDNM_MEAT_MAP)
		//	println(s"extractMeat got meatMap: ${meatMap}")
		// BUT it doesn't quite work. Probly is fixable. See DummyItemMaker.mkDummyBinItem and BinStoreApi.readThatDummyBinYo
		// OTOH dobleMap
		// works great to extract a Map of Maps in one clean step.
		val dobleMap = fetchOrThrow[SMap[String,SMap[String,BigDecimal]]](itm, FLDNM_DOBLE_MAP)
		println(s"extractDoble.println: reading dobleMap: ${dobleMap}")
		// Now we just need to interpret the inner-maps.
		val mapOfStatMaps: SMap[String, BinTypes.StatEntry] = dobleMap.map(kvTup => {
			val ekey = kvTup._1
			val innerStatMap = kvTup._2
			val semap = innerMapToStatEntry(ekey, innerStatMap)
			(ekey, semap)
		})
		BinMeatInfo(mapOfStatMaps)
	}
	// Will throw (in the tuple-building .get calls) if either stat (mean or var) is missing.  Ignores any other stats.
	def innerMapToStatEntry(ekey : String, inMap : SMap[String,BigDecimal]) :  BinTypes.StatEntry = {
		// (BinTypes.EntryKey, BinTypes.EntryMean, BinTypes.EntryVar)
		val (meanOpt, vrOpt) = (inMap.get(SUBFLDNM_MEAN), inMap.get(SUBFLDNM_VAR))
		(ekey, meanOpt.get, vrOpt.get) // Naive tuple form of the StatEntry
	}
}


trait ToBinItem extends ToItem with KnowsBinItem {
	override type FIType = FromBinItem
	def getFBI : FromBinItem = myFromItem
	def mkBinItemSkel(scen : String, timeInfo : BinTimeInfo) : Item = mkBinItemSkel(scen, timeInfo.obsTime, timeInfo.predTime, timeInfo.calcTime)
	def mkBinItemSkel(scen : String, timeObs : String, timePred : String, timeCalc : String) : Item = {
		Item(
			KEYNM_PART_SCENARIO -> 	scen,
			FLDNM_TIME_OBS 	->	timeObs,
			FLDNM_TIME_PRED -> 	timePred,
			FLDNM_TIME_CALC -> 	timeCalc
		)
	}
	def combineMapWithItem(partialItem : Item, addMap : SMap[String, AttributeValue]) : Item = {
		val comboMap = partialItem.map ++ addMap
		val comboItem = Item(comboMap)
		comboItem
	}
	def fleshOutBinItem(binWithSceneAndTimes : Item, binSeqNum : String, parentBinSeqNum : String,
						binMass : BigDecimal, binRelWeight : BigDecimal): Item = {   // binAbsWeight : BigDecimal
		val addMap = SMap[String, AttributeValue](
			FLDNM_BIN_TAG -> AttributeValue(binSeqNum),
			FLDNM_PARENT_TAG -> AttributeValue(parentBinSeqNum),
			FLDNM_BIN_MASS -> AttributeValue(binMass),
			FLDNM_BIN_REL_WEIGHT -> AttributeValue(binRelWeight)
			// FLDNM_BIN_ABS_WEIGHT -> AttributeValue(binAbsWeight),

		)
		val comboMap = binWithSceneAndTimes.map ++ addMap
		val comboItem = Item(comboMap)
		comboItem
	}
	// Each stat entry contains TWO bigDecimal values (currently), for MEAN and VARIANCE.
	// We could encode it as an inner Map (clearer, more robust) or List (more compact in storage, so far is broken)
	def statEntryToList(stent : BinTypes.StatEntry): List[BigDecimal] = List(stent._2, stent._3)
	def statEntryToMap(stent : BinTypes.StatEntry): Map[String, BigDecimal] = Map(SUBFLDNM_MEAN -> stent._2, SUBFLDNM_VAR -> stent._3)

	def addTagsToBinItem(partialBin : Item, tagInfo : BinTagInfo) : Item = {
		val addMap = SMap[String, AttributeValue](
			FLDNM_BIN_TAG -> AttributeValue(tagInfo.binTag),
			FLDNM_PARENT_TAG -> AttributeValue(tagInfo.parentTag),
			FLDNM_BIN_FLAVOR -> AttributeValue(tagInfo.binFlavor)
		)
		combineMapWithItem(partialBin, addMap)
	}

	def addMassInfoToBinItem(partialBin : Item, massInfo : BinMassInfo) : Item = {
		val massMap = Map[String, AttributeValue](FLDNM_BIN_MASS -> AttributeValue(massInfo.binMass))
		val relWtMap = massInfo.relWt_opt.fold[Map[String, AttributeValue]](Map())(relWt => Map(FLDNM_BIN_REL_WEIGHT -> AttributeValue(relWt)))
		val absWtMap = massInfo.absWt_opt.fold[Map[String, AttributeValue]](Map())(absWt => Map(FLDNM_BIN_ABS_WEIGHT -> AttributeValue(absWt)))
		val addMap :Map[String, AttributeValue] = massMap ++ relWtMap ++ absWtMap
		combineMapWithItem(partialBin, addMap)
	}

	private def addMassToBinItem(partialBin : Item, binMass : BigDecimal) : Item = {
		val addMap = Map[String, AttributeValue](
			FLDNM_BIN_MASS -> AttributeValue(binMass))
		combineMapWithItem(partialBin, addMap)
	}

	def addMeatToBinItem(partialBin : Item, meatInfo : BinMeatInfo) : Item = {
		val mmap: BinTypes.StatMap = meatInfo.meatMap
		val mmWithLists_opt : Option[SMap[BinTypes.EntryKey, List[BigDecimal]]] = if (false) {
			// {"QQQ":{"L":[{"N":"0.0772"},{"N":"0.0430"}]},"SPY":{"L":[{"N":"0.0613"},{"N":"0.0351"}]}}
			// On read, so far our Map-of-Lists based encoding comes out like Map(MSFT -> List(Chunk(Number(0.117),Number(0.023)))
			// and is interp as having a SET of BigDecimal involved.  Surely that is fixable but map-of-maps works
			// and is nicely general, so we are going with that for now.
			Some( mmap.map(kvPair => (kvPair._1, statEntryToList(kvPair._2))))
		} else None

		// Map of maps works fine and is encoded like
		// {"QQQ":{"M":{"mean":{"N":"0.0772"},"var":{"N":"0.0430"}}},"SPY":{"M":{"mean":{"N":"0.0613"},"var":{"N":"0.0351"}}}}
		val mmWithMaps: SMap[BinTypes.EntryKey, SMap[String, BigDecimal]] = mmap.map(kvPair => (kvPair._1, statEntryToMap(kvPair._2)))

		val addMap = Map[String, AttributeValue](
			// FLDNM_BIN_FLAVOR -> AttributeValue(meatInfo.binFlavor),
		//	FLDNM_MEAT_MAP -> AttributeValue(mmWithLists),
			FLDNM_DOBLE_MAP -> AttributeValue(mmWithMaps))

		combineMapWithItem(partialBin, addMap)
	}
	def fillBinSortKey(partBinItem : Item) : Item = fillSortKey(partBinItem, KEYNM_SORT_COMPOUND, FLDSEQ_SORT_BIN, "#")

	def buildBinItem(skelBinItem : Item, tagInfo: BinTagInfo,  massInfo : BinMassInfo, binMeat : BinMeatInfo) : Item = {
		val binItemWithTags = addTagsToBinItem(skelBinItem, tagInfo)
		val binItemWithMass = addMassInfoToBinItem(binItemWithTags, massInfo)
		val binItemWithMeat = addMeatToBinItem(binItemWithMass, binMeat)

		val fullBI = fillBinSortKey(binItemWithMeat)
		fullBI
	}
}


trait DummyItemMaker extends KnowsBinItem {

	protected val myFBI : FromBinItem = new FromBinItem {}
	protected val myTBI : ToBinItem = new ToBinItem{
		override protected val myFromItem = myFBI
	}

	val scn_messy = "messy_01"
	val scn_dummy = "dummy_01"
	val sort_AA = "20230423#20230421"
	val sort_BB = "20230423#20230421#bbb"
	val googSym = "GOOG"
	val msftSym = "MSFT"

	def mkMessyItem = {
		// Copied from ZDynamoDB example code.
		val bigItem: Item = Item(
			KEYNM_PART_SCENARIO	-> scn_messy,
			KEYNM_SORT_COMPOUND -> sort_AA,
			"id"          -> 0,
			"bin"       -> Chunk.fromArray("abC".getBytes),
			"binSet"    -> Set(Chunk.fromArray("aBc".getBytes)),
			"boolean"   -> true,
			"list"      -> List(1, 2, 3, 7, 8),
			"map"       -> SMap(
				"a" -> true,
				"b" -> false,
				"c" -> true
			),
			"num"       -> 5,
			"numSet"    -> Set(4, 3, 2, 1),
			"null"      -> null,
			"string"    -> "string",
			"stringSet" -> Set("a", "b", "c")
		)
		bigItem
	}
	def mkDummyBinItem = {
		// Looking via Workbench, we see that inside collection fields, dynamo often stores pairs of {type, txtVal},
		// where type is one of "N", "BOOL"...
		val partialBinItem : Item = Item(
			KEYNM_PART_SCENARIO	-> scn_dummy,
			FLDNM_TIME_OBS ->	"20221117_21:30",
			FLDNM_TIME_PRED -> "20231117_21:30",
			FLDNM_TIME_CALC -> "20221118_14:22",
			FLDNM_BIN_TAG -> "001",
			FLDNM_PARENT_TAG -> "-1",
			FLDNM_BIN_FLAVOR -> BFLV_ANN_RET_MEAN_VAR,  // Should this point directly to the value-map field?
			FLDNM_BIN_REL_WEIGHT -> BigDecimal("0.222"),
			FLDNM_BIN_ABS_WEIGHT -> BigDecimal("0.101"),
			FLDNM_BIN_MASS -> BigDecimal("255"),


			// Encodes as {"DMSFT":{"M":{"mean":{"N":"0.117"},"var":{"N":"0.023"}}},"DGOOG":{"M":{"mean":{"N":"0.093"},"var":{"N":"0.018"}}}}
			FLDNM_DOBLE_MAP	-> SMap(
				"D" + msftSym -> SMap(SUBFLDNM_MEAN -> BigDecimal("0.117"), SUBFLDNM_VAR -> BigDecimal("0.023")),
				"D" + googSym -> SMap(SUBFLDNM_MEAN -> BigDecimal("0.093"), SUBFLDNM_VAR -> BigDecimal("0.018"))
			),

			// BROKEN and unused so far:  Map of Lists comes out as List of Chunks => List of Sets or something?
			// Encodes as  {"MSFT":{"L":[{"N":"0.117"},{"N":"0.023"}]},"GOOG":{"L":[{"N":"0.093"},{"N":"0.018"}]}}
			FLDNM_BROKED_MEAT_MAP	-> SMap(
				msftSym -> List(BigDecimal("0.117"), BigDecimal("0.023")),
				googSym -> List(BigDecimal("0.093"), BigDecimal("0.018"))
			),

			// Has same decoding problem
			// {"MSFT":{"L":[{"S":"0.285"},{"S":"0.110"}]},"GOOG":{"L":[{"S":"0.412"},{"S":"0.194"}]}}
			// Fails on read 	Error getting string set value.
			// Expected AttributeValue.StringSet but found List(Chunk(String(0.285),String(0.110)))
			FLDNM_STRINGY_MEAT_MAP -> SMap(
					msftSym -> List("0.285", "0.110"),
					googSym -> List("0.412", "0.194")
			)

			// TODO:  Densify, maybe using Strings instead of decimals.
			// If we go in through Java API can we reduce the amount of wrappering in the stored values?
			//

		)
		val fullBinItem = myTBI.fillBinSortKey(partialBinItem)
		fullBinItem
	}
}