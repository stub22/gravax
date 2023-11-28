package fun.gravax.dbin.model

import fun.gravax.dbin.model.DbinModelTypes._

object DbinModelTypes {
	type EntryKey = String
	type EntryMean = BigDecimal
	type EntryVariance = BigDecimal
	type EntryExpectedSquare = BigDecimal

	type BinTag = String
	type BinFlavor = String	// TODO: Let's make a Scala 3 enum!
	type BinRelWt = BigDecimal // Relative weight of a sub-bin within a parent.
	type BinAbsWt = BigDecimal // Absolute weight of a bin within some root, which is the product of the relWeights on the bin's path from root.

	type DistScenario = String
	type DistTime = String
}

case class BinEntry(key : EntryKey, mean : EntryMean, variance : EntryVariance)

// The Map fields help indicate that there can be only one item at each key/tag.

// In the spirit of combinability, each bin is a model which may be referenced by multiple parent bins.
// The Bin doesn't know its own weight in any parent scheme.
// But it does know the weights of its own subBins, which must sum to 1.0 when present.
// This map of tags to weights gives us the identifying info we need to find/query sub-bins when desired
// (e.g. in order to compute covariances), without bloating the Bin to be too large.  This allows us to
// stream a sequence of Bins

case class Bin(tag: BinTag, parentTag : Option[BinTag],
			   entries : Map[EntryKey, BinEntry],
			   subWeights : Map[BinTag, BinRelWt])

case class DistribKeys(scenarioID : DistScenario, obsTime : DistTime, calcTime : DistTime, predTime : DistTime)

case class DistribRoot(keys : DistribKeys, rootBin : Bin)
// .getChildren   .getDescendants(numLevels)

object TestBins {

	val entKeys = Seq("AAPL", "MSFT", "GOOG", "SPY")
	def genEntry(key : EntryKey) : BinEntry = BinEntry(key, BigDecimal("1.0"), BigDecimal("0.5"))
	def genEntries: Map[EntryKey, BinEntry] = entKeys.map(ekey => genEntry(ekey)).map(ent => ent.key -> ent).toMap

	val bin01 = Bin("btag01", None, genEntries, Map("btag02" -> BigDecimal("0.3"), "btag03" -> BigDecimal("0.7")))
	val bin02 = Bin("btag02", None, genEntries, Map())
	val bin03 = Bin("btag03", None, genEntries, Map())
	val bins = Seq(bin01, bin02, bin03)
}

/*
[
{
	"tag": "btag01",
	"parentTag": null,
	"entries": {
	"AAPL": {
		"key": "AAPL",
		"mean": 1,
		"variance": 0.5
	},
	"MSFT": {
		"key": "MSFT",
		"mean": 1,
		"variance": 0.5
	},
	"GOOG": {
		"key": "GOOG",
		"mean": 1,
		"variance": 0.5
	},
	"SPY": {
		"key": "SPY",
		"mean": 1,
		"variance": 0.5
	}
	},
	"subWeights": {
	"btag02": 0.3,
	"btag03": 0.7
	}
},
{
	"tag": "btag02",
	"parentTag": null,
	"entries": {
	"AAPL": {
		"key": "AAPL",
		"mean": 1,
		"variance": 0.5
	},
	"MSFT": {
		"key": "MSFT",
		"mean": 1,
		"variance": 0.5
	},
	"GOOG": {
		"key": "GOOG",
		"mean": 1,
		"variance": 0.5
	},
	"SPY": {
		"key": "SPY",
		"mean": 1,
		"variance": 0.5
	}
	},
	"subWeights": {}
},
{
	"tag": "btag03",
	"parentTag": null,
	"entries": {
	"AAPL": {
		"key": "AAPL",
		"mean": 1,
		"variance": 0.5
	},
	"MSFT": {
		"key": "MSFT",
		"mean": 1,
		"variance": 0.5
	},
	"GOOG": {
		"key": "GOOG",
		"mean": 1,
		"variance": 0.5
	},
	"SPY": {
		"key": "SPY",
		"mean": 1,
		"variance": 0.5
	}
	},
	"subWeights": {}
}
]
*/