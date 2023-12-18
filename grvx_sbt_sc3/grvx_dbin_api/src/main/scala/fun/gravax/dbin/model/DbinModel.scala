package fun.gravax.dbin.model

import cats.effect.IO
import fun.gravax.dbin.model.DbinModelTypes.*

object DbinModelTypes extends EntryStatTypes {
	override type EntryKey = String
	override type EntryMean = BigDecimal
	override type EntryVariance = BigDecimal
	override type EntryExpectedSquare = BigDecimal

	type BinTag = String
	type BinFlavor = String	// TODO: Let's make a Scala 3 enum!
	type BinRelWt = BigDecimal // Relative weight of a sub-bin within a direct parent.  Weight of all siblings must sum to 1.0.
	type BinAbsMass = BigDecimal
	// Absolute mass of a bin within some root, which is the product of the relWeights on the bin's path from root.
    // AbsMass of all root's leaf descendants must sum to 1.0.   For direct children of root, AbsMass == RelWt.

	type DistScenario = String
	type DistTime = String
}

case class BinEntry(key : EntryKey, mean : EntryMean, variance : EntryVariance)

// The Map fields help indicate that there can be only one item at each key/tag.

// In the spirit of combinability, each bin is a model which may be referenced by multiple parent bins.
// The Bin doesn't know its own weight in any parent scheme.
// But it does know the weights of its own subBins (normalSubWeights), which must sum to 1.0 when present.
// This map of tags to weights gives us the identifying info we need to find/query sub-bins when desired
// (e.g. in order to compute covariances), without bloating the Bin to be too large.  This allows us to
// stream a sequence of Bins

// Bin at a tag is immutable.  We may think of bins as a function (distKeys, tag) => (entries, subWeights)
// The normalSubWeights must sum to exactly 1.0 for the direct children of the bin.
case class Bin(tag: BinTag, parentTag : Option[BinTag], entries : Map[EntryKey, BinEntry],
			   normalSubWeights : Map[BinTag, BinRelWt])

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