package fun.gravax.dbin.tapend

import munit.FunSuite
import fun.gravax.dbin.model.DbinModelTypes.{BinRelWt, EntryKey, EntryMean, EntryVariance}
import fun.gravax.dbin.model.PortfolioTypes.AssetWeight
import fun.gravax.dbin.model.{Bin, BinCovarOps, BinEntry, PortfolioOps}

class PortfolioSuite extends FunSuite {
	val (symG, symA, symM, symS) = ("GOOG", "AAPL", "MSFT", "SPY")

	test("pvar") {
		val pwts = Map(symG -> bd("0.2"), symA -> bd("0.15"), symM -> bd("0.22"), symS -> bd("0.43"))
		val pops = new PortfolioOps {}
		val bcops = new BinCovarOps {}
		val means01 = Map(symG -> bd("0.1"), symA -> bd("0.2"), symM -> bd("0.3"), symS -> bd("0.4"))

		// val entsG = mkEntry(symG, "1)
	}

	def bd(numTxt: String) = BigDecimal(numTxt)

	def mkEntry(sym: EntryKey, mean: EntryMean, vrnc: EntryVariance): BinEntry = BinEntry(sym, mean, vrnc)

	def mkEntry(sym: EntryKey, meanTxt: String, vrncTxt: String): BinEntry = mkEntry(sym, bd(meanTxt), bd(vrncTxt))

	// Each combo of Portfolio X Distrib will yield a portfolio mean + var, which are scalars.
	// It could also yield a bin structure.
	// AND/OR some measure of correlation of portfolio with various Entries that might be in or out of the portfolio.
	def evalManyPortfoliosOverOneDistrib = {
		???
	}

	// The ManyDistribs could be closely related or wildly different.
	// Our main constraint is that they must provide entries for all the assets in the portfolio.
	// The usual job of the distrib is to provide globalMeans + covarMatrix.
	// When we want to drill deeper into the bins, it would not usually be a "ManyDistribs" scenario, right?
	def evalOnePortfolioOverManyDistribs = {

		???
	}

	// Best may be defined using conventional portfolio theory, sharpe+sortino ratios.
	def findBestPortfolioForDistrib = ???
	def findBestDistribForPortfolio = ???

	def buildBinDistribFromUniformSpec = ???
	def buildBinDistribFromGaussianSpec = ???

}
