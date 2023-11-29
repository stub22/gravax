package fun.gravax.dbin.tapend

import cats.instances.list.*
import cats.syntax.all.*
import fun.gravax.dbin.model.DbinModelTypes.{BinRelWt, EntryKey, EntryMean, EntryVariance}
import fun.gravax.dbin.model.PortfolioTypes.AssetWeight
import fun.gravax.dbin.model.{Bin, BinCovarOps, BinEntry, PortfolioOps}
import munit.FunSuite

class MathSuite extends FunSuite {
	test("upper-triangular") {
		val x = Vector(1, 2, 3, 4)
		val xt = x.tails.toVector
		println(s"xt=[${xt}]")
		// xts=[Vector(1, 2, 3, 4), Vector(2, 3, 4), Vector(3, 4), Vector(4), Vector()]
		val xtd = xt.dropRight(1)
		println(s"xtd=[${xtd}]")
		// xtds=[Vector(1, 2, 3, 4), Vector(2, 3, 4), Vector(3, 4), Vector(4)]
		val pairs = xtd.flatMap(subv => {
			val hd = subv.head
			subv.map((hd, _))
		})
		println(s"pairs=[${pairs}]")

	}

	test("cats-cartesian") {
		val x = Vector(1, 2, 3, 4)
		val xx: Seq[(Int, Int)] = (x,x).mapN((_, _))
		val xxs = xx.mkString(", ")
		println(s"xxs=[${xxs}]")
		// xxs=[(1,1), (1,2), (1,3), (1,4), (2,1), (2,2), (2,3), (2,4), (3,1), (3,2), (3,3), (3,4), (4,1), (4,2), (4,3), (4,4)]
	}

	val (symG, symA, symM, symS) = ("GOOG", "AAPL", "MSFT", "SPY")

	test("pvar") {
		val pwts = Map(symG -> bd("0.2"), symA -> bd("0.15"), symM -> bd("0.22"), symS -> bd("0.43"))
		val pops = new PortfolioOps {}
		val bcops = new BinCovarOps {}
		val means01 = Map(symG -> bd("0.1"), symA -> bd("0.2"), symM -> bd("0.3"), symS -> bd("0.4"))

		// val entsG = mkEntry(symG, "1)
	}
	def bd(numTxt : String) = BigDecimal(numTxt)
	def mkEntry(sym : EntryKey, mean : EntryMean, vrnc : EntryVariance) : BinEntry = BinEntry(sym, mean, vrnc)
	def mkEntry(sym : EntryKey, meanTxt : String, vrncTxt : String): BinEntry = mkEntry(sym, bd(meanTxt), bd(vrncTxt))
}
