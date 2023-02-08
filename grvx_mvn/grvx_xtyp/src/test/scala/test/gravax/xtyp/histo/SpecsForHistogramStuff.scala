package test.gravax.xtyp.histo

import fun.gravax.xtyp.catty.{HGImpl, HistoBinEasy}
import org.scalatest.Tag
import org.scalatest.flatspec.AnyFlatSpec

private trait SpecsForHistogramStuff

// FIXME:  Copied this tag from grvx_aws
object Tag_NoBatch extends Tag("test.gravax.NoBatch")

class FirstHistoSpec extends AnyFlatSpec {

	"A HistoGram factory" should "make some histograms" taggedAs (Tag_NoBatch) in {
		// Using Strings as BinKeys
		val binA01 = new HistoBinEasy[String]("apple", 3)
		val binA02 = new HistoBinEasy[String]("apple", 7)
		println(s"Made binA01=${binA01}, binA02=${binA02}")

		val combinedCount = binA01.combineCounts(binA01.count, binA02.count)
		println(s"Combined count: ${combinedCount}")

		val lameHG = new HGImpl(List(binA01, binA02))

	}
}

