package fun.gravax.snapout.afake

import com.github.mjakubowski84.parquet4s.RowParquetRecord
import org.slf4j.{Logger, LoggerFactory}

// Note in IDEA environment variables we set:
// HADOOP_HOME=E:\_emnt\axio_git_clnz\agc_02\adaxmvn\axmgc_dmo_bgdt\hdp
// to use module's hdp/bin/winutils.exe

trait DataCompileRunner {
	private val rsrcPth_lmlExpWebJson = "/gdat/lean_mathlib/lml_exweb_20210521_sz196MB.json"
	private val rsrcPth_lmlExpStrctJson = "/gdat/lean_mathlib/lml_exstruct_20210529_sz91MB.json"

	def runCompile : Unit = {
		val prqtSrcScanner = new PrqtSourceScanner(rsrcPth_lmlExpWebJson, rsrcPth_lmlExpStrctJson)

		prqtSrcScanner.doScan()
	}
}

class PrqtSourceScanner(rsrcPth_lmlExpWebJson : String, rsrcPth_lmlExpStrctJson : String) {
	val myS4JLogger: Logger = LoggerFactory.getLogger(this.getClass)

	def doScan() : Unit = {
		logBar()
		myS4JLogger.info(s"Pretending to scan prqt file=${rsrcPth_lmlExpWebJson}, how bow da?")
		doGenericWriteThenRead
		logBar()
	}
	def doGenericWriteThenRead : Unit = {
		val datMkr = new MakesUserDat {}
		val udat: List[RowParquetRecord] = datMkr.mkUsrDat
		val rdrAndWrtr = new ReadsAndWritesPrqtWithOurSchema {}
		val dirPath ="prqextmp"
		val fname = "urecs_" + System.currentTimeMillis() + ".parquet"
		val filePath = s"$dirPath/$fname"

		rdrAndWrtr.writePrqtFile(udat, filePath)
		rdrAndWrtr.readPrqtFile(filePath)
	}
	private def logBar() : Unit = {
		myS4JLogger.info("================================================================")
	}

}