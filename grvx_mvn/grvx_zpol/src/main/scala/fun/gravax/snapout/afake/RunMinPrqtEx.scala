package fun.gravax.snapout.afake

import com.github.mjakubowski84.parquet4s.{ParquetIterable, ParquetReader, ParquetWriter, Path}

import java.nio.file.Files
import scala.util.Random

// HADOOP_HOME=D:\_dmnt\axio_gclnz\axio_ide_03\adaxmvn\axmgc_dmo_bgdt\hdp_win_292
object RunMinPrqtEx extends App {

	val flg_doWrites = false

	case class TrifleRec(id: Int, text: String)

	val count = 100
	val data01  = (1 to count).map(i => TrifleRec(id = i, text = "one_" + Random.nextString(4)))
	val data02  = (1 to count).map(i => TrifleRec(id = i, text = "two_" + Random.nextString(4)))
	val folderNm = "tmpdat/prqt_min_ex"
	val fileNm01 = "mpe_01.parquet"
	val fileNm02 = "mpe_02.parquet"
	//  Relative path in absolute URI: file:C:%5CUsers%5Cstub%5CAppData%5CLocal%5CTemp%5Cmin_prqt_ex2907342757329609633
	// val brokePath  = Path(Files.createTempDirectory(folderNm))
	val goodFolderPath = Path(folderNm)
	val folderPathToUse = goodFolderPath

	if (flg_doWrites) {
		// write
		ParquetWriter.of[TrifleRec].writeAndClose(folderPathToUse.append(fileNm01), data01)
		ParquetWriter.of[TrifleRec].writeAndClose(folderPathToUse.append(fileNm02), data02)
	}
	// read
	val readData: ParquetIterable[TrifleRec] = ParquetReader.as[TrifleRec].read(folderPathToUse)
	try readData.foreach(row => println(s"RunMinPrqtEx.init got TrifleRec: ${row}"))
	finally readData.close()

	val brsa = new BigRecStoreApi {}
	if (flg_doWrites) {
		brsa.doWrite
	}
	brsa.doRead
}

// parquet4s happens to pull in Shapeless
// import shapeless.ops.nat.LT.<
// Shapeless magic looks kinda like this...
// type LowerThan10[V <: Int] = V < 10