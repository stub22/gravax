package fun.gravax.snapout.afake

import com.github.mjakubowski84.parquet4s.{ParquetReader, ParquetWriter, Path}


case class SubRec(title : String, weight : BigDecimal, fruits : List[String])

case class BigRec(nameo : String, subRecMap : Map[String, SubRec])

trait BigRecStoreApi {
	val sr01 = SubRec("dwayne", BigDecimal("22.6"), List("grape", "pineapple"))
	val sr02 = SubRec("sara", BigDecimal("19.8"), List("banana", "mango"))

	val br01 = BigRec("home", Map[String, SubRec]("one" -> sr01, "two" -> sr02))
	val br02 = BigRec("work", Map[String, SubRec]("elf" -> sr02, "dwarf" -> sr01))

	val brs = List(br01, br02)

	val folderNm = "prqt_nest_ex"
	val fileNm01 = "pne_01.parquet"
	val folderPathToUse = Path(folderNm)

	def doWrite : Unit = {
		ParquetWriter.of[BigRec].writeAndClose(folderPathToUse.append(fileNm01), brs)
	}
	def doRead : Unit = {
		val readData = ParquetReader.as[BigRec].read(folderPathToUse)
		try readData.foreach(println)
		finally readData.close()
	}
}
/*

Int	☑	☑
Long	☑	☑
Byte	☑	☑
Short	☑	☑
Boolean	☑	☑
Char	☑	☑
Float	☑	☑
Double	☑	☑
BigDecimal	☑	☑
java.time.LocalDateTime [*with INT96]	☑	☒
java.time.LocalDateTime [*with INT64]	☑	☑
java.time.LocalDate	☑	☑
java.sql.Timestamp [*with INT96]	☑	☒
java.sql.Timestamp [*with INT64]	☑	☑
java.sql.Date	☑	☑
Array[Byte]	☑	☑
Complex types can be arbitrarily nested.

Option
List
Seq
Vector
Set
Array - An array of bytes is treated as primitive binary
Map - Key must be of primitive type, only the immutable version.
Any Scala collection that has Scala collection Factory (in 2.12 it is derived from CanBuildFrom). Refers to both mutable and immutable collections. Collection must be bounded only by one type of element - because of that Map is supported only in the immutable version.
Any case class
 */
