package fun.gravax.zaxlam.plain

trait SerialDataExamples

import scala.beans.BeanProperty

class InboundDat(@BeanProperty var txtDat01: String, @BeanProperty var numDat01: Int, @BeanProperty var txtDat02: String) {
	def this() = this("", 14, "")
}



trait ZaxInObj {
}

trait ZaxResultObj extends AnyRef
case class DeeperResult(textField : String, numField : Int) extends ZaxResultObj