package fun.gravax.zaxlam.plain

trait SerialDataExamples

import scala.beans.BeanProperty

/*
https://edward-huang.com/aws/cloud/2019/11/28/how-to-setup-aws-lambda-in-scala-without-any-external-library/
The JSON deserialization uses Java Beans, which requires a default constructor with a getter and a setter.
However, Case Class doesn’t have a default constructor – the only way to mimic Java Beans is to create a mutable
class that forces its variable to be passed around to multiple functions.

Therefore, you need to translate the above Scala Code to this:
class Request(@BeanProperty var firstName: String, @BeanProperty var lastName:String) {
    def this() = this("", "", "")

Luckily, AWS gives us another option of implementing the handler function with InputStream and OutputStream.
In this case, we need to handle the serialization and deserialization ourselves.
}
 */
class InboundDat(@BeanProperty var txtDat01: String, @BeanProperty var numDat01: Int, @BeanProperty var txtDat02: String) {
	def this() = this("", -999, "")

	override def toString: String = s"InboundDat[txtDat01=${txtDat01}, numDat01=${numDat01}, txtDat02=${txtDat02}]"
}



trait ZaxInObj {
}

trait ZaxResultObj extends AnyRef
case class DeeperResult(textField : String, numField : Int) extends ZaxResultObj