package fun.gravax.zaxlam.plain

import com.amazonaws.services.lambda.runtime.{Context, LambdaLogger, RequestHandler}
import java.util.{Map => JMap, List => JList}
private trait EasierLambdaStuff


trait HandlerMon {
	def dumpCtx(lamLog: LambdaLogger, ctx: Context) : Unit = {
		lamLog.log(s"HandlerMon.dumpCtx got ctx: ${ctx}")
	}
	def dumpEnv(lamLog: LambdaLogger) : Unit = {
		val envJMap: JMap[String, String] = System.getenv()
		lamLog.log(s"HandlerMon.dumpEnv got envJMap: ${envJMap}")
	}
}
trait AxLamHandler[In,Out] extends RequestHandler[In,Out] {
	val myMon = new HandlerMon{}

	protected def handleRq(in : In, ctx: Context): Out
	override def handleRequest(in : In, context: Context): Out = {
		val lamLog: LambdaLogger = context.getLogger
		myMon.dumpEnv(lamLog)
		println("HelloZaxlam.handleRequest is printing to console?!")
		myMon.dumpCtx(lamLog, context)
		lamLog.log(s"AxLamHandler.handleRequest is naively dumping input: ${in}")
		val out = handleRq(in, context)
		lamLog.log(s"AxLamHandler.handleRequest is naively dumping output: ${out}")
		out
	}

}
import scala.jdk.CollectionConverters._

class HelloZaxlam() extends AxLamHandler[JMap[String,String], String] {
	override protected def handleRq(in: JMap[String, String], ctx: Context): String = "HELLO_ZAXLAM_RESULT_v007"
}

class HowdyZaxlam() extends AxLamHandler[String, String] {
	override protected def handleRq(in: String, ctx: Context): String = "HowdyZaxlam_result_v007"
}

class DeeperZaxlam() extends AxLamHandler[JMap[String,InboundDat], JMap[String,ZaxResultObj]] {
	override protected def handleRq(in: JMap[String,Object], ctx: Context): JMap[String,ZaxResultObj] = {
		val rslt01 = DeeperResult("jeellaeu", 37)
		val rslt02 = DeeperResult("scoober", 14)
		val outSMap = Map[String, ZaxResultObj]("firstOne" -> rslt01, "nextOne" -> rslt02)
		val outJMap : JMap[String,ZaxResultObj] = outSMap.asJava
		outJMap
	}
}

/***
 *
 *
https://docs.aws.amazon.com/lambda/latest/dg/java-handler.html
Input types
Integer, Long, Double, etc. – The event is a number with no additional formatting—for example, 3.5. The runtime converts the value into an object of the specified type.

String – The event is a JSON string, including quotes—for example, "My string.". The runtime converts the value (without quotes) into a String object.

Type, Map<String,Type> etc. – The event is a JSON object. The runtime deserializes it into an object of the specified type or interface.

List<Integer>, List<String>, List<Object>, etc. – The event is a JSON array. The runtime deserializes it into an object of the specified type or interface.

InputStream – The event is any JSON type. The runtime passes a byte stream of the document to the handler without modification. You deserialize the input and write output to an output stream.

Library type – For events sent by AWS services, use the types in the aws-lambda-java-events library.

https://github.com/aws/aws-lambda-java-libs/tree/main/aws-lambda-java-events/src/main/java/com/amazonaws/services/lambda/runtime/events

If you define your own input type, it should be a deserializable, mutable plain old Java object (POJO), with a default constructor and properties for each field in the event. Keys in the event that don't map to a property as well as properties that aren't included in the event are dropped without error.

The output type can be an object or void. The runtime serializes return values into text. If the output is an object with fields, the runtime serializes it into a JSON document. If it's a type that wraps a primitive value, the runtime returns a text representation of that value.
 *
 *
 */