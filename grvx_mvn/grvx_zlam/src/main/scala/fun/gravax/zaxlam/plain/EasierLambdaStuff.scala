package fun.gravax.zaxlam.plain

import com.amazonaws.services.lambda.runtime.{ClientContext, CognitoIdentity, Context, LambdaLogger, RequestHandler}

import java.util.{List => JList, Map => JMap}
import scala.collection.immutable.{Map => SMap}
// import scala.collection.mutable
import scala.jdk.CollectionConverters._

private trait EasierLambdaStuff


trait HandlerMon {
	def dumpCtx(lamLog: LambdaLogger, ctx: Context) : Unit = {
		lamLog.log(s"HandlerMon.dumpCtx got ctx: ${ctx}\n")
		val ident: CognitoIdentity = ctx.getIdentity
		lamLog.log(s"HandlerMon.dumpCtx got cognito-identity: ${ident}\n")
		if (ident != null) {
			val identId = ident.getIdentityId
			val poolId = ident.getIdentityPoolId
			lamLog.log(s"HandlerMon.dumpCtx getIdentityId=${identId}, getIdentityPoolId=${poolId}\n")
			// lamLog
		}
		val cliCtx: ClientContext = ctx.getClientContext
		lamLog.log(s"HandlerMon.dumpCtx got client-context: ${cliCtx}\n")
		if (cliCtx != null) {
			val cliCli = cliCtx.getClient
			lamLog.log(s"HandlerMon.dumpCtx got cli-cli: ${cliCli}\n")
			val cliCust = cliCtx.getCustom
			lamLog.log(s"HandlerMon.dumpCtx got cli-custom: ${cliCust}\n")
			val cliEnv = cliCtx.getEnvironment
			lamLog.log(s"HandlerMon.dumpCtx got cli-env: ${cliCust}\n")
		}
		val rqID = ctx.getAwsRequestId
		val funcArn = ctx.getInvokedFunctionArn
		val funcName = ctx.getFunctionName
		val funcVers = ctx.getFunctionVersion
		lamLog.log(s"HandlerMon.dumpCtx got awsRqId=${rqID}, funcArn=${funcArn}, funcNm=${funcName}, funcVers=${funcVers}\n")
		val memLimit: Int = ctx.getMemoryLimitInMB
		val remainTime: Int = ctx.getRemainingTimeInMillis
		val logGrpNm = ctx.getLogGroupName
		val logStrmNm = ctx.getLogStreamName
		lamLog.log(s"HandlerMon.dumpCtx got memLimit=${memLimit}, remainTime=${remainTime}, logGrpNm=${logGrpNm}, logStrmNm=${logStrmNm}\n")
	}
	def dumpEnv(lamLog: LambdaLogger) : Unit = {
		// The env includes AWS secret keys so this is a security risk.
		val envJMap: JMap[String, String] = System.getenv()
		// lamLog.log(s"HandlerMon.dumpEnv got envJMap: ${envJMap}\n")
	}
	def dumpEntityWithTypeInfo(lamLog: LambdaLogger, entityName : String, entityData : Any) : Unit = {
		if (entityData == null) {
			lamLog.log(s"entity[${entityName}] has value NULL\n")
		} else {
			val dat = entityData.toString
			val clzInf = entityData.getClass.getName
			// lamLog.log(s"entity[${entityName}].toString=${dat}, .clzInf=${clzInf} \n")
			entityData match {
				case jmap: JMap[_, _] => {
					lamLog.log(s"Map entity[${entityName}]:\n"); dumpMap(lamLog, jmap)
				}
				case jlst: JList[_] => {
					lamLog.log(s"List entity[${entityName}]:\n"); dumpList(lamLog, jlst)
				}
				case jint: java.lang.Integer => {
					lamLog.log(s"Integer entity[${entityName}]: ${jint}\n")
				}
				case jdoub: java.lang.Double => {
					lamLog.log(s"Double entity[${entityName}]: ${jdoub}\n")
				}
				case jbool: java.lang.Boolean => {
					lamLog.log(s"Boolean entity[${entityName}]: ${jbool}\n")
				}
				case jstrng: java.lang.String => {
					lamLog.log(s"String entity[${entityName}]: ${jstrng}\n")
				}
				case other => {
					lamLog.log(s"entity[${entityName}] value has UNKNOWN TYPE=${other.getClass.getName} and value '${other}'\n")
				}
			}
		}
	}
	def dumpMap(lamLog: LambdaLogger, jMap : JMap[_, _]) : Unit = {
		val sMap = jMap.asScala.toMap
		lamLog.log(s"Naive map pre-dump: ${sMap}\n")
		sMap.foreach(kvPair => {
			val (entryKey, entryValue) = kvPair
			// lamLog.log(s"key class is ${entryKey.getClass.getName}, value class is ${entryValue.getClass.getName}\n")
			// lamLog.log(s"handleRq.inZSMap(${entryKey})=${entryValue}\n")
			if ((entryKey != null) && (entryValue != null)) {
				entryKey match {
					case ekeyStrng: java.lang.String => dumpEntityWithTypeInfo(lamLog, ekeyStrng, entryValue)
					case other => {
						lamLog.log(s"entryKey ${entryKey} is NOT a java.lang.String!\n")
						val ekeyTxt = entryKey.toString
						dumpEntityWithTypeInfo(lamLog, s"[ekeyNotString=${ekeyTxt}]", entryValue)
					}
				}
			}
		})
	}
	def dumpList(lamLog: LambdaLogger, jLst : JList[_]) : Unit = {
		val sList: List[Any] = jLst.asScala.toList
		lamLog.log(s"Dumping list: ${sList}\n")
		val lstWithIdx = sList.zipWithIndex
		lstWithIdx.foreach(pair => dumpEntityWithTypeInfo(lamLog, s"lstElem[${pair._2}]", pair._1))
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
		lamLog.log(s"AxLamHandler.handleRequest is naively dumping input as .toString: ${in}\n")
		myMon.dumpEntityWithTypeInfo(lamLog, "AxLamHandler.handleRequest.IN", in)
		val out = handleRq(in, context)
		lamLog.log(s"AxLamHandler.handleRequest is naively dumping output as .toString: ${out}\n")
		myMon.dumpEntityWithTypeInfo(lamLog, "AxLamHandler.handleRequest.OUT",  out)
		out
	}
}

class HelloZaxlam() extends AxLamHandler[JMap[String,String], String] {

	override protected def handleRq(in: JMap[String, String], ctx: Context): String = {
		val lamLog: LambdaLogger = ctx.getLogger
		val inSMap = in.asScala
		val inMapTxt = inSMap.mkString(";")
		"HELLO_ZAXLAM_RESULT_[" + inMapTxt + "]"
	}
}

class HowdyZaxlam() extends AxLamHandler[String, String] {
	override protected def handleRq(in: String, ctx: Context): String = "HowdyZaxlam_result_v008aaf"
}

class DeeperZaxlam() extends AxLamHandler[JMap[String,InboundDat], JMap[String,ZaxResultObj]] {
	override protected def handleRq(in: JMap[String,InboundDat], ctx: Context): JMap[String,ZaxResultObj] = {
		val rslt01 = DeeperResult("jeellaeu", 37)
		val rslt02 = DeeperResult("scoober", 14)
		val outSMap = SMap[String, ZaxResultObj]("firstOne" -> rslt01, "nextOne" -> rslt02)
		val outJMap : JMap[String,ZaxResultObj] = outSMap.asJava
		outJMap
	}
}


// Using AnyRef because AWS Lambda decoder will always pass Objects/boxed types.
class MappyZaxlam() extends AxLamHandler[JMap[String, AnyRef], JMap[String, AnyRef]] {
	override protected def handleRq(inJMap: JMap[String, AnyRef], ctx: Context): JMap[String, AnyRef] = {
		// DANGER: SysEnv may contain AWS keys and other info we don't want to accidentally log, print, return, store, ...
		// val envJMap: JMap[String, String] = System.getenv()
		// val envSMap: SMap[String, String] = envJMap.asScala.toMap

		// The output JSON serializer wants to see JAVA mutable maps and Lists
		val inSMap: SMap[String, AnyRef] = deepConvertJMapToSMap(inJMap)
		val outSMap = lambdaScala(inSMap)
		val outJMap: JMap[String, AnyRef] = deepConvertSMapToJMap(outSMap)
		outJMap
	}

	def deepConvertJMapToSMap(jMap : JMap[String, AnyRef]) : SMap[String, AnyRef] = {
		// FIXME: This step does not deep-convert any JMaps or JLists lurking in the values.
		val shallowSMap: SMap[String, AnyRef] = jMap.asScala.toMap
		shallowSMap
	}
	def deepConvertSMapToJMap(sMap : SMap[String, AnyRef]) : JMap[String, AnyRef] = {
		// FIXME: This step does not deep-convert any SMaps or SLists lurking in the values.
		val shallowJMap = sMap.asJava
		shallowJMap
	}
	val MAPKEY_ECHO_MAP = "ECHO_MAP"
	// Override this method to do something useful.
	protected def lambdaScala(inSMap : SMap[String, AnyRef]) : SMap[String, AnyRef] = {
		val echoedInputJMap = deepConvertSMapToJMap(inSMap)
		val dummyOutSMap = SMap[String, AnyRef](MAPKEY_ECHO_MAP -> echoedInputJMap) // , "ENV_JMAP" -> envJMap)
		dummyOutSMap
	}
}

/*

Dumping list: List(13.2, huzzah, 9)
someDat.toString=13.2, clzInf=java.lang.Double Not a map or list: 13.2
someDat.toString=huzzah, clzInf=java.lang.String Not a map or list: huzzah
someDat.toString=9, clzInf=java.lang.Integer Not a map or list: 9

someDat.toString=true, clzInf=java.lang.Boolean


someDat.toString={txtDat01=Ltest news, numDat01=69, txtDat02=neener NEANER}, clzInf=java.util.LinkedHashMapSubMap:
someDat.toString=[12, 18, 3], clzInf=java.util.ArrayListSubList:

someDat.toString=[13.2, huzzah, 9], clzInf=java.util.ArrayListSubList:

HandlerMon.dumpCtx got ctx: lambdainternal.api.LambdaContext@6ed3ef1

key class is java.lang.String
value class is java.util.Collections$UnmodifiableMap
handleRq.inZSMap(ENV_JMAP)


*/


/***
 *
 *
Lambda-Java framework decodes the incoming Json into a JMap of String -> Whatever,
where Whatever might be String, Integer, Double, Boolean, List, Map.
https://docs.aws.amazon.com/lambda/latest/dg/java-handler.html
The Lambda runtime receives an event as a JSON-formatted string and converts it into an object. It passes the event
object to your function handler along with a context object that provides details about the invocation and the function.
You tell the runtime which method to invoke by setting the handler parameter on your function's configuration.

Handler formats
package.Class::method – Full format. For example: example.Handler::handleRequest.
package.Class – Abbreviated format for functions that implement a handler interface. For example: example.Handler.


https://docs.aws.amazon.com/lambda/latest/dg/java-handler.html
Input types
Integer, Long, Double, etc. – The event is a number with no additional formatting—for example, 3.5. The runtime converts the value into an object of the specified type.

String – The event is a JSON string, including quotes—for example, "My string.". The runtime converts the value (without quotes) into a String object.

Type, Map<String,Type> etc. – The event is a JSON object. The runtime deserializes it into an object of the specified type or interface.

List<Integer>, List<String>, List<Object>, etc. – The event is a JSON array. The runtime deserializes it into an object of the specified type or interface.


You can add initialization code outside of your handler method to reuse resources across multiple invocations.
When the runtime loads your handler, it runs static code and the class constructor. Resources that are created
during initialization stay in memory between invocations, and can be reused by the handler thousands of times.

You specify the type of object that the event maps to in the handler method's signature. In the preceding example,
the Java runtime deserializes the event into a type that implements the Map<String,String> interface.
String-to-string maps work for flat events...

... the value of each field must be a string or number. If the event includes a field that has an object as a
value, the runtime can't deserialize it and returns an error.

InputStream – The event is any JSON type. The runtime passes a byte stream of the document to the handler without
modification. You deserialize the input and write output to an output stream.

Library type – For events sent by AWS services, use the types in the aws-lambda-java-events library.
https://github.com/aws/aws-lambda-java-libs/tree/main/aws-lambda-java-events/src/main/java/com/amazonaws/services/lambda/runtime/events

If you define your own input type, it should be a deserializable, mutable plain old Java object (POJO), with a default
constructor and properties for each field in the event. Keys in the event that don't map to a property as well as
properties that aren't included in the event are dropped without error.

The output type can be an object or void. The runtime serializes return values into text. If the output is an object
with fields, the runtime serializes it into a JSON document. If it's a type that wraps a primitive value, the runtime
returns a text representation of that value.
 */