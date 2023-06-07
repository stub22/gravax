package fun.gravax.zaxlam.plain

import fun.gravax.distrib.gen.DbConnParams
import fun.gravax.zaxlam.srvlss.UnsafeTaskRunner
import fun.gravax.zaxlam.xform.{ZaxTypes, ZaxlamMapReadHelper}

import java.lang.{Boolean => JBool, Double => JDoub, Integer => JInt}
import scala.collection.immutable.{Map => SMap}

private trait CommandInterpStuff


trait KnowsHappyVocab {
	val MAPKEY_HTTP_BODY = "body"

	val MAPKEY_HAPPY_CMD = "command"
	val HCMD_QUERY_BIN_DATA  = "QRY_BIN_DAT"
	val MAPKEY_QRY_FLG = "qryFlg"
	val myDfltQryFlg = true

	val MAPKEY_ERR_TXT = "errTxt"
	val MAPKEY_RESULT_DAT = "rsltDat"
	val MAPKEY_DBCMD_OUT = "dbCmdOut"
}

// TODO:  Fix the in-out mapping for HttpApi
// TODO:  Fix the JMap <-> SMap nonsense

// Happy comes after Mappy, until we pick a better name.
class HappyZaxlam extends MappyZaxlam with ZaxTypes with KnowsHappyVocab {

	val myMRH = new ZaxlamMapReadHelper {}

	override def lambdaScala(inZSMap : ZaxSMap) : ZaxSMap = {
		val bodyOpt: Option[AnyRef] = inZSMap.get(MAPKEY_HTTP_BODY)
		println(s"HappyZaxlam.lambdaScala.println: bodyOpt=${bodyOpt}")
		if (bodyOpt.isDefined) {
			val bodyObj = bodyOpt.get
			println(s"bodyClass = ${bodyObj.getClass}, body.toString=${bodyObj.toString}")
			// odyClass = class java.lang.String, body.toString={"command" : "QRY_BIN_DAT"}
		}
		runHappyCommand(inZSMap)
	}
	def runHappyCommand(inZSMap : ZaxSMap) : ZaxSMap = {
		val cmdOpt = inZSMap.get(MAPKEY_HAPPY_CMD)
		val cmdOutSMap = cmdOpt.fold[ZaxSMap](errMapForNoCmdTxt)(cmdTxt => {
			cmdTxt match {
				case HCMD_QUERY_BIN_DATA => {
					val qryOpRslt = doQueryOp(inZSMap)
					println(s"doQueryOp result is: ${qryOpRslt}")
					val dbOpOutMap : ZaxSMap = qryOpRslt match {
						case Left(err) => errToMap(err)
						case Right(winMap) => winMap
					}
					dbOpOutMap
				}
				case other => {
					val errTxt = s"HappyZaxlam.lambdaScala did not find a match for cmdTxt: ${cmdTxt}"
					val err = (cmdOpt, errTxt)
					println (errTxt)
					errToMap(err)
				}
			}
		})
		val cmdOutJMap =  myMapXformer.deepConvertSMapToJMap(cmdOutSMap)
		val echoJMap = myMapXformer.deepConvertSMapToJMap(inZSMap)

		val happyOutSMap = SMap[String, AnyRef](MAPKEY_ECHO_MAP -> echoJMap, MAPKEY_DBCMD_OUT -> cmdOutJMap) // , "ENV_JMAP" -> envJMap)
		happyOutSMap
	}
	protected def errMapForNoCmdTxt : ZaxSMap = {
		val errTxt = s"No cmdTxt found in input"
		val err = (None, errTxt)
		errToMap(err)
	}
	// Override this method to do a useful DB query
	protected def doQueryOp(inZSMap : ZaxSMap) : ZaxResult = fakeQueryOp(inZSMap)

	// This op is kinda useless because it doesn't produce a result.
	// We use it just as a unit test that "a DB op wired and ran", without any data correctness requirements.
	private def fakeQueryOp(inZSMap : ZaxSMap) : ZaxResult = {
		val qfEith: Either[ZaxErr, Option[ZaxFlag]] = myMRH.getFlag(inZSMap, MAPKEY_QRY_FLG)

		val dbOpRslt : ZaxResult = qfEith match {
			case Left(err) => Left(err)
			case Right(flgOpt) => {
				val flg = flgOpt.getOrElse(myDfltQryFlg)
				println(s"Resolved query-local flag-opt ${flgOpt} to ${flg}")
				val dbRslt = runQuietFakeDbOperation(flg)
				dbRslt
			}
		}
		dbOpRslt
	}
	private lazy val myDbAdapter = new ZaxlamDbAdapter {}
	private def runQuietFakeDbOperation(qflg : Boolean) : ZaxResult = {
		val noRsltAvail: Unit = myDbAdapter.launchQuietDbTask() // Defaults to local DB access at host.docker.internal:8000
		val scaryNum = -700 : JInt
		val fakeRMap = SMap[String,AnyRef](MAPKEY_RESULT_DAT -> scaryNum)
		Right(fakeRMap)
	}
	protected def errToMap(err : ZaxErr) : ZaxSMap = {
		SMap(MAPKEY_ERR_TXT -> err._2)
	}
}
trait ZaxlamDbAdapter {
	import fun.gravax.distrib.gen.DistribGenStoreLoadTrial
	// Default from a lambda is to connect locally, and assume docker is running (because that's how we launch lambdas)
	private val myDefaultConnParams = DbConnParams(true, true)

	lazy private val myUnsafeRunner = new UnsafeTaskRunner {}

	def launchQuietDbTask(dbConnParams_opt: Option[DbConnParams] = None) : Unit = { // quiet => no result
		val connParams = dbConnParams_opt.getOrElse(myDefaultConnParams)
		val taskMaker = new DistribGenStoreLoadTrial(Some(connParams))
		val quietDbTask = taskMaker.mkQuietDbTask
		myUnsafeRunner.doRunUnitTaskNow(quietDbTask)
	}
}
// TODO:
//  Deploy to AWS
//  invoke on AWS from command line, and from our Scala LambdaClient, also using AWS web console.
//  Access dynamo-READ local + in-cloud, tested with 'sam local invoke'
//  deploy with dynamo-READ features to AWS, test dynamo-READ access there.
//  try ZIO-log (under local, then AWS)
//  try ZIO-stream (under local, then AWS)
//  try zio-stream parallel (under local, then AWS)

// TODO: Decode input commands using something like routes, noting that a generalized lambda-handler is much like
// an http service.
//  Produce helpful failures as data we can return to the client.

trait Match
case class MatchTextExact(keyName : String, exactTextVal : String) extends Match

trait Matchers

/*

https://github.com/aws-samples/aws-lambda-java-workshop/blob/main/labs/unicorn-location-api/UnicornLocationFunction/src/main/java/com/unicorn/location/UnicornPostLocationHandler.java

import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyRequestEvent;
import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyResponseEvent;

public class UnicornPostLocationHandler implements RequestHandler<APIGatewayProxyRequestEvent, APIGatewayProxyResponseEvent> {

    public APIGatewayProxyResponseEvent handleRequest(final APIGatewayProxyRequestEvent input, final Context context) {

        return new APIGatewayProxyResponseEvent()
                .withStatusCode(200)
                .withBody("This is supposed to be the Unicorn Location API at some point!");

    }
}

https://github.com/aws/aws-lambda-java-libs/blob/main/aws-lambda-java-events/src/main/java/com/amazonaws/services/lambda/runtime/events/APIGatewayProxyRequestEvent.java


// let requestJSON = JSON.parse(event.body);

 */