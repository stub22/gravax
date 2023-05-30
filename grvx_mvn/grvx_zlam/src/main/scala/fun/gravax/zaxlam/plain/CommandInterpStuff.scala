package fun.gravax.zaxlam.plain

import fun.gravax.distrib.gen.UnsafeTaskRunner

import java.util.{List => JList, Map => JMap}
import java.lang.{Boolean => JBool, Integer => JInt, Double => JDoub}
import scala.collection.immutable.{Map => SMap}


private trait CommandInterpStuff

trait ZaxTypes {
	type ZaxSMap = SMap[String, AnyRef]
	type ZaxFlag = Boolean
	type ZaxErr = (AnyRef,String)
	type ZaxResult=Either[ZaxErr,ZaxSMap]
}
trait ZaxlamMapReadHelper extends  ZaxTypes {

	// 3 possibilities:  a) good value b) no value c) bad value
	def getFlag(smap : ZaxSMap, key : String) : Either[ZaxErr, Option[ZaxFlag]] = {
		val flgOpt : Option[AnyRef] = smap.get(key)
		val eith = flgOpt match {
			case None => Right(None)
			case Some(jbool : JBool) => {
				val sbool = jbool.booleanValue()
				Right(Some(sbool))
			}
			case Some(other) => {
				val otherDesc = if (other != null) s"other[clz=${other.getClass.getName}]='${other.toString}'" else "NULL"
				Left(other, s"Expected ZaxFlag/Boolean at key=${key}, but got ${otherDesc}")
			}

		}
		eith
	}
}
trait KnowsHappyVocab {
	val MAPKEY_HAPPY_CMD = "command"
	val HCMD_QUERY_BIN_DATA  = "QRY_BIN_DAT"
	val MAPKEY_QRY_FLG = "qryFlg"
	val myDfltQryFlg = true

	val MAPKEY_ERR_TXT = "errTxt"
	val MAPKEY_RESULT_DAT = "rsltDat"
	val MAPKEY_DBCMD_OUT = "dbCmdOut"
}

// Happy comes after Mappy, until we pick a better name.
class HappyZaxlam extends MappyZaxlam with ZaxTypes with KnowsHappyVocab {


	val myMRH = new ZaxlamMapReadHelper {}

	override def lambdaScala(inZSMap : ZaxSMap) : ZaxSMap = {
		val cmdOpt = inZSMap.get(MAPKEY_HAPPY_CMD)
		val cmdOutSMap = cmdOpt.fold[ZaxSMap](errMapForNoCmdTxt)(cmdTxt => {
			cmdTxt match {
				case HCMD_QUERY_BIN_DATA => {
					val qryOpRslt = doQueryOp(inZSMap)
					println(s"Crazy dbQuery result is: ${qryOpRslt}")
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
		val cmdOutJMap = deepConvertSMapToJMap(cmdOutSMap)
		val echoJMap = deepConvertSMapToJMap(inZSMap)

		val dummyOutSMap = SMap[String, AnyRef](MAPKEY_ECHO_MAP -> echoJMap, MAPKEY_DBCMD_OUT -> cmdOutJMap) // , "ENV_JMAP" -> envJMap)
		dummyOutSMap
	}
	protected def errMapForNoCmdTxt : ZaxSMap = {
		val errTxt = s"No cmdTxt found in input"
		val err = (None, errTxt)
		errToMap(err)
	}
	protected def doQueryOp(inZSMap : ZaxSMap) : ZaxResult = fakeQueryOp(inZSMap)

	private def fakeQueryOp(inZSMap : ZaxSMap) : ZaxResult = {
		val qfEith: Either[ZaxErr, Option[ZaxFlag]] = myMRH.getFlag(inZSMap, MAPKEY_QRY_FLG)

		val dbOpRslt : ZaxResult = qfEith match {
			case Left(err) => Left(err)
			case Right(flgOpt) => {
				val flg = flgOpt.getOrElse(myDfltQryFlg)
				println(s"Resolved query-local flag-opt ${flgOpt} to ${flg}")
				val dbRslt = doCrazyDbStuff(flg)
				dbRslt
			}
		}
		dbOpRslt
	}
	private lazy val myDbAdapter = new ZaxlamDbAdapter {}
	private def doCrazyDbStuff(qflg : Boolean) : ZaxResult = {
		val noRsltAvail: Unit = myDbAdapter.goCrazy
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

	val locDbFlgOpt = Some(false)
	lazy val myTaskMaker = new DistribGenStoreLoadTrial(locDbFlgOpt)

	def goCrazy : Unit = {
		val neatoDbTask = myTaskMaker.mkQuietDbTask
		UnsafeTaskRunner.doRunNow(neatoDbTask)
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