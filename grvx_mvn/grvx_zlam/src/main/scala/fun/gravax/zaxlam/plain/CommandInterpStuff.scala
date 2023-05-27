package fun.gravax.zaxlam.plain

import java.util.{List => JList, Map => JMap}
import scala.collection.immutable.{Map => SMap}


private trait CommandInterpStuff

// Nappy comes after Mappy, until we pick a better name.
class NappyZaxlam extends MappyZaxlam {
	override protected def lambdaScala(inSMap : SMap[String, AnyRef]) : SMap[String, AnyRef] = {
		val echoedInputJMap = deepConvertSMapToJMap(inSMap)
		val dummyOutSMap = SMap[String, AnyRef](MAPKEY_ECHO_MAP -> echoedInputJMap) // , "ENV_JMAP" -> envJMap)
		dummyOutSMap
	}
}
// TODO:
//  Deploy to AWS
//  invoke on AWS from sam and from our LambdaClient, also using AWS web console.
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