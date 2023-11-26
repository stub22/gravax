package fun.gravax.zaxlam.fblob

import com.amazonaws.services.lambda.runtime.Context
import com.amazonaws.services.lambda.runtime.events.S3Event
import com.amazonaws.services.lambda.runtime.events.models.s3.S3EventNotification
import fun.gravax.zaxlam.plain.{AxLamHandler, KnowsHappyVocab, MappyZaxlam}
import fun.gravax.zaxlam.srvlss.UnsafeTaskRunner
import fun.gravax.zaxlam.xform.{ZaxTypes, ZaxlamMapReadHelper}
import zio.{Cause, Task, ZIO, ZIOAppArgs}

import java.util.{HashMap => JHMap, List => JList, Map => JMap}


class StrongTypedBlobbyZaxlamAintWorkin extends AxLamHandler[S3Event,JMap[String, AnyRef]] {
	// Gets invoked, but inS3evt is an empty collection.
	// EventBridge format is different from what S3 publishes
	// https: //stackoverflow.com/questions/71576949/map-eventbridge-json-for-an-s3-event-to-a-java-object
	// https://github.com/aws/aws-lambda-java-libs/tree/main
	// https://github.com/aws/aws-lambda-java-libs/issues/109
	// https://docs.aws.amazon.com/AmazonS3/latest/userguide/notification-content-structure.html
	// https://github.com/aws/aws-lambda-java-libs/tree/events-v4-serialization-v2/aws-lambda-java-events/src/main/java/com/amazonaws/services/lambda/runtime/events
	override protected def handleRq(inS3evt: S3Event, ctx: Context): JMap[String, AnyRef] = {
		println(s"BlobbyZaxlam.handleRq got in-S3Event=${inS3evt}")

		val x: JList[S3EventNotification.S3EventNotificationRecord] = inS3evt.getRecords
		println(s"Got notice list: ${x})")
		val x1: S3EventNotification.S3EventNotificationRecord = x.get(0)
		val s3info: S3EventNotification.S3Entity = x1.getS3
		println(s"Got s3info: ${s3info}")
		val buck = s3info.getBucket
		val obj = s3info.getObject
		println(s"Got bucket=${buck}, object=${obj}")
		val outMap = new java.util.HashMap[String, AnyRef]()
		outMap.put("mutable", "ism")
		outMap
	}
}
/*
When run with local invoke it appears the function is running in us-east-1, even though that is not our default
region, and even though we specify --region like
sam local invoke --region us-west-2  -e .\blobCreateEvtFromS3.json

funcArn=arn:aws:lambda:us-east-1:012345678912:function:BlobbyZaxlamRsrc

Direct access using S3 client in AWS SDK is working under local Lambda.

 */
class BlobbyZaxlam extends MappyZaxlam with ZaxTypes with KnowsHappyVocab {

	val myMRH = new ZaxlamMapReadHelper {}
	lazy private val myUnsafeRunner = new UnsafeTaskRunner {}

	override def lambdaScala(inZSMap: ZaxSMap): ZaxSMap = {
		println(s"BlobbyZaxlam.lambdaScala got inZSMap=${inZSMap}")
		val detailMap: Option[AnyRef] = inZSMap.get("detail")
		println(s"BlobbyZaxlam.lambdaScala.detail = ${detailMap}")
		val blobScen = new BlobTextContentScenario{}
		val objectKey = "dumTxtBlob_1700904704498.txt"
		val readJob: ZIO[Any, Object, String] = blobScen.mkReadingWiredJob(objectKey)
		val readJobDT = readJob.timed.debug("readJob.timed output: ")
	//	val s: ZIO[Any, Cause[Object], String] = readJob.sandbox
//		val x: ZIO[Any, Exception, String] = readJob.orElseFail(new Exception("oops readJob failed"))
		val rjwex: ZIO[Any, Exception, (zio.Duration, String)] = readJobDT.mapError(obj => new Exception(s"readJob failed with err: ${obj.toString}"))
		val readTask : Task[(zio.Duration, String)] =  rjwex
		// val readTask = readJob.p
		if (false) {
			val out: Either[Throwable, (zio.Duration, String)] = myUnsafeRunner.doRunUnsafeTaskToEither(readTask)
			val outTxt = out.toString
			println(s"BlobbyZaxlam.lambdaScala got read task output as either: ${outTxt}")
		} else if (false) {
			val otherTask = readJob.mapError(obj => new Exception(s"readJob failed with err: ${obj.toString}"))
			val taskOutput: String = myUnsafeRunner.doRunUnsafeTaskMightThrow(otherTask)
			println(s"BlobbyZaxlam.lambdaScala got read task output: ${taskOutput}")
		} else {
			// Seems that when we access using this AP-name, Lambda role needs to have permissions to access the
			// arn of the access point (which is different from the arn of the bucket itself).
			val blobBucketName = "west-bdib01-acc-aa-dn9y8m67asjjed3gx7cry7amy93causw2a-s3alias"
			val oldSynk = new ReadS3LikeGravcld {}
			val x = oldSynk.fetchBlob(blobBucketName, objectKey)
		}
		val outSMap = inZSMap
		outSMap
	}

	def readSync = {
	// 	val r3sf = new ReadS3Files{}

	}
}
trait ReadS3LikeGravcld {

	import java.time.Instant
	import java.util.{List => JList}
	import java.util.stream

	import software.amazon.awssdk.core.{ResponseBytes, SdkField}
	import software.amazon.awssdk.regions.Region
	import software.amazon.awssdk.services.s3.S3Client
	import software.amazon.awssdk.services.s3.model.{Bucket, GetObjectRequest, GetObjectResponse, ListBucketsRequest, ListBucketsResponse, ListObjectsRequest, ListObjectsResponse, S3Object}

	def fetchBlob(blobBucketNm : String, blobObjKey : String) : String = {
		// First we fetch a prefixes blob, as validation that we can talk to S3.
		val bcktNm_appOntMiscPub = "appstract-ont-misc-pub"
		val objKey_kbPrefixes = "kbpedia_v250/supp/kbprc_prefixes.n3"
		val s3cli = mkS3Client
		val kbPrfxTxt = loadKbPrefixesText(s3cli, bcktNm_appOntMiscPub, objKey_kbPrefixes)
		// OK now try fetching the blob we were asked for.
		val blobTxt = getBucktObjBytesAsUtf8(s3cli, blobBucketNm, blobObjKey)
		println(s"Got blobTxt: ${blobTxt}")
		blobTxt
	}
	def mkS3Client: S3Client = {
		val region = Region.US_WEST_2;
		println("Building s3Cli")
		/*
	https://sdk.amazonaws.com/java/api/latest/software/amazon/awssdk/services/s3/S3ClientBuilder.html
	BuilderT region(Region region)
	Configure the region with which the SDK should communicate.
	If this is not specified, the SDK will attempt to identify the endpoint automatically using the following logic:

	Check the 'aws.region' system property for the region.
	Check the 'AWS_REGION' environment variable for the region.
	Check the {user.home}/.aws/credentials and {user.home}/.aws/config files for the region.
	If running in EC2, check the EC2 metadata service for the region.
	If the region is not found in any of the locations above, an exception will be thrown at SdkBuilder.build() time.
		*/

		val s3cli = S3Client.builder().region(region).build();
		/*
	https://sdk.amazonaws.com/java/api/latest/software/amazon/awssdk/services/s3/S3Client.html
		*/
		println(s"Made s3Cli ${s3cli.toString}")
		s3cli
	}

	def loadKbPrefixesText(s3cli: S3Client, bcktNm: String, kbprfxObjKey: String): String = {
		val kbprfxTxt = getBucktObjBytesAsUtf8(s3cli, bcktNm, kbprfxObjKey)
		println(s"Loaded kb-prefix text of length=${kbprfxTxt.length}")
		println(s"First 127 chars: ", kbprfxTxt.take(127))
		kbprfxTxt
	}

	def getBucktObjBytesAsUtf8(s3cli: S3Client, bcktNm: String, objKy: String): String = {
		val gobjRq = buildGetObjRq(bcktNm, objKy)
		val respBytesObj: ResponseBytes[GetObjectResponse] = s3cli.getObjectAsBytes(gobjRq)
		respBytesObj.asUtf8String()
		//  GetObjectData.getObjectBytes(s3,bucketName,objectKey, path);
	}

	def buildGetObjRq(bcktNm: String, objKy: String): GetObjectRequest = {
		val emptyBldr = GetObjectRequest.builder()
		val readyBldr = emptyBldr.bucket(bcktNm).key(objKy)
		val rq = readyBldr.build()
		rq
	}
}
/*
BlobbyZaxlam.lambdaScala got inZSMap=HashMap(
detail-type -> Object Created,
source -> aws.s3,
version -> 0,
id -> 1e657a5b-5efd-c6f6-a60c-3e181e0af4d7,
resources -> [arn:aws:s3:::bux-distrib-ingest-bucket-01],
region -> us-west-2,
detail -> {version=0,
	bucket={name=bux-distrib-ingest-bucket-01},
	object={key=dumTxtBlob_1700899723828.txt,
	size=185,
	etag=8b56107b5dd6ccde9c6b74b1cd7711c8,
	sequencer=006561AB8C8ED5611D},
request-id=8RJYD66ZE45SV7H5, requester=693649829226, source-ip-address=47.6.209.194, reason=PutObject}, account -> 693649829226, time -> 2023-11-25T08:08:44Z)

 */

/*
EventBridge format is different from what S3 publishes
https://stackoverflow.com/questions/71576949/map-eventbridge-json-for-an-s3-event-to-a-java-object

https://docs.aws.amazon.com/AmazonS3/latest/userguide/notification-content-structure.html

In CloudWatch log, an S3 event from a blob write (routed through Event Bridge) looks like this:

{
	"version": "0",
	"id": "1e657a5b-5efd-c6f6-a60c-3e181e0af4d7",
	"detail-type": "Object Created",
	"source": "aws.s3",
	"account": "693649829226",
	"time": "2023-11-25T08:08:44Z",
	"region": "us-west-2",
	"resources": [
		"arn:aws:s3:::bux-distrib-ingest-bucket-01"
	],
	"detail": {
		"version": "0",
		"bucket": {
			"name": "bux-distrib-ingest-bucket-01"
		},
		"object": {
			"key": "dumTxtBlob_1700899723828.txt",
			"size": 185,
			"etag": "8b56107b5dd6ccde9c6b74b1cd7711c8",
			"sequencer": "006561AB8C8ED5611D"
		},
		"request-id": "8RJYD66ZE45SV7H5",
		"requester": "693649829226",
		"source-ip-address": "47.6.209.194",
		"reason": "PutObject"
	}
}

========================================================
Meanwhile our default map-noodling output looks like:
=======================================================
2023-11-25T00:08:47.046-08:00	INIT_START Runtime Version: java:11.v30 Runtime Version ARN: arn:aws:lambda:us-west-2::runtime:b618e7d8279121da58fc61d7c8e12828ab44e8fe45eb3b24503ba85483215f7a

2023-11-25T00:08:47.076-08:00	Picked up JAVA_TOOL_OPTIONS: -XX:+TieredCompilation -XX:TieredStopAtLevel=1

2023-11-25T00:08:47.527-08:00	START RequestId: 356955e1-c95d-4710-b280-dac4717dd717 Version: $LATEST

2023-11-25T00:08:48.117-08:00	HelloZaxlam.handleRequest is printing to console?!

2023-11-25T00:08:48.118-08:00	HandlerMon.dumpCtx got ctx: lambdainternal.api.LambdaContext@7cef4e59

2023-11-25T00:08:48.118-08:00	HandlerMon.dumpCtx got cognito-identity: null

2023-11-25T00:08:48.118-08:00	HandlerMon.dumpCtx got client-context: null

2023-11-25T00:08:48.118-08:00	HandlerMon.dumpCtx got awsRqId=356955e1-c95d-4710-b280-dac4717dd717, funcArn=arn:aws:lambda:us-west-2:693649829226:function:BlobbyZaxlamFunc105A, funcNm=BlobbyZaxlamFunc105A, funcVers=$LATEST

2023-11-25T00:08:48.118-08:00	HandlerMon.dumpCtx got memLimit=1024, remainTime=9409, logGrpNm=/aws/lambda/BlobbyZaxlamFunc105A, logStrmNm=2023/11/25/[$LATEST]f9359a684cdb486982a7d43412429f75

2023-11-25T00:08:48.118-08:00	AxLamHandler.handleRequest is naively dumping input as .toString: {version=0, id=1e657a5b-5efd-c6f6-a60c-3e181e0af4d7, detail-type=Object Created, source=aws.s3, account=693649829226, time=2023-11-25T08:08:44Z, region=us-west-2, resources=[arn:aws:s3:::bux-distrib-ingest-bucket-01], detail={version=0, bucket={name=bux-distrib-ingest-bucket-01}, object={key=dumTxtBlob_1700899723828.txt, size=185, etag=8b56107b5dd6ccde9c6b74b1cd7711c8, sequencer=006561AB8C8ED5611D}, request-id=8RJYD66ZE45SV7H5, requester=693649829226, source-ip-address=47.6.209.194, reason=PutObject}}

2023-11-25T00:08:48.118-08:00	Map entity[AxLamHandler.handleRequest.IN]:

2023-11-25T00:08:48.283-08:00	Naive map pre-dump: HashMap(detail-type -> Object Created, source -> aws.s3, version -> 0, id -> 1e657a5b-5efd-c6f6-a60c-3e181e0af4d7, resources -> [arn:aws:s3:::bux-distrib-ingest-bucket-01], region -> us-west-2, detail -> {version=0, bucket={name=bux-distrib-ingest-bucket-01}, object={key=dumTxtBlob_1700899723828.txt, size=185, etag=8b56107b5dd6ccde9c6b74b1cd7711c8, sequencer=006561AB8C8ED5611D}, request-id=8RJYD66ZE45SV7H5, requester=693649829226, source-ip-address=47.6.209.194, reason=PutObject}, account -> 693649829226, time -> 2023-11-25T08:08:44Z)

2023-11-25T00:08:48.296-08:00	String entity[detail-type]: Object Created

2023-11-25T00:08:48.298-08:00	String entity[source]: aws.s3

2023-11-25T00:08:48.298-08:00	String entity[version]: 0

2023-11-25T00:08:48.298-08:00	String entity[id]: 1e657a5b-5efd-c6f6-a60c-3e181e0af4d7

2023-11-25T00:08:48.298-08:00	List entity[resources]:

2023-11-25T00:08:48.319-08:00	Dumping list: List(arn:aws:s3:::bux-distrib-ingest-bucket-01)

2023-11-25T00:08:48.321-08:00	String entity[lstElem[0]]: arn:aws:s3:::bux-distrib-ingest-bucket-01

2023-11-25T00:08:48.321-08:00	String entity[region]: us-west-2

2023-11-25T00:08:48.321-08:00	Map entity[detail]:

2023-11-25T00:08:48.321-08:00	Naive map pre-dump: HashMap(request-id -> 8RJYD66ZE45SV7H5, requester -> 693649829226, reason -> PutObject, version -> 0, bucket -> {name=bux-distrib-ingest-bucket-01}, source-ip-address -> 47.6.209.194, object -> {key=dumTxtBlob_1700899723828.txt, size=185, etag=8b56107b5dd6ccde9c6b74b1cd7711c8, sequencer=006561AB8C8ED5611D})

2023-11-25T00:08:48.321-08:00	String entity[request-id]: 8RJYD66ZE45SV7H5

2023-11-25T00:08:48.321-08:00	String entity[requester]: 693649829226

2023-11-25T00:08:48.321-08:00	String entity[reason]: PutObject

2023-11-25T00:08:48.321-08:00	String entity[version]: 0

2023-11-25T00:08:48.321-08:00	Map entity[bucket]:

2023-11-25T00:08:48.323-08:00	Naive map pre-dump: Map(name -> bux-distrib-ingest-bucket-01)

2023-11-25T00:08:48.324-08:00	String entity[name]: bux-distrib-ingest-bucket-01

2023-11-25T00:08:48.324-08:00	String entity[source-ip-address]: 47.6.209.194

2023-11-25T00:08:48.324-08:00	Map entity[object]:

2023-11-25T00:08:48.326-08:00	Naive map pre-dump: Map(key -> dumTxtBlob_1700899723828.txt, size -> 185, etag -> 8b56107b5dd6ccde9c6b74b1cd7711c8, sequencer -> 006561AB8C8ED5611D)

2023-11-25T00:08:48.326-08:00	String entity[key]: dumTxtBlob_1700899723828.txt

2023-11-25T00:08:48.326-08:00	Integer entity[size]: 185

2023-11-25T00:08:48.326-08:00	String entity[etag]: 8b56107b5dd6ccde9c6b74b1cd7711c8

2023-11-25T00:08:48.326-08:00	String entity[sequencer]: 006561AB8C8ED5611D

2023-11-25T00:08:48.326-08:00	String entity[account]: 693649829226

2023-11-25T00:08:48.326-08:00	String entity[time]: 2023-11-25T08:08:44Z

2023-11-25T00:08:48.326-08:00	BlobbyZaxlam.lambdaScala got inZSMap=HashMap(detail-type -> Object Created, source -> aws.s3, version -> 0, id -> 1e657a5b-5efd-c6f6-a60c-3e181e0af4d7, resources -> [arn:aws:s3:::bux-distrib-ingest-bucket-01], region -> us-west-2, detail -> {version=0, bucket={name=bux-distrib-ingest-bucket-01}, object={key=dumTxtBlob_1700899723828.txt, size=185, etag=8b56107b5dd6ccde9c6b74b1cd7711c8, sequencer=006561AB8C8ED5611D}, request-id=8RJYD66ZE45SV7H5, requester=693649829226, source-ip-address=47.6.209.194, reason=PutObject}, account -> 693649829226, time -> 2023-11-25T08:08:44Z)

2023-11-25T00:08:48.340-08:00	AxLamHandler.handleRequest is naively dumping output as .toString: {detail-type=Object Created, source=aws.s3, version=0, id=1e657a5b-5efd-c6f6-a60c-3e181e0af4d7, resources=[arn:aws:s3:::bux-distrib-ingest-bucket-01], region=us-west-2, detail={version=0, bucket={name=bux-distrib-ingest-bucket-01}, object={key=dumTxtBlob_1700899723828.txt, size=185, etag=8b56107b5dd6ccde9c6b74b1cd7711c8, sequencer=006561AB8C8ED5611D}, request-id=8RJYD66ZE45SV7H5, requester=693649829226, source-ip-address=47.6.209.194, reason=PutObject}, account=693649829226, time=2023-11-25T08:08:44Z}

2023-11-25T00:08:48.340-08:00	Map entity[AxLamHandler.handleRequest.OUT]:

2023-11-25T00:08:48.341-08:00	Naive map pre-dump: HashMap(detail-type -> Object Created, source -> aws.s3, version -> 0, id -> 1e657a5b-5efd-c6f6-a60c-3e181e0af4d7, resources -> [arn:aws:s3:::bux-distrib-ingest-bucket-01], region -> us-west-2, detail -> {version=0, bucket={name=bux-distrib-ingest-bucket-01}, object={key=dumTxtBlob_1700899723828.txt, size=185, etag=8b56107b5dd6ccde9c6b74b1cd7711c8, sequencer=006561AB8C8ED5611D}, request-id=8RJYD66ZE45SV7H5, requester=693649829226, source-ip-address=47.6.209.194, reason=PutObject}, account -> 693649829226, time -> 2023-11-25T08:08:44Z)

2023-11-25T00:08:48.341-08:00	String entity[detail-type]: Object Created

2023-11-25T00:08:48.341-08:00	String entity[source]: aws.s3

2023-11-25T00:08:48.341-08:00	String entity[version]: 0

2023-11-25T00:08:48.341-08:00	String entity[id]: 1e657a5b-5efd-c6f6-a60c-3e181e0af4d7

2023-11-25T00:08:48.341-08:00	List entity[resources]:

2023-11-25T00:08:48.341-08:00	Dumping list: List(arn:aws:s3:::bux-distrib-ingest-bucket-01)

2023-11-25T00:08:48.341-08:00	String entity[lstElem[0]]: arn:aws:s3:::bux-distrib-ingest-bucket-01

2023-11-25T00:08:48.341-08:00	String entity[region]: us-west-2

2023-11-25T00:08:48.341-08:00	Map entity[detail]:

2023-11-25T00:08:48.341-08:00	Naive map pre-dump: HashMap(request-id -> 8RJYD66ZE45SV7H5, requester -> 693649829226, reason -> PutObject, version -> 0, bucket -> {name=bux-distrib-ingest-bucket-01}, source-ip-address -> 47.6.209.194, object -> {key=dumTxtBlob_1700899723828.txt, size=185, etag=8b56107b5dd6ccde9c6b74b1cd7711c8, sequencer=006561AB8C8ED5611D})

2023-11-25T00:08:48.341-08:00	String entity[request-id]: 8RJYD66ZE45SV7H5

2023-11-25T00:08:48.341-08:00	String entity[requester]: 693649829226

2023-11-25T00:08:48.341-08:00	String entity[reason]: PutObject

2023-11-25T00:08:48.341-08:00	String entity[version]: 0

2023-11-25T00:08:48.341-08:00	Map entity[bucket]:

2023-11-25T00:08:48.341-08:00	Naive map pre-dump: Map(name -> bux-distrib-ingest-bucket-01)

2023-11-25T00:08:48.341-08:00	String entity[name]: bux-distrib-ingest-bucket-01

2023-11-25T00:08:48.341-08:00	String entity[source-ip-address]: 47.6.209.194

2023-11-25T00:08:48.341-08:00	Map entity[object]:

2023-11-25T00:08:48.341-08:00	Naive map pre-dump: Map(key -> dumTxtBlob_1700899723828.txt, size -> 185, etag -> 8b56107b5dd6ccde9c6b74b1cd7711c8, sequencer -> 006561AB8C8ED5611D)

2023-11-25T00:08:48.341-08:00	String entity[key]: dumTxtBlob_1700899723828.txt

2023-11-25T00:08:48.341-08:00	Integer entity[size]: 185

2023-11-25T00:08:48.341-08:00	String entity[etag]: 8b56107b5dd6ccde9c6b74b1cd7711c8

2023-11-25T00:08:48.341-08:00	String entity[sequencer]: 006561AB8C8ED5611D

2023-11-25T00:08:48.341-08:00	String entity[account]: 693649829226

2023-11-25T00:08:48.341-08:00	String entity[time]: 2023-11-25T08:08:44Z

2023-11-25T00:08:48.362-08:00	END RequestId: 356955e1-c95d-4710-b280-dac4717dd717

2023-11-25T00:08:48.362-08:00	REPORT RequestId: 356955e1-c95d-4710-b280-dac4717dd717 Duration: 834.86 ms Billed Duration: 835 ms Memory Size: 1024 MB Max Memory Used: 94 MB Init Duration: 479.18 ms


Above is for a cold instance.   When it hits a warm instance the time is dramatically better!

2023-11-25T00:15:55.574-08:00	REPORT RequestId: 7186caa8-df22-43a7-9813-8b4eba49e967 Duration: 6.73 ms Billed Duration: 7 ms Memory Size: 1024 MB Max Memory Used: 95 MB
 */

// Think this one is just for direct dispatch from S3 to Lambda:
// https://javadoc.io/doc/com.amazonaws/aws-lambda-java-events/latest/com/amazonaws/services/lambda/runtime/events/S3ObjectLambdaEvent.html
