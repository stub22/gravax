package fun.gravax.zaxlam.fblob

import com.amazonaws.services.lambda.runtime.Context
import com.amazonaws.services.lambda.runtime.events.S3Event
import com.amazonaws.services.lambda.runtime.events.models.s3.S3EventNotification
import fun.gravax.zaxlam.plain.{AxLamHandler, KnowsHappyVocab, MappyZaxlam}
import fun.gravax.zaxlam.xform.{ZaxTypes, ZaxlamMapReadHelper}

import java.util.{HashMap => JHMap, List => JList, Map => JMap}


class BlobbyZaxlam extends AxLamHandler[S3Event,JMap[String, AnyRef]] {
	override protected def handleRq(in: S3Event, ctx: Context): JMap[String, AnyRef] = {
		println(s"BlobbyZaxlam.handleRq got in-S3Event=${in}")

		val x: JList[S3EventNotification.S3EventNotificationRecord] = in.getRecords
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

class JsonLovinBlobbyZaxlam extends MappyZaxlam with ZaxTypes with KnowsHappyVocab {

	val myMRH = new ZaxlamMapReadHelper {}

	override def lambdaScala(inZSMap: ZaxSMap): ZaxSMap = {
		println(s"BlobbyZaxlam.lambdaScala got inZSMap=${inZSMap}")
		val detailMap = inZSMap.get("detail")
		val outSMap = inZSMap
		outSMap
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
