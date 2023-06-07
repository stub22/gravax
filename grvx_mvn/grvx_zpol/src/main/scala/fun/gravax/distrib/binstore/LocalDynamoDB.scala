package fun.gravax.distrib.binstore

import software.amazon.awssdk.auth.credentials.{AwsBasicCredentials, StaticCredentialsProvider}
import software.amazon.awssdk.regions.Region
import zio.{TaskLayer, ZLayer}
import zio.dynamodb.{Item, PrimaryKey, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.aws.dynamodb.{DynamoDb => ZDynDb}

import java.net.URI

// Adapted from https://github.com/zio/zio-dynamodb/blob/series/2.x/examples/src/main/scala/zio/dynamodb/examples/dynamodblocal/DynamoDB.scala

// To use local DB at localhost:8000, all we need is LocalDynamoDB.layer.
// Must be running a localDB with either a docker container (discussed in zio-dynamodb docs) or a java launch like so.
// Docs say there is also an in-memory configuration possible.
// java -Djava.library.path=./dynDBLocal_1210/DynamoDBLocal_lib -jar ./dynDBLocal_1210/DynamoDBLocal.jar  -sharedDb -dbPath ./dyndat_01/
// "NoSQL Workbench" client is useful for inspecting this local DB (at least works for @stub22 on MS Windows 10).
// Can also use that to look at cloud data if

trait LocalDynamoDB {
	private val FAKE_ACCESS_KEY = "dummy"
	private val FAKE_SECRET_KEY = "dummy"

	// Works for a native-local DynamoDB instance when no docker is involved on either client or server side.
	private val regularLocalUrlTxt = "http://localhost:8000"
	// This hostname resolves to localhost, but only when docker is running. This host can be seen from inside+outside docker.
	private val fromDockerUrlTxt = "http://host.docker.internal:8000"


	private val commAwsConf = ZLayer.succeed(
		zio.aws.core.config.CommonAwsConfig(
			region = None,	// What is relation between this .region and the one below in ZDynDb.customized/builder?
			credentialsProvider = StaticCredentialsProvider.create(AwsBasicCredentials.create(FAKE_ACCESS_KEY, FAKE_SECRET_KEY)),
			endpointOverride = None,
			commonClientConfig = None
		)
	)
	private val dfltAwsConf = zio.aws.core.config.AwsConfig.default

	protected def getFlg_useDockerHostnm : Boolean = false
	private val localUrlTxt = if (getFlg_useDockerHostnm) fromDockerUrlTxt else regularLocalUrlTxt

	// Note Region - does this wind up getting used for any configs in the localDB case?
	private val dynamoDbLayer: ZLayer[Any, Throwable, ZDynDb] =
		(zio.aws.netty.NettyHttpClient.default ++ commAwsConf) >>> dfltAwsConf >>> ZDynDb.customized {
			builder =>
				// See also .region ABOVE
				builder.endpointOverride(URI.create(localUrlTxt)).region(Region.US_WEST_2)
		}

	// implicit final class ZLayerProvideSomeOps[RIn, E, ROut](self : zio.ZLayer[RIn, E, ROut]) extends scala.AnyVal
	// def >>>[E1 >: E, ROut2](that : => zio.ZLayer[ROut, E1, ROut2])(implicit trace : zio.Trace) : zio.ZLayer[RIn, E1, ROut2]
	private val myTaskLayer: TaskLayer[ZDynDBExec] = dynamoDbLayer >>> ZDynDBExec.live
	def getLayer : TaskLayer[ZDynDBExec] = {
		println(s"LocalDynamoDB.getLayer used localUrlTxt=${localUrlTxt}, returning myTaskLayer : ${myTaskLayer}")
		myTaskLayer
	}
}
