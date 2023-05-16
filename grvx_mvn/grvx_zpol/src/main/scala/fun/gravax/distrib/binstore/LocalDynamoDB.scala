package fun.gravax.distrib.binstore

import software.amazon.awssdk.auth.credentials.{AwsBasicCredentials, StaticCredentialsProvider}
import software.amazon.awssdk.regions.Region
import zio.{TaskLayer, ZLayer}
import zio.dynamodb.{Item, PrimaryKey, DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.aws.dynamodb.{DynamoDb => ZDynDb}

import java.net.URI

// Adapted from https://github.com/zio/zio-dynamodb/blob/series/2.x/examples/src/main/scala/zio/dynamodb/examples/dynamodblocal/DynamoDB.scala

// Seems there is more than one way to run
// To use local DB at localhost:8000, all we need is LocalDynamoDB.layer.
// Must be running a localDB with either a docker container (discussed in zio-dynamodb docs) or a java launch like so.
// Docs say there is also an in-memory configuration possible.
// java -Djava.library.path=./dynDBLocal_1210/DynamoDBLocal_lib -jar ./dynDBLocal_1210/DynamoDBLocal.jar  -sharedDb -dbPath ./dyndat_01/
// "NoSQL Workbench" client is useful for inspecting this local DB (at least works for @stub22 on MS Windows 10).
// Can also use that to look at cloud data if

object LocalDynamoDB {
	val awsConfig = ZLayer.succeed(
		zio.aws.core.config.CommonAwsConfig(
			region = None,
			credentialsProvider = StaticCredentialsProvider.create(AwsBasicCredentials.create("dummy", "dummy")),
			endpointOverride = None,
			commonClientConfig = None
		)
	)

	// Note Region - does this wind up getting used for any configs in the localDB case?
	val dynamoDbLayer: ZLayer[Any, Throwable, ZDynDb] =
		(zio.aws.netty.NettyHttpClient.default ++ awsConfig) >>> zio.aws.core.config.AwsConfig.default >>> ZDynDb.customized {
			builder =>
				builder.endpointOverride(URI.create("http://localhost:8000")).region(Region.US_WEST_2)
		}

	// implicit final class ZLayerProvideSomeOps[RIn, E, ROut](self : zio.ZLayer[RIn, E, ROut]) extends scala.AnyVal
	// def >>>[E1 >: E, ROut2](that : => zio.ZLayer[ROut, E1, ROut2])(implicit trace : zio.Trace) : zio.ZLayer[RIn, E1, ROut2]
	val layer: TaskLayer[ZDynDBExec] = dynamoDbLayer >>> ZDynDBExec.live
}
