package fun.gravax.zdynamo

import software.amazon.awssdk.auth.credentials.{AwsBasicCredentials, StaticCredentialsProvider}
import software.amazon.awssdk.regions.Region
import zio.{TaskLayer, ZLayer}
import zio.aws.core.config
import zio.aws.dynamodb.DynamoDb
import zio.aws.{dynamodb, netty}
import zio.dynamodb.{DynamoDBExecutor => ZDynDBExec}

import java.net.URI

// Adapted from https://github.com/zio/zio-dynamodb/blob/series/2.x/examples/src/main/scala/zio/dynamodb/examples/dynamodblocal/DynamoDB.scala
object LocalDynamoDB {
  val awsConfig = ZLayer.succeed(
    config.CommonAwsConfig(
      region = None,
      credentialsProvider = StaticCredentialsProvider.create(AwsBasicCredentials.create("dummy", "dummy")),
      endpointOverride = None,
      commonClientConfig = None
    )
  )

  val dynamoDbLayer: ZLayer[Any, Throwable, DynamoDb] =
    (netty.NettyHttpClient.default ++ awsConfig) >>> config.AwsConfig.default >>> dynamodb.DynamoDb.customized {
      builder =>
        builder.endpointOverride(URI.create("http://localhost:8000")).region(Region.US_EAST_1)
    }

  // implicit final class ZLayerProvideSomeOps[RIn, E, ROut](self : zio.ZLayer[RIn, E, ROut]) extends scala.AnyVal
  // def >>>[E1 >: E, ROut2](that : => zio.ZLayer[ROut, E1, ROut2])(implicit trace : zio.Trace) : zio.ZLayer[RIn, E1, ROut2]
  val layer : TaskLayer[ZDynDBExec] = dynamoDbLayer >>> ZDynDBExec.live
}
