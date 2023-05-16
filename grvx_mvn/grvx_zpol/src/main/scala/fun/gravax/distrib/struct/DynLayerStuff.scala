package fun.gravax.distrib.struct

import fun.gravax.distrib.binstore.LocalDynamoDB
import software.amazon.awssdk.auth.credentials.{AwsBasicCredentials, ProfileCredentialsProvider, StaticCredentialsProvider}
import software.amazon.awssdk.regions.Region
import zio.aws.core.config.{AwsConfig, CommonAwsConfig}
import zio.aws.core.httpclient.HttpClient
import zio.{RIO, Task, TaskLayer, ULayer, URLayer, ZIO, ZLayer}
import zio.dynamodb.{DynamoDBExecutor => ZDynDBExec}
import zio.aws.dynamodb.{DynamoDb => ZDynDb}
import zio.aws.core.{config => zconfig}
import zio.aws.{netty => znetty}

import java.net.URI

private trait DynLayerStuff
/* To use "&" instead of with, import zio.IntersectionTypeCompat
private[zio] trait IntersectionTypeCompat {
	type &[+A, +B] = A with B
} */
trait DynLayerSetup {
	// type TaskLayer[+ROut] = ZLayer[Any, Throwable, ROut]
	// To use local DB, all we need is this layer.
	val localDB_layer: TaskLayer[ZDynDBExec] = LocalDynamoDB.layer

	val dfltNettyClient: ZLayer[Any, Throwable, HttpClient] = znetty.NettyHttpClient.default

	// Note that AwsConfig is different from CommonAwsConfig
	val dfltAwsConf: ZLayer[HttpClient, Nothing, AwsConfig] = zconfig.AwsConfig.default
	val otherAwsConf: ZLayer[HttpClient with CommonAwsConfig, Nothing, AwsConfig] = zconfig.AwsConfig.configured()

	val zdbLyr: ZLayer[AwsConfig, Throwable, ZDynDb] = ZDynDb.live
	val zdbExecLyr: URLayer[ZDynDb, ZDynDBExec] = ZDynDBExec.live

	// Relies on default settings for credentials and config, such as ~/aws/
	val ezDbLayer: ZLayer[Any, Throwable, ZDynDBExec] = dfltNettyClient >>> dfltAwsConf >>> zdbLyr >>> zdbExecLyr

	/*
	// ZDynDBExec is a trait defining this single method:
	//  def execute[A](atomicQuery : ZDynDBQry[_, A]) : zio.ZIO[scala.Any, scala.Throwable, A]
	 type URLayer[-RIn, +ROut] = zio.ZLayer[RIn, scala.Nothing, ROut]
 	*/

	protected def getFlg_useLocalDB : Boolean = true
	private lazy val myFlg_useLocalDB = getFlg_useLocalDB

	def wireDynamoTask(program : RIO[ZDynDBExec, Unit]) : Task[Unit] = {
		val taskDBLayer : TaskLayer[ZDynDBExec] = if (myFlg_useLocalDB) localDB_layer else ezDbLayer
		program.provide(taskDBLayer)
	}
}
trait MoreDynLayerExamplesUnused {
	// Is profileCred the default already?
	private val profileCred =  ProfileCredentialsProvider.create()


	private val myCommonAwsConfig: ULayer[CommonAwsConfig] = ZLayer.succeed(
		zconfig.CommonAwsConfig(
			region = None,
			credentialsProvider = profileCred,
			endpointOverride = None,
			commonClientConfig = None
		)
	)

	private val exBasicCred = AwsBasicCredentials.create("dummy", "dummy")
	private val exAwsConfig = ZLayer.succeed(
		zconfig.CommonAwsConfig(
			region = None,
			credentialsProvider = StaticCredentialsProvider.create(exBasicCred),
			endpointOverride = None,
			commonClientConfig = None
		)
	)
	private val exDynDbLayer: ZLayer[Any, Throwable, ZDynDb] = {
		// Talking to local server, from zio-dynamodb/LiveSpec.scala
		(znetty.NettyHttpClient.default ++ exAwsConfig) >>> zconfig.AwsConfig.configured() >>> ZDynDb.customized {
			builder =>
				builder.endpointOverride(URI.create("http://localhost:8000")).region(Region.US_WEST_2)
		}
	}
	private val exTestLayer: ZLayer[Any, Throwable, ZDynDBExec] = (exDynDbLayer >>> ZDynDBExec.live)

}
/*


Example From zio-dynamodb/readme.md
  override def run =
    program.provide(
      netty.NettyHttpClient.default,
      config.AwsConfig.default, // uses real AWS dynamodb
      dynamodb.DynamoDb.live,
      DynamoDBExecutor.live
    )

Talking to local server, from zio-dynamodb/LiveSpec.scala
  object DdbHelper {
    import scala.language.implicitConversions

    val ddbLayer: ULayer[DynamoDbAsyncClient] = {
      val effect = ZIO.acquireRelease(for {
        _       <- ZIO.unit
        region   = Region.US_EAST_1
        endpoint = URI.create("http://localhost:8000")
        client   = DynamoDbAsyncClient.builder().endpointOverride(endpoint).region(region).build()
      } yield client)(client => ZIO.attempt(client.close()).ignore)
      ZLayer.fromZIO(ZIO.scoped(effect))
    }

 */

/*
public interface AwsClientBuilder<BuilderT extends AwsClientBuilder<BuilderT, ClientT>, ClientT>
    extends SdkClientBuilder<BuilderT, ClientT> {

    /**
     * Configure the credentials that should be used to authenticate with AWS.
     *
     * <p>The default provider will attempt to identify the credentials automatically using the following checks:
     * <ol>
     *   <li>Java System Properties - <code>aws.accessKeyId</code> and <code>aws.secretAccessKey</code></li>
     *   <li>Environment Variables - <code>AWS_ACCESS_KEY_ID</code> and <code>AWS_SECRET_ACCESS_KEY</code></li>
     *   <li>Credential profiles file at the default location (~/.aws/credentials) shared by all AWS SDKs and the AWS CLI</li>
     *   <li>Credentials delivered through the Amazon EC2 container service if AWS_CONTAINER_CREDENTIALS_RELATIVE_URI environment
     *   variable is set and security manager has permission to access the variable.</li>
     *   <li>Instance profile credentials delivered through the Amazon EC2 metadata service</li>
     * </ol>
     *
     * <p>If the credentials are not found in any of the locations above, an exception will be thrown at {@link #build()} time.
     * </p>
     */
    BuilderT credentialsProvider(AwsCredentialsProvider credentialsProvider);

    /**
     * Configure the region with which the SDK should communicate.
     *
     * <p>If this is not specified, the SDK will attempt to identify the endpoint automatically using the following logic:
     * <ol>
     *     <li>Check the 'aws.region' system property for the region.</li>
     *     <li>Check the 'AWS_REGION' environment variable for the region.</li>
     *     <li>Check the {user.home}/.aws/credentials and {user.home}/.aws/config files for the region.</li>
     *     <li>If running in EC2, check the EC2 metadata service for the region.</li>
     * </ol>
     *
     * <p>If the region is not found in any of the locations above, an exception will be thrown at {@link #build()} time.
     */
    BuilderT region(Region region);
 */