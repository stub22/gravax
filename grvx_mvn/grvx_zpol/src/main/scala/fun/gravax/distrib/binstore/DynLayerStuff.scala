package fun.gravax.distrib.binstore

import software.amazon.awssdk.auth.credentials.{AwsBasicCredentials, ProfileCredentialsProvider, StaticCredentialsProvider}
import zio.aws.core.{config => zconfig}
import zio.aws.core.config.{AwsConfig, CommonAwsConfig}
import zio.aws.{netty => znetty}
import zio.aws.core.httpclient.HttpClient
import zio.dynamodb.{DynamoDBExecutor => ZDynDBExec}
import zio.aws.dynamodb.{DynamoDb => ZDynDb}
import zio.{RIO, Task, TaskLayer, ULayer, URLayer, ZLayer}

private trait DynLayerStuff
//To use "&" instead of with, import some exporter of zio.IntersectionTypeCompat to get    type &[+A, +B] = A with B
// type TaskLayer[+ROut] = ZLayer[Any, Throwable, ROut]
// type URLayer[-RIn, +ROut] = zio.ZLayer[RIn, scala.Nothing, ROut]
// ZDynDBExec is a trait defining this single method:
//  def execute[A](atomicQuery : ZDynDBQry[_, A]) : zio.ZIO[scala.Any, scala.Throwable, A]

class DynLayerSetup(flg_localDb : Boolean, flg_fromDocker : Boolean) {

	private val localDynDbSetup = new LocalDynamoDB {
		override protected def getFlg_connFromDocker: Boolean = flg_fromDocker
	}
	private val localDynLayer: TaskLayer[ZDynDBExec] = localDynDbSetup.getLayer

	private val dfltNettyClient: ZLayer[Any, Throwable, HttpClient] = znetty.NettyHttpClient.default

	// Note that AwsConfig is different from CommonAwsConfig
	private val dfltAwsConf: ZLayer[HttpClient, Nothing, AwsConfig] = zconfig.AwsConfig.default
	private val otherAwsConf_UNUSED: ZLayer[HttpClient with CommonAwsConfig, Nothing, AwsConfig] = zconfig.AwsConfig.configured()

	private val zdbLyr: ZLayer[AwsConfig, Throwable, ZDynDb] = ZDynDb.live
	private val zdbExecLyr: URLayer[ZDynDb, ZDynDBExec] = ZDynDBExec.live

	// Relies on default settings for credentials and config, such as ~/aws/
	private val dfltAwsDynLayer: ZLayer[Any, Throwable, ZDynDBExec] = dfltNettyClient >>> dfltAwsConf >>> zdbLyr >>> zdbExecLyr

	private lazy val myFlg_useLocalDB = flg_localDb

	def wireDynamoTask[Rslt](program : RIO[ZDynDBExec, Rslt]) : Task[Rslt] = {
		val taskDBLayer : TaskLayer[ZDynDBExec] = if (myFlg_useLocalDB) localDynLayer else dfltAwsDynLayer
		program.provide(taskDBLayer)
	}
}
private trait MoreDynLayerExamples_UNUSED {
	// Some code scraps we copied from zio-dynamodb source and tweaked
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
	// Below is first step of LocalDynamoDB setup.
	private val exBasicCred = AwsBasicCredentials.create("dummy", "dummy")
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

Talking to local server, from zio-dynamodb/PartialCopyOfZioDynamoDbLiveSpec.scala
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