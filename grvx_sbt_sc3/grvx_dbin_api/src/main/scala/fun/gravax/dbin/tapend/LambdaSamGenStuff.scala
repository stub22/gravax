package fun.gravax.dbin.tapend

private trait LambdaSamGenStuff

import sttp.tapir.serverless.aws.sam._

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

/*
Copied from:
https://github.com/softwaremill/tapir/blob/master/serverless/aws/lambda-cats-effect-tests/src/main/scala/sttp/tapir/serverless/aws/lambda/tests/LambdaSamTemplate.scala
*/
object LambdaSamTemplate extends App {

	val jarPath =
		Paths.get("serverless/aws/lambda-cats-effect-tests/target/jvm-2.13/tapir-aws-lambda-cats-effect-tests.jar").toAbsolutePath.toString

	val samOptions: AwsSamOptions = AwsSamOptions(
		"Tests",
		source = CodeSource(
			"java11",
			jarPath,
			"sttp.tapir.serverless.aws.lambda.tests.IOLambdaHandlerV2::handleRequest"
		),
		memorySize = 1024
	)
	val endptsForSam = Endpoints.apiEndpoints
	val yaml = AwsSamInterpreter(samOptions).toSamTemplate(endptsForSam.map(_.endpoint).toList).toYaml
	Files.write(Paths.get("aws-lambda-cats-effect-template.yaml"), yaml.getBytes(UTF_8))
}
