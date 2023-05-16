package fun.gravax.zdynamo

import fun.gravax.distrib.binstore.LocalDynamoDB
import zio.dynamodb.{DynamoDBExecutor => ZDynDBExec, DynamoDBQuery => ZDynDBQry}
import zio.{Console, RIO, Task, TaskLayer, ZIO, ZIOAppDefault, ZLayer, dynamodb => ZDyn}
import zio.stream.ZStream

// Adapted by @stub22, starting from
// https://github.com/zio/zio-dynamodb/blob/series/2.x/examples/src/main/scala/zio/dynamodb/examples/dynamodblocal/StudentZioDynamoDbExample.scala
/**
 * An equivalent app to [[StudentJavaSdkExample]] but using `zio-dynamodb` - note the reduction in boiler plate code!
 */
object StudentZioDynamoDbExample extends ZIOAppDefault {
	import Student.schema  // Used implicitly in call to ZDyn.batchReadFromStream.

	val studTblNm = "student"
	val studKeySchm = ZDyn.KeySchema(Student.FLDNM_EMAIL, Student.FLDNM_SUBJECT)
	val emailAttrDef = ZDyn.AttributeDefinition.attrDefnString(Student.FLDNM_EMAIL)
	val subjAttrDef = ZDyn.AttributeDefinition.attrDefnString(Student.FLDNM_SUBJECT)

	val (flg_doCreate, flg_doDelete) = (false, false)
	val streamOfStudents = ZStream(Student.avi, Student.adam)


	private def mkProgram = for {
		_ <- maybeCreateStudTable
		_ <- ZDyn.batchWriteFromStream(streamOfStudents) { student =>
			ZDynDBQry.put(studTblNm, student)
		}.runDrain
		_ <- ZDynDBQry.put(studTblNm, Student.avi.copy(payment = Payment.CreditCard)).execute
		_ <- ZDyn.batchReadFromStream(studTblNm, streamOfStudents)(s => Student.primaryKey(s.email, s.subject)) // implicitly uses schema
				.tap(pair => Console.printLine(s"student=${pair._2}"))
				.runDrain
		_ <- maybeDeleteStudTable
	} yield ()

	/*
	RIO[R, A] is a type alias for ZIO[R, Throwable, A], which represents an effect that requires an R,
	and may fail with a Throwable value, or succeed with an A.
	type RIO[-R, +A]  = ZIO[R, Throwable, A]
	 */

	def maybeCreateStudTable: RIO[ZDynDBExec, Unit] = if (flg_doCreate) {
		ZDynDBQry.createTable(studTblNm, studKeySchm, ZDyn.BillingMode.PayPerRequest)(
			emailAttrDef, subjAttrDef).execute
	} else ZIO.succeed()


	def maybeDeleteStudTable: RIO[ZDynDBExec, Unit] = if (flg_doDelete) {
		ZDynDBQry.deleteTable(studTblNm).execute
	}  else ZIO.succeed()

	/*
	Task[A] is a type alias for ZIO[Any, Throwable, A], which represents an effect that has no requirements,
	and may fail with a Throwable value, or succeed with an A.
	type Task[+A] = ZIO[Any, Throwable, A]
	 */

	override def run : Task[Unit] = {
		// ZDynDBExec is a trait defining this single method:
		//  def execute[A](atomicQuery : ZDynDBQry[_, A]) : zio.ZIO[scala.Any, scala.Throwable, A]
		/*
		TaskLayer[+ROut] is a type alias for ZLayer[Any, Throwable, ROut]
		type TaskLayer[+ROut] = ZLayer[Any, Throwable, ROut]
		 */
		val localDB_layer: TaskLayer[ZDynDBExec] = LocalDynamoDB.layer
		mkProgram.provide(localDB_layer)
	}
}
