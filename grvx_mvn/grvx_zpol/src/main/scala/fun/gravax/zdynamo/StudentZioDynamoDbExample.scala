package fun.gravax.zdynamo

import zio.dynamodb.DynamoDBQuery.{createTable, put}
import zio.dynamodb._
import zio.stream.ZStream
import zio.{Console, ZIO, ZIOAppDefault}

/**
 * An equivalent app to [[StudentJavaSdkExample]] but using `zio-dynamodb` - note the reduction in boiler plate code!
 */
object StudentZioDynamoDbExample extends ZIOAppDefault {
    import Student.schema
    val streamOfStudents = ZStream(Student.avi, Student.adam)

  private def mkProgram = for {
    _ <- maybeCreateStudTable
      /*createTable("student", KeySchema("email", "subject"), BillingMode.PayPerRequest)(
           AttributeDefinition.attrDefnString("email"),
           AttributeDefinition.attrDefnString("subject")
         ).execute
       */
    _ <- batchWriteFromStream(streamOfStudents) { student =>
           put("student", student)
         }.runDrain
    _ <- put("student", Student.avi.copy(payment = Payment.CreditCard)).execute
    _ <- batchReadFromStream("student", streamOfStudents)(s => Student.primaryKey(s.email, s.subject)) // implicitly uses schema
           .tap(pair => Console.printLine(s"student=${pair._2}"))
           .runDrain
    _ <- maybeDeleteStudTable //  DynamoDBQuery.deleteTable("student").execute
  } yield ()

    val (flg_doCreate, flg_doDelete) = (true, false)

    def maybeCreateStudTable: ZIO[DynamoDBExecutor, Throwable, Unit] = if (flg_doCreate) {
        createTable("student", KeySchema("email", "subject"), BillingMode.PayPerRequest)(
            AttributeDefinition.attrDefnString("email"),
            AttributeDefinition.attrDefnString("subject")).execute
    } else ZIO.succeed()


    def maybeDeleteStudTable: ZIO[DynamoDBExecutor, Throwable, Unit] = if (flg_doDelete) DynamoDBQuery.deleteTable("student").execute else ZIO.succeed()

  override def run = mkProgram.provide(LocalDynamoDB.layer)
}
