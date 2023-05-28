package fun.gravax.zaxlam.plain


import org.slf4j.{Logger, LoggerFactory}


trait SomeLoggingStuff {

	protected lazy val myS4JLogger : Logger = LoggerFactory.getLogger(this.getClass)


	def printSomeMsgs : Unit = {
		myS4JLogger.warn("SomeLoggingStuff has a warning for U");
		myS4JLogger.info("Vapid message at INFO level");
		myS4JLogger.debug("Debug message providing help-docs on logging: {}", bcDocTxt)
		myS4JLogger.trace("Wow, even this trace detail is so fine and fluffy!")
	}
	val bcDocTxt = """
						Insert help docs relevant to SLF4J_v1.7 + Log4J_2.17.1 here
					 |
				   """.stripMargin

}