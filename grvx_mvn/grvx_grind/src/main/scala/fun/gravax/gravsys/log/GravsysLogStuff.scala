package fun.gravax.gravsys.log

private trait GravsysLogStuff

import org.apache.log4j.{BasicConfigurator, Level => Log4JLevel, Logger => Log4JLogger}
import org.slf4j.{Logger, LoggerFactory}

trait FallbackLog4J {
	// TODO:  Reconcile Akka logging with SLf4J logging, Log4J, Jena
	protected lazy val myS4JLogger : Logger = LoggerFactory.getLogger(this.getClass)

	def setupFallbackLogging(log4JLevel: Log4JLevel) : Unit = {
		BasicConfigurator.configure()
		Log4JLogger.getRootLogger.setLevel(log4JLevel)
		printSomeMsgs
	}

	private def printSomeMsgs : Unit = {
		myS4JLogger.warn("TstOntApp init with one warning msg");
		myS4JLogger.warn("Vapid message at INFO level");
		myS4JLogger.debug("Debug message providing help-docs on logging: {}", bcDocTxt)
		myS4JLogger.trace("Wow, even this trace detail is so fine and fluffy!")
	}
	val bcDocTxt = """
					 |What BasicConfigurator.configure() does, from:
					 |https://logging.apache.org/log4j/1.2/apidocs/org/apache/log4j/BasicConfigurator.html
					 |
					 |Add a ConsoleAppender that uses PatternLayout using the PatternLayout.TTCC_CONVERSION_PATTERN
					 |and prints System.out to the root category.
				   """.stripMargin

}

trait LoggingLauncher {
	val flg_setupFallbackLog4J = false // Set to false if log4j.properties is expected, e.g. from Jena.
	val myFallbackLog4JLevel = org.apache.log4j.Level.INFO
	lazy val myFLog4J = new FallbackLog4J {}

	def setup : Unit = {
		if (flg_setupFallbackLog4J) {
			myFLog4J.setupFallbackLogging(myFallbackLog4JLevel)
		}

	}
}
