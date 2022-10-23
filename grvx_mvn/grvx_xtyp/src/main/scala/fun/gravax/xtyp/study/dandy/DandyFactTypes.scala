package fun.gravax.xtyp.study.dandy

private trait DandyFactTypes

/***
 * Dandy
 */


trait DandyFact {
	// Three main types of keying information for a dandy obs
	type DWhen
	type DWhere
	type DWhat
	// Numeric types for values in a dandy obs
	type DQty // For general amounts, which may be exact-fractional/decimal/algebraic...or even approx-float?
	type DCnt // For nonnegative integer amounts
}

trait StrngDndyFct extends DandyFact {
	// Simplified model in which all keys are Strings.

}
class LocalStrngDndyFctKey(whn : String, whr : String, wht : String) extends StrngDndyFct

trait ExampleFactMaker {
	def makeObsFact(whn : String, whr : String, wht : String) : DandyFact

	def combineFacts(df1 : DandyFact, df2 : DandyFact) : DandyFact
}

