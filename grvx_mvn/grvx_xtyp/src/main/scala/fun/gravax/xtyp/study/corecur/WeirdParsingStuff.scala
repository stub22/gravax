package fun.gravax.xtyp.study.corecur

import scala.util.Try

private trait WeirdParsingStuff

object RunWeirdParsingStuff {
	def main(args: Array[String]): Unit = {
		doMunchTests
	}
	def doMunchTests : Unit = {
		val mt = new MunchTester {}
		mt.doMunchTests

		val x = "-25".toInt
		println(s"x=${x}")
		assert (x == -25, "x is not -25")
		// assert (x == 7, "x is not 7")

		val someFloats = new Array[Float](25)
		val otherFloats : Array[Float] = Array[Float](22.5f, 14.0f)
		println(s"otherFloats as List : ${otherFloats.toList}")

	}

}
trait MunchTester {
	val jmt = new JustMunchTokens {}
	def doMunchTests : Unit = {
		oneMunchTest("25+18*3")
		oneMunchTest("")
		oneMunchTest("8+wrong+1")
		oneMunchTest("*+233234***22++")
	}
	def oneMunchTest(inTxt : String) : Unit = {
		val mnchTry: Try[Seq[jmt.MunchedToken]] =  Try { jmt.textToTokens(inTxt) }
		val msg = mnchTry.fold[String](
			t => s"Lexing in=[${inTxt}] failed with exception : ${t} ",
			outToks => s"Lexed in=[${inTxt}] as out=[${outToks}]")
		println(msg)
	}
}
sealed trait Expr
object Exprs {
	case class Sum(left: Expr, right: Expr)

	case class Product(left: Expr, right: Expr)

	case class Number(numv : Numeric[_])

}
case class ParseError(info: String)

case class IncompleteToken()

trait ExprParser {
	// The text must parse to a SINGLE expression (tree), or an error.
	def doParse(inTxt : String) : Either[ParseError,Expr]
}



case class ParseToken[T](tokVal : T, tokTxt : String)





case class FoldFriendlyParseState(alreadyParsed : Seq[ParseToken[_]], partialTok : String)
/*
First legal chars:
	digit 0-9
	plus +
	mult *
Will later extend with:
	. decimal
	( open paren
	) close paren
	text a-zA-Z
	white ' '
 */



trait WeirdParser {

	def parseMathExpr(inTxt : String): Either[ParseError,Expr] = {
		val cseq : Seq[Char] = inTxt.toList.toSeq
		// cseq match
		???
	}

	def maybeMatchNumber(inTxt : String) : Option[ParseToken[Number]]

	def maybeMatchSumInfix(leftExpr : Expr, inTxt : String)


}

// Like good ol lex+yacc, we want to separate the lexical token matching from the parsing into abstract data structure.

trait JustMunchTokens {



	// If we matched a token, just tell us the number of chars matched.
	class Muncher(label : String, val maybeMatch : Function1[String, Option[Int]]) {
		override def toString = label.toUpperCase
	}

	case class MunchedToken(muncher : Muncher, tokenTxt : String)

	val mDigits = new Muncher("digits", inTxt => {
		val leadingDigits = inTxt.takeWhile(c => (c >= '0') && (c <= '9'))
		if (leadingDigits.length > 0) Some(leadingDigits.length) else None
	})
	val mPlus = new Muncher("plus", inTxt => {
		if (inTxt.startsWith("+")) Some(1) else None
	})
	val mMult = new Muncher("mult", inTxt => {
		if (inTxt.startsWith("*")) Some(1) else None
	})

	val ourMunchers = List(mDigits, mPlus, mMult)

	case class UnfoldishLexState(remainingTxt : String)

	// def findMuncher()[String,Int]

	// Implements a context-free lexer to recognize tokens using munch-patterns (greedily?), supplied in priority order.

	def textToTokens(inTxt : String): Seq[MunchedToken] = {
		// def unfold[A, S](init : S)(f : Function1[S, Option[Tuple2[A, S]]]) : CC[A]
		val initState = UnfoldishLexState(inTxt)
		List.unfold[MunchedToken,UnfoldishLexState](initState) { pst => {
			val remTxt: String = pst.remainingTxt
			if (remTxt.length == 0) None else {
				val foundPair_opt: Option[(Muncher, Int)] = huntMuncher(ourMunchers, remTxt)
				foundPair_opt match {
					case None => throw new Exception(s"No muncher matched ${remTxt}")
					case Some((m, cc)) => {
						val mtxt = remTxt.take(cc)
						val outToken = MunchedToken(m, mtxt)
						val nextRemTxt = remTxt.substring(cc)
						val nextState = UnfoldishLexState(nextRemTxt)
						Some(outToken, nextState)
					}
				}
			}
		}}
	}

	def huntMuncher(munchers : Seq[Muncher], inTxt : String) : Option[(Muncher, Int)] = {
		// We want to find the FIRST matching token, and prevent eval of the rest.
		// However, we would also like to pull out the integer result, without calling maybeMatch an extra time.
		// We could accomplish both of those using .fold to hunt through the munchers.
		// Or, we could instead use a LazyList (or a View).
		// If we use .flatMap we will wind up evaluating all the munchers every time, but the code is very easy,
		// and we avoid the second-pass maybe-match.
		// Using iterator.flatMap.nextOption works.
		val mtup01 = huntMuncherUsingFind(munchers, inTxt)
		val mtup02 = huntMuncherUsingFlatMap(munchers, inTxt)
		val mtup03 = huntMuncherUsingIterator(munchers, inTxt)
		assert(mtup01 == mtup02)
		assert(mtup02 == mtup03)
		assert(mtup03 == mtup01)
		mtup03
	}

	def huntMuncherUsingFind(munchers : Seq[Muncher], inTxt : String) : Option[(Muncher, Int)] = {
		// Using .find  properly shortcuts the evaluation, but it constrains us from
		val firstMuncher: Option[Muncher] = munchers.find(m => {
			val mr_opt: Option[Int] = m.maybeMatch(inTxt)
			mr_opt.isDefined
		})
		// Here we pay for a second-pass maybeMatch
		firstMuncher.map(m => (m, m.maybeMatch(inTxt).get))
	}
	def huntMuncherUsingFlatMap(munchers : Seq[Muncher], inTxt : String) : Option[(Muncher, Int)] = {
		// Easy coding, but it checks .maybeMatch on ALL the munchers, even after a successful hit.
		val matches: Seq[(Muncher, Int)] = munchers.flatMap(m => m.maybeMatch(inTxt).map((m, _)))
		matches.headOption
	}
	def huntMuncherUsingIterator(munchers : Seq[Muncher], inTxt : String) : Option[(Muncher, Int)] = {
		val matchesIter: Iterator[(Muncher, Int)] = munchers.iterator.flatMap(m => m.maybeMatch(inTxt).map((m, _)))
		matchesIter.nextOption()
	}

}