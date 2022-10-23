package test.gravax.trial

import org.scalatest.flatspec.AnyFlatSpec

private trait FakeLazySpecs

class FirstFakeLazySpec extends AnyFlatSpec {
	"Lazy Examples" should "do lazy example stuff" in {
		val lec = new LazyExampleCode {}
		lec.lazyFactEx
	}
}
// @deprecated(message = "Use LazyList instead of Stream", since = "2.13.0")
// https://stackoverflow.com/questions/60128207/whats-the-difference-between-lazylist-and-stream-in-scala
// https://www.scala-lang.org/api/current/scala/collection/immutable/LazyList.html
trait LazyExampleCode {
	def lazyFactEx : Unit = {
		// The
		// forward reference to value fact defined on line 18 extends over definition of value fact
		// https://stackoverflow.com/questions/66629113/scala-compilation-issue-forward-reference-extends-over-definition-of-value
		// val fact: LazyList[BigInt] = 1 #:: fact.zipWithIndex.map{case (p,x)=>p*(x+1)}
		lazy val factLL: LazyList[BigInt] = 1 #:: factLL.zipWithIndex.map{case (p,x)=>p*(x+1)}
		val first10 = factLL.take(10)
		println(s"first10=${first10}")  // first10=LazyList(<not computed>)
		val f24 = factLL(24)
		println(s"f24=${f24}")	// f24=620448401733239439360000
		println(s"AGAIN first10=${first10}")
		println(s"factorial-lazy-list=${factLL}")
		val retake = factLL.take(5)
		println(s"retake=${retake}")
		val fv = factLL.view
		val rv = retake.view
		println(s"rv=${rv}, fv=${fv}")
		// val fvl = fv.toList // An exception or error caused a run to abort: Java heap space
		// println(s"fvl=${fvl}")
		val rvl = rv.toList
		println(s"rvl=${rvl}")
		println(s"AGAIN rv=${rv}, retake=${retake}")
	}
}