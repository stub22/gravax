package fun.gravax.dbin.tapend

import cats.instances.list._
import cats.syntax.all._

import munit.FunSuite

class MathSuite extends FunSuite {
	test("upper-triangular") {
		val x = Vector(1, 2, 3, 4)
		val xt = x.tails.toVector
		println(s"xt=[${xt}]")
		// xts=[Vector(1, 2, 3, 4), Vector(2, 3, 4), Vector(3, 4), Vector(4), Vector()]
		val xtd = xt.dropRight(1)
		println(s"xtd=[${xtd}]")
		// xtds=[Vector(1, 2, 3, 4), Vector(2, 3, 4), Vector(3, 4), Vector(4)]
		val pairs = xtd.flatMap(subv => {
			val hd = subv.head
			subv.map((hd, _))
		})
		println(s"pairs=[${pairs}]")

	}

	test("cats-cartesian") {
		val x = Vector(1, 2, 3, 4)
		val xx: Seq[(Int, Int)] = (x,x).mapN((_, _))
		val xxs = xx.mkString(", ")
		println(s"xxs=[${xxs}]")
		// xxs=[(1,1), (1,2), (1,3), (1,4), (2,1), (2,2), (2,3), (2,4), (3,1), (3,2), (3,3), (3,4), (4,1), (4,2), (4,3), (4,4)]
	}

}
