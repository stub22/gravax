package fun.gravax.dbin.model

import cats.Monoid

private trait CombinableResultStuff

trait Combiner[X] {
	def empty : X

	def combine(x1 : X, x2 : X) : X
}

trait MonoidBackedResComb[X] extends Combiner[X] {
	protected def getMonoidInst : Monoid[X]

	override def empty: X = getMonoidInst.empty
	override def combine(x1: X, x2: X): X = getMonoidInst.combine(x1, x2)
}

