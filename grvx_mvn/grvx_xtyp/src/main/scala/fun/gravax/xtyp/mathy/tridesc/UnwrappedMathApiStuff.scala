package fun.gravax.xtyp.mathy.tridesc

private trait UnwrappedMathApiStuff


trait Adder[Num] {
	def add(n1 : Num, n2 : Num) : Num
}

trait SomeOps {
	def plus[N](n1 : N, n2 : N) : N
}
abstract class SomeOpsSpireImpl() extends SomeOps {

}