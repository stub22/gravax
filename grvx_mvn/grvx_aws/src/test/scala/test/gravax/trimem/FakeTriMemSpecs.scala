package test.gravax.trimem

import org.apache.arrow.vector.types.pojo.ArrowType;
import org.apache.arrow.vector.types.pojo.Field;
import org.apache.arrow.vector.types.pojo.FieldType;
import org.apache.arrow.vector.types.pojo.Schema;

import org.scalatest.flatspec.AnyFlatSpec

import org.scalatest.Tag

private trait FakeTriMemSpecs

class FirstFakeTriMemSpec() extends AnyFlatSpec {
	val myTMO = new TriMemOps {}
	"A TriMem" should "do good stuff" in {
		myTMO.go
	}

}
trait TriMemOps {
	def go : Unit = {
		println("Arggh println really?")
	}
}