package test.gravax.trial

import collection.mutable.Stack
import org.scalatest.flatspec.AnyFlatSpec

import org.scalatest.Tag

object Tag_NoBatch extends Tag("test.gravax.NoBatch")

class StackSpec extends AnyFlatSpec {

	"A Stack" should "pop values in last-in-first-out order" in {
		val stack = new Stack[Int]
		stack.push(1)
		stack.push(2)
		assert(stack.pop() === 2)
		assert(stack.pop() === 1)
	}

	it should "throw NoSuchElementException if an empty stack is popped" in {
		val emptyStack = new Stack[String]
		assertThrows[NoSuchElementException] {
			emptyStack.pop()
		}
	}
}

class WackySpec extends AnyFlatSpec {
	"Strings" should "throw if a negative index is accessed" in {
		val s = "hi"
		assertThrows[IndexOutOfBoundsException] { // Result type: Assertion
			s.charAt(-1)
		}
	}

	"Compiler" should "process types correctly" in {
		println("message 01 BEFORE the first compile check")
		assertDoesNotCompile("val a: String = 1")
		println("message 02 AFTER the first compile check")
		assertTypeError("val a: String = 1")
		println("message 03 AFTER the second compile check")
		assertCompiles("val a: Int = 1")
		println("message 04 AFTER the third compile check")
	}


	it  should "work like in the scalatest docs" taggedAs(Tag_NoBatch) in {
		assert(1 + 2 === 3, "this is a clue from 'assert' about MATH")
		assertResult(5, "this is a clue from 'assertResult' about ADDITION") { 3 + 1 }
	}

}