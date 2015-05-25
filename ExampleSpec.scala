import collection.mutable.Stack
import org.scalatest._

object P01{
	def last[A](l:List[A]):A = l.last
}

object P02{
	def penultimate[A](l:List[A]):A = l.reverse.tail.head
}

object P03{
	def nth[A](k:Int,l:List[A]):A =l.take(k+1).last 
}

object P04{
	def length[A](l:List[A]):Int = {
		var n = 0
		l.foreach(x=>n += 1)
		n
	}
}
class ExampleSpec extends FunSpec with Matchers {

	describe ("Scala 99 Test"){
		it("P01--Find the last element of a list"){
			import P01._
			val v = last(List(1,1,3,5,8))
			v should be (8)
  		}

  		it("P02--Find the last but one element of a list"){
    			import P02._
			val v = penultimate(List(1,1,3,5,8))
			v should be (5)    
		} 
		
		it("P03--Find the Kth element of a list"){
			import P03._
			val v = nth(2,List(1,1,2,3,5,8))
			v should be (2)
		}
		
		it("P04--Find the number of elements"){
			val v = P04.length(List(1,1,2,3,5,8))
			v should be (6)
		}
  	}
}

