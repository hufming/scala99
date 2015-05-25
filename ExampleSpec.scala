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

object P05{
	def reverse[A](l:List[A]):List[A] = {
		var ls:List[A] = Nil
		l.foreach(x=>ls=x::ls)
		ls
	}
	def reverse1[A](l:List[A]):List[A] = {
		(List[A]() /: l){(x,y)=>y::x}
	}
	def reverse2[A](l:List[A]):List[A] = {
		(l :\ List[A]()){(x,y)=>y :+x}
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
		
		it("P05--Reverse a list"){
			val v = P05.reverse(List(1,1,2,3,5,8))
			v should be (List(8,5,3,2,1,1))
			val v1 = P05.reverse1(List(1,1,2,3,5,8))
			v1 should be (List(8,5,3,2,1,1))
			val v2 = P05.reverse2(List(1,1,2,3,5,8))
			v2 should be (List(8,5,3,2,1,1))
		}
  	}
}

