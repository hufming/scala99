import org.scalatest._

object P06{
	def isPalindrome[A](l:List[A]):Boolean = l.reverse.equals(l)
}

object P07{
	def flatten(l:List[Any]):List[Any] ={
		l.flatMap{
			case ls:List[_]=>flatten(ls)
			case v=>List(v)
		}
	}
}

object P08{
	def compress[A](l:List[A]):List[A] = {
		var ls=List(l.head)
		l.foreach(v=> if(v !=ls.head) ls = v::ls)
		ls.reverse
	}
}

class Scala99Spec1 extends FunSpec with Matchers {

	describe ("Scala 99 Test"){
		it("P06--Find out whether a list is a palindrome"){
			import P06._
			val v = isPalindrome(List(1,2,3,2,1))
			v should be (true)
  		}
		it("P07--Flatten a nested list structure"){
			import P07._
			val v = flatten(List(List(1,1),2,List(3,List(5,8))))
			v should be (List(1,1,2,3,5,8))
		}
		
		it("P08--Eliminate consecutive duplicates of list elements"){
			import P08._
			val v = compress(List('a,'a,'a,'a,'b,'c,'c,'a,'a,'d,'e,'e,'e,'e))
			v should be (List('a,'b,'c,'a,'d,'e))
		}

  	}
}

