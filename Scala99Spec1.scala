import org.scalatest._

object P06{
	def isPalindrome[A](l:List[A]):Boolean = l.reverse.equals(l)
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
		
		it("P08--Eliminate consecutive duplicates of list elements"){
			import P08._
			val v = compress(List('a,'a,'a,'a,'b,'c,'c,'a,'a,'d,'e,'e,'e,'e))
			v should be (List('a,'b,'c,'a,'d,'e))
		}

  	}
}

