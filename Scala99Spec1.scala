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

object P09{
	def getduplist[A](l:List[A]):List[A] = {
		val head = l.head
		val tail = l.dropWhile(_ == head)
		l.diff(tail)
	}
	def pack[A](l:List[A]):List[List[A]] = {
		var ls = List[List[A]]()
		var tail = l
		while(tail != Nil)
		{
			val v = getduplist(tail)
			ls = v::ls
			tail = tail.diff(v)
		}
		ls.reverse	
	}
	def spanlist[A](l:List[A]) = {
		val head = l.head
		val tail = l.dropWhile(_ == head)
		(l.diff(tail),tail)
	} 
	def pack1[A](l:List[A]):List[List[A]] = {
		if(l.isEmpty) List[List[A]]()
		else
		{
			val v = spanlist(l)
			if (v._2 == Nil) List(v._1)
			else
				v._1::pack1(v._2)
		}
			
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
		it("P09-1--Get first consecutive duplicates list"){
			import P09._
			val v = getduplist(List(1,1,2,3))
			v should be (List(1,1))
		}
		it("P09-Pack consecutive duplicates element into separate sublists"){
			import P09._
			val v = pack(List('a,'a,'a,'a,'b,'c,'c,'a,'a,'d,'e,'e,'e,'e))
			v should be (List(List('a,'a,'a,'a),List('b),List('c,'c),List('a,'a),List('d),List('e,'e,'e,'e)))
		
			val v1 = pack1(List('a,'a,'a,'a,'b,'c,'c,'a,'a,'d,'e,'e,'e,'e))
			v1 should be (List(List('a,'a,'a,'a),List('b),List('c,'c),List('a,'a),List('d),List('e,'e,'e,'e)))
		}

  	}
}

