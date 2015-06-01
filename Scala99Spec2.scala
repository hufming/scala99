import org.scalatest._

object P12{
	def decode[A](l:List[(Int,A)]):List[A] = {
		l.map(v=>(1 to v._1).toList.map(_ =>v._2)).flatten
	}
}

object P13{

	def encodeDirect[A](l:List[A]):List[(Int,A)] = {
		if (l.isEmpty) Nil
		else
		{
			val (head,tail) = l.span(_ == l.head)
			(head.length,l.head)::encodeDirect(tail)
		}
	}
}
object P14{
	
	def duplicateN[A](n:Int,l:List[A]):List[A] = {
		l.flatMap(v=>(1 to n).toList.map(_=> v))
	}
	def duplicate[A](l:List[A]):List[A] = {
		duplicateN(2,l)
	}
}

class Scala99Spec2 extends FunSpec with Matchers {

	describe ("Scala 99 Test--Part2"){
		it("P12--Decode a run-length encoded list"){
			import P12._
			val v = decode(List((4,'a),(1,'b),(2,'c),(1,'d),(4,'e)))
			v should be (List('a,'a,'a,'a,'b,'c,'c,'d,'e,'e,'e,'e))
  		}
		
		it("P13-- Run-length encoding of a list (direct solution)"){
			import P13._
			val v = encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
			v should be (List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
		}

		it("P14--Duplicate the elements of a list"){
			import P14._
			val v = duplicate(List('a,'b,'c,'d))
			v should be (List('a,'a,'b,'b,'c,'c,'d,'d))
		}
	
		it("P15--Duplicate the elements of a list a given number of times"){
			import P14._
			val v = duplicateN(3,List('a,'b,'c,'d))
			v should be (List('a,'a,'a,'b,'b,'b,'c,'c,'c,'d,'d,'d))
		}

  	}
}

