import org.scalatest._

object P12{
	def decode[A](l:List[(Int,A)]):List[A] = {
		l.map(v=>(1 to v._1).toList.map(_ =>v._2)).flatten
	}
}

object P14{
	
	def duplicateN[A](n:Int,l:List[A]):List[A] = {
		l.flatMap(v=>List.make(n,v))
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

