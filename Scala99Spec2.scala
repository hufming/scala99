import org.scalatest._

object P12{
	def decode[A](l:List[(Int,A)]):List[A] = {
		l.map(v=>(1 to v._1).toList.map(_ =>v._2)).flatten
	}
}


class Scala99Spec2 extends FunSpec with Matchers {

	describe ("Scala 99 Test--Part2"){
		it("P12--Decode a run-length encoded list"){
			import P12._
			val v = decode(List((4,'a),(1,'b),(2,'c),(1,'d),(4,'e)))
			v should be (List('a,'a,'a,'a,'b,'c,'c,'d,'e,'e,'e,'e))
  		}

  	}
}

