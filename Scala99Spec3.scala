import org.scalatest._

object P16{
	def drop[A](n:Int,l:List[A]):List[A] = {
		(1 to l.length).toList.zip(l).filter(v=>v._1 % n != 0).map(_._2)
	}
}

object P17{
	def split[A](n:Int,l:List[A]) = (l.take(n),l.drop(n))
}

class Scala99Spec3 extends FunSpec with Matchers {

	describe ("Scala 99 Test--Part3"){
		it("P16--Drop every Nth element from a list"){
			import P16._
			val v = drop(3,List('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k))
			v should be (List('a,'b,'d,'e,'g,'h,'j,'k))
  		}
		
		it("P17--Split a list into two parts"){
			import P17._
			val v = split(3,List('a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k))
			v should be (List('a,'b,'c),List('d,'e,'f,'g,'h,'i,'j,'k))
		}

  	}
}

