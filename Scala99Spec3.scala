import org.scalatest._

object P16{
	def drop[A](n:Int,l:List[A]):List[A] = {
		(1 to l.length).toList.zip(l).filter(v=>v._1 % n != 0).map(_._2)
	}
}

object P17{
	def split[A](n:Int,l:List[A]) = (l.take(n),l.drop(n))
}

object P18{
	def slice[A](start:Int,end:Int,l:List[A]) = l.drop(start).take(end-start)
}

object P19{
	def rotate[A](n:Int,l:List[A]) = {
		var position = n
		if (n < 0) position = l.length + n
		val v = l.splitAt(position)
		v._2:::v._1
	}
}

object P20{
	def removeAt[A](n:Int,l:List[A]) = {
		val ls = l.zipWithIndex
		(ls.filterNot(v=>v._2 == n).map(v =>v._1),ls.find(v=>v._2==n).get._1)
	}
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

		it("P18--Extract a slice from a list"){
			import P18._
			val v = slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
			v should be (List('d, 'e, 'f, 'g))
		}
		
		it("P19--Rotate a list N places to the left"){
			import P19._
			val v = rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
			v should be (List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
			val v1 = rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
			v1 should be (List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
		}

		it("P20-- Remove the Kth element from a list"){
			import P20._
			val v = removeAt(1, List('a, 'b, 'c, 'd))
			v should be ((List('a, 'c, 'd),'b))
		}

  	}
}

