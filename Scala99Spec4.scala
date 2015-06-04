import org.scalatest._


object P21{
	def insertAt[A](v:A,pos:Int,l:List[A])={
		l.splitAt(pos) match{
			case (head,tail) => head:::(v::tail)
		}
	}
}

object P22{
	def range(start:Int,end:Int)={
		(start to end).toList
	}
}

object P23{
	import scala.util._
	def RandList(num:Int,Bound:Int) = {
		var v=List[Int]()
		while(v.length < num)
		{
			val t=new Random().nextInt(Bound)
			if(!v.contains(t)) v=t::v
		}
		v
		
	}
	def randomSelect[A](n:Int,l:List[A])= RandList(n,l.length).map(v=>l(v))
}

object P24{
	import P23._
	def lotto(num:Int,max:Int) = RandList(num,max).map(v=>if (v==0) max else v) 
}

object P25{
	import P23._
	def randomPermute[A](l:List[A]) = randomSelect(l.length,l)
}
class Scala99Spec4 extends FunSpec with Matchers {

	describe ("Scala 99 Test--Part4"){
		it("P21-- Insert an element at a given position into a list"){
  			import P21._
			val v = insertAt('new, 1, List('a, 'b, 'c, 'd))
			v should be (List('a, 'new, 'b, 'c, 'd))
		}

		it("P22-- Create a list containing all integers within a given range"){
			import P22._
			val v = range(4, 9)
			v should be (List(4, 5, 6, 7, 8, 9))
		}
		
		
		it("P23-- Create a list containing all integers within a given range"){
			import P23._
			val src =  List('a, 'b, 'c, 'd, 'f, 'g, 'h)
			val v= randomSelect(3, src)
			v.distinct should be (v)
			v.length should be (3)
			v.intersect(src) should be (v)
			
		}

		it("P24-- Lotto: Draw N different random numbers from the set 1..M"){
			import P24._
			val v = lotto(6, 49)
			v.length should be (6)
			v.distinct should be (v)
			v.forall(_ >= 1) should be (true)
			v.forall(_ <= 49) should be (true)
		}
		
		it("P25-- Generate a random permutation of the elements of a list"){
			import P25._
			val src = List('a, 'b, 'c, 'd, 'e, 'f)
			val v = randomPermute(src)
			(v diff src).size should be (0)
		}
  	}
}

