import org.scalatest._


object P26{
	def perm(m:Int,n:Int)={
			((m-n+1) to m).toList.foldLeft(1){ (x,y)=> x*y}
		}
	def combinations[A](k:Int,l:List[A])={
		var ls:List[List[Any]] = l.map(List(_))
		
		if(k>1){
			(1 to k-1).toList.foreach{ x=>
				ls = ls.flatMap{
					v=>(l.diff(v)).map(_::v)}
			}
		}
		
		ls.map(_.toSet).distinct.map(_.toList)
		

	}
	
}




class Scala99Spec5 extends FunSpec with Matchers {

	describe ("Scala 99 Test--Part5"){
		it("P26--1 Caculate permutation"){
			import P26._
			val v = perm(3,3)
			v should be (3*2*1)

			val v1 = perm(4,2)
			v1 should be (4*3)
		}

		it("P26-- Generate the combinations of K distinct objects chosen from the N elements of a list"){
			import P26._
			val ls = List('a, 'b, 'c, 'd, 'e, 'f)
			val v = combinations(1, ls)
			v.length should be (perm(ls.length,1)/perm(1,1))
			v should be (List(List('a),List('b),List('c),List('d),List('e),List('f)))

			val v1 = combinations(3, ls)
			v1.length should be (perm(ls.length,3)/perm(3,3)) 

			
		}

		
  	}
}
