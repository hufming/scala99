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

object P31{
	implicit class IntExt(val x:Int){
		import scala.math
		//method1
		def isPrime:Boolean = x match{
			case 1 => false
			case 2 => true
			case 3 => true
			case _ => (2 to math.sqrt(x).toInt).toList.forall(x % _ != 0)
		}
		//method2
		def isPrime2:Boolean = x match{
			case 1 => false
			case 2 => true
			case 3 => true
			case _ => getPrimes(math.sqrt(x).toInt).forall(x % _ != 0)
		}
		def getPrimes(max:Int)={
				(2 to max).toList.filter(_.isPrime2)			
		}
		//method 3
		var primes=List(2,3)
		def buildPrimes(max:Int):List[Int]={
			if(primes.max<max){
				(primes.max+1 to max).toList.foreach(x=>if(x.isPrime3) primes=x::primes)
			}
			primes.filter(_<=max)
		}
		
		def isPrime3:Boolean = x match{
			case 1 => false
			case 2 => true
			case 3 => true
			case _ => buildPrimes(math.sqrt(x).toInt).forall(x % _ != 0)
		}
	}
	
}


object P32{
	def gcd(a:Int,b:Int):Int={
		if(a < b) return gcd(b,a)
		val c = a % b
		c match{
			case 0 =>b
			case r =>gcd(b,r)
		}
		
		
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

		it("P31--Determine whether a given integer number is prime"){
			import P31._
			2.isPrime should be (true)
			7.isPrime should be (true)
			7.isPrime2 should be (true)
			457.isPrime2 should be (true)
			100.isPrime2 should be (false)
			457.isPrime3 should be (true)
			5153.isPrime3 should be (true)

		}
		
		it("P32--Determine the greatest common divisor of two positive integer numbers"){
			import P32._
			gcd(36,63) should be (9)
		}

		
  	}
}

