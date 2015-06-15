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

object P33{
	implicit class IntCoprime(val x:Int){
		import P32._
		def isCoprimeTo(a:Int):Boolean = 1 == gcd(x,a)
	}
}

object P34{
	implicit class IntTotient(val x:Int){
		import P33._
		def totient:Int = {
			(1 to x).filter(_.isCoprimeTo(x)).length
		}
	}
}

object P35{
	implicit class IntprimeFactors(val x:Int){
		import P31._		
		def primes:List[Int]= (2 to x).toList.filter(_.isPrime3)
  		def minPrimeFactor:Int = primes.find(x % _ == 0).get
		def primeFactors:List[Int] = {
			if(x.isPrime)
			{
				return List(x)
			}
			x.minPrimeFactor::(x/x.minPrimeFactor).toInt.primeFactors
		}
		def primeFactorMultiplicity:List[(Int,Int)]={
			val primeFactors = x.primeFactors
			primeFactors.distinct.map(v=>(v,primeFactors.count(_==v)))
		}
	}
	
}

object P37{
	import P35._
	def phi(x:Int):Int = {
		x.primeFactorMultiplicity.foldLeft(1.0){(y,x)=>y*(x._1-1)*math.pow(x._1,x._2-1)}.toInt
	}
}

object tools{
	def timestat[A](tips:String)(f: =>A):A={
		val start = System.currentTimeMillis()
		val ret=f
		println(tips+ " cost "+(System.currentTimeMillis()-start)+"ms")
		ret
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
		
		it("P33--Determine whether two positive integer numbers are coprime"){
			import P33._
			35.isCoprimeTo(64) should be (true)
		}

		it("P34--Calculate Euler's totient function"){
			import P34._
			10.totient should be (4)
		}
			
		it("P35--Determine the prime factors of a given positive integer"){
			import P35._
			315.primeFactors should be (List(3,3,5,7))
		}
		
		it("P36--Determine the prime factors of a given positive integer (2)"){
			import P35._
			315.primeFactorMultiplicity should be (List((3,2), (5,1), (7,1)))	
		}

		it("P37-- Calculate Euler's totient function phi(m) (improved)"){
			import P37._
			phi(10) should be (4)
		}
		
		it("P38-- Compare the two methods of calculating Euler's totient function"){
			import P31._
			import P34._
			import P37._
			import tools._
			val v1 = timestat("P34"){10090.totient}
			
			val v2 = timestat("P37"){phi(10090)}
			v1 should be (v2)
			timestat("prime"){49999.isPrime} should be (true)
			timestat("prime2"){49999.isPrime2} should be (true)
			timestat("prime3"){49999.isPrime3} should be (true)
		}		

		
  	}
}

