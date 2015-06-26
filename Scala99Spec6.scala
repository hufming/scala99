import org.scalatest._

object P49{
    def gray(n:Int):List[String]=n match{
        case 1 =>List("0","1")
        case x =>gray(x-1).map("0"+_):::gray(x-1).reverse.map("1"+_)
    }

}



class Scala99Spec6 extends FunSpec with Matchers {

	describe ("Scala 99 Test--Part6"){
		it("P49-- Gray code"){
			import P49._
			val v = gray(3)
			v should be (List("000", "001", "011", "010", "110", "111", "101", "100"))
        }




  	}
}

