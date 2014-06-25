import scalaz.Plus
import scalaz.syntax.plus._

// These are needed for the examples, not for Retry itself
import scala.util.Random
import scalaz.\/
import scalaz.std.option._
import scalaz.syntax.id._


object Retry {

  def retry[F[_] : Plus, A](tries: Int)(f: => F[A]): F[A] =
    (1 until tries).foldLeft(f){ (result, i) =>
      result <+> f
    }

  def optionExample =
    retry(5){
      if(Random.nextDouble() < 0.5) {
        println("Failing")
        none[Int]
      } else {
        println("Succeeding")
        some(Random.nextInt())
      }
    }

  type Failable[A] = \/[String, A]

  def eitherExample =
    retry(5){
      if(Random.nextDouble() < 0.5) {
        println("Failing")
        "Failed".left : Failable[Int]
      } else {
        println("Succeeding")
        Random.nextInt().right : Failable[Int]
      }
    }
}
