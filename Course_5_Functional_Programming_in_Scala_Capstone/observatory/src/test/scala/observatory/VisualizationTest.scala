package observatory


import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait VisualizationTest extends FunSuite with Checkers {

  test("Great-circle distance halfway around") {

    import org.scalatest.Matchers._

    val Epsilon = 1e-1

    val x = Location(0.0, 0.0)
    val y = Location(0.0, 180.0)

    println(x.greatCircleDistance(y))
    x.greatCircleDistance(y) should be (20015.1 +- Epsilon)
  }

}
