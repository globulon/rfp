package rfp.recap

import org.scalatest.Inspectors
import org.scalatest.Matchers
import org.scalatest.WordSpec

final class GeneratorSpec extends WordSpec with Matchers with Inspectors with Generators {

  "trees " should {
    "generate tree" in {
      withTree {
        case i @ Inner(left, right) =>
//          println(i)
          left should not be right
        case l @ Leaf(_) =>
//          println(l)
      }

    }
  }

  private def withTree(f: Tree[Int] => Unit) = f(trees.generate)
}
