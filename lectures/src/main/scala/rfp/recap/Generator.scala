package rfp.recap

import scala.language.postfixOps

trait Generator[+T] {
  self =>
  def generate: T

  def map[U](f: T => U): Generator[U] = new Generator[U] {
    def generate: U = f(self generate)
  }

  def flatMap[U](f: T => Generator[U]): Generator[U] = new Generator[U] {
    def generate: U = f(self generate).generate
  }
}

trait Generators {
  protected val integers: Generator[Int] = new Generator[Int] {
    private val rand = new java.util.Random

    def generate: Int = rand.nextInt()
  }

  protected val booleans: Generator[Boolean] = integers map (_ > 0)

  protected def pairs[T, U](g1: Generator[T], g2: Generator[U]): Generator[(T, U)] = for {
    i <- g1
    j <- g2
  } yield (i, j)

  protected def single[T](x: T): Generator[T] = new Generator[T] {
    def generate: T = x
  }

  protected def choose(lo: Int, hi: Int): Generator[Int] = integers map (_ % (hi - lo))

  protected def oneOf[T](xs: T*): Generator[T] = choose(0, xs.length) map (xs(_))

  private def emptyList = single(Nil)

  private def nonEmptyList = for {
    head <- integers
    tail <- lists
  } yield head :: tail

  protected def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    results <- if (isEmpty) emptyList else nonEmptyList
  } yield results

  private def leaf = integers map Leaf.apply

  private def inner = pairs(trees, trees) map { pair => Inner(pair._1, pair._2) }

  protected def trees: Generator[Tree[Int]] = for {
    isLeaf <- booleans
    result <- if (isLeaf) leaf else inner
  } yield result
}

trait TestGenerator {

  private def withGenerated[T](g: Generator[T])(f: T => Unit) = f(g.generate)

  protected def test[T](g: Generator[T], numTimes: Int = 100)(p: T => Boolean) = {
    for (i <- 1 to 100) {
      withGenerated(g) { value =>
        assert(p(g.generate), s"test failed for ${}")
      }
    }
    println(s"test executed ${numTimes}")
  }
}

