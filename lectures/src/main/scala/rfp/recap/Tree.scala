package rfp.recap

sealed trait Tree[+T]

case class Inner[+T](left: Tree[T], right: Tree[T]) extends Tree[T]

case class Leaf[+T](x: T) extends Tree[T]
