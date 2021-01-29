package part2abstractmath

import cats.data.Op

object Functors {

  val aModifiedList   = List(1, 2, 3).map(_ + 1)
  val aModifiedOption = Option(2).map(_ + 1)
  // ...

  // simplified defintion
  trait MyFunctor[F[_]] {

    def map[A, B](initialValue: F[A])(f: A => B): F[B]

  }

  import cats.Functor
  import cats.instances.list._
  val listFunctor        = Functor[List]
  val incrementedNumbers = listFunctor.map(List(1, 2, 3))(_ + 1)

  import cats.instances.option._
  val optionFunctor     = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(1))(_ + 1)

  // generalizing an API
  def do10xList(list: List[Int]): List[Int]         = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  // ...

//  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  // TODO 1: define your own functor for a binary tree
  // hint: define an object which extends Functor[Tree]
  trait Tree[+T]

  object Tree {
    // smart constructors
    def leaf[T](value: T): Tree[T]                                  = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object FunctorTree extends Functor[Tree] {

    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(v)      => Leaf(f(v))
      case b: Branch[A] => Branch(f(b.value), map(b.left)(f), map(b.right)(f))
    }

  }

  // extension method - map
  import cats.syntax.functor._
  val tree            = Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(20)), Tree.leaf(20))
  val incrementedTree = tree.map(_ + 1)

  //TODO 2: create a shorter version of do10x by using extension methods
  def do10x[F[_]: Functor](container: F[Int]): F[Int] = container.map(_ * 10)

  def main(args: Array[String]): Unit = {
    println(do10x(List(1, 2, 3)))
    println(do10x(Option(2)))

    val tree = Tree.branch(
      value = 1,
      left = Tree.branch(
        value = 2,
        left = Tree.leaf(10),
        right = Tree.leaf(20)
      ),
      right = Tree.leaf(30)
    )
    println(do10x(tree))
  }

}
