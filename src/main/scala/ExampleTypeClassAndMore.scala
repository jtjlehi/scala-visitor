/** Defines how to compare values of type `A` */
trait Ord[A] {
  def compare(a1: A, a2: A): Int
  extension (x: A)
    def <(y: A) = compare(x, y) < 0
    def >(y: A) = compare(x, y) > 0
}

/** Returns the maximum of two values */
def max[A: Ord](a1: A, a2: A): A = if a1 > a2 then a1 else a2

object MaxElement {
  // `using ord: Ord[A]` is the context parameter
  def maxElement[A](as: List[A])(using ord: Ord[A]): A =
    as reduceLeft (max(_, _)(using ord))
  // in this one, the `using ord: Ord[A]` tells max to use `ord` as the argument
  def maxElement1[A](as: List[A])(using ord: Ord[A]): A = as reduceLeft max
  // because we never use `ord` we just remove it (new to Scala 3)
  def maxElement2[A](as: List[A])(using Ord[A]): A = as reduceLeft max
  // we can have Scala generate the context parameter for us
  // this is called a 'context bounds'
  def maxElement3[A: Ord](as: List[A]) = as reduceLeft max
}

// implement a given instance for Ord[Int]
given intOrd: Ord[Int] with {
  def compare(a1: Int, a2: Int): Int =
    if a1 == a2 then 0
    else if a1 > a2 then +1
    else -1
}

// implement a given instance for Ord[List[T]]
// (the separate objects are used so the givens don't collide)

// T must also implement Ord
// the using clause finds a given instance for Ord[T]
object ListOrd1 {
  given listOrd[T](using ord: Ord[T]): Ord[List[T]] with {
    def compare(a1: List[T], a2: List[T]): Int = ???
  }
}
// use a context bounds to find a given instance for Ord[T]
object ListOrd2 {
  given listOrd[T: Ord]: Ord[List[T]] with {
    def compare(a1: List[T], a2: List[T]): Int = ???
  }
}
// you can remove the name for the given instance as well
object ListOrd3 {
  given [T: Ord]: Ord[List[T]] with {
    def compare(a1: List[T], a2: List[T]): Int = ???
  }
}

trait SemiGroup[T] {
  extension (x: T) def combine(y: T): T
}

trait Monoid[T] extends SemiGroup[T] {
  def unit: T
}
// this object makes it so we can just use Monoid[T] instead of summon[Monoid[T]]
object Monoid {
  def apply[T: Monoid] = summon[Monoid[T]]
}

def combineAll[T: Monoid](xs: List[T]): T =
  xs.foldLeft(Monoid[T].unit)(_.combine(_))
