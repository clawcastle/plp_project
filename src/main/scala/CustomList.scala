import scala.collection.immutable

sealed abstract class CustomList[T]

object CustomList {
  def map[T1, T2](list: CustomList[T1], func: T1 => T2): CustomList[T2] = list match {
    case Nil() => Nil()
    case Cons(head, tail) => Cons(func(head), map(tail, func))
  }

  def foldl[T1, T2](list: CustomList[T1], seed: T2, func: (T1,T2) => T2): T2 = list match {
    case Nil() => seed
    case Cons(head, tail) => foldl(tail, func(head, seed), func)
  }

  def find(value: T, list: CustomList[T]): Boolean = list match {
    case Nil() => false
    case Cons(head, tail) => head == value || find(value, tail)
  }

  def forEach(list: CustomList[T], func: T => Unit): Unit = list match {
    case Nil() => ();
    case Cons(head, tail) => {
      func(head);
      forEach(tail, func);
    }
  }

  def reverse(list: CustomList[T]): CustomList[T] = reverseRec(list, () => Nil())

  def reverseRec(list: CustomList[T], cont: () => CustomList[T]): CustomList[T] = list match {
    case Nil() => cont()
    case Cons(head, tail) => reverseRec(list, () => Cons(head, cont()))
  }

  def remove(list: CustomList[T], predicate: T => Boolean): CustomList[T] = reverse(removeRec(list, predicate, () => Nil()))

  private def removeRec(list: CustomList[T], predicate: T => Boolean, cont: () => CustomList[T]): CustomList[T] = list match {
    case Nil() => cont()
    case Cons(head, tail) => if(predicate(head)) removeRec(tail, predicate, () => cont()) else removeRec(tail, predicate, () => Cons(head, cont()))
  }

  def any(list: CustomList[T], predicate: T => Boolean): Boolean = list match {
    case Nil() => false
    case Cons(head, tail) => predicate(head) || any(tail, predicate)
  }

  def all(list: CustomList[T], predicate: T => Boolean): Boolean = list match {
    case Nil() => true
    case Cons(head, tail) =>predicate(head) && all(tail, predicate)
  }

  def filter(list: CustomList[T], predicate: T => Boolean): CustomList[T] = reverse(filterRec(list, predicate, () => Nil()))

  private def filterRec(list: CustomList[T], predicate: T => Boolean, cont: () => CustomList[T]): CustomList[T] = list match {
    case Nil() => cont()
    case Cons(head, tail) => if (predicate(head)) filterRec(tail, predicate, () => Cons(head, cont())) else filterRec(tail, predicate, () => cont())
  }

  def fromScalaList[T](scalaList: List[T]): CustomList[T] = scalaList.reverse match {
    case immutable.Nil => Nil()
    case ::(head, next) => Cons(head, fromScalaList(next))
  }
}

case class Nil[T]() extends CustomList[T]
case class Cons[T](head: T, tail: CustomList[T]) extends CustomList[T]
