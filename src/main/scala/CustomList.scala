import scala.collection.immutable

sealed abstract class CustomList[T] {
  def apply(i: Int): T = applyRec(i, this)

  def length(): Int = lengthRec(this, 0)

  def append(element: T): CustomList[T] = Cons(element, this);

  def appendIf(element: T, predicate: () => Boolean): CustomList[T] = predicate() match {
    case true => append(element);
    case false => this;
  }

  def merge(list2: CustomList[T]): CustomList[T] = mergeRec(this, list2)

  def mergeIf(list2: CustomList[T], predicate: () => Boolean): CustomList[T] = predicate() match {
    case true => mergeRec(this, list2)
    case false => this
  }

  def map[T2](func: T => T2): CustomList[T2] = mapRec(this, func)

  def any(predicate: T => Boolean): Boolean = anyRec(this, predicate)

  def forEach(func: T => Unit): Unit = forEachRec(this, func)

  def find(element: T, comparer: (T, T) => Boolean = (e1: T, e2: T) => e1.equals(e2)): Boolean = findRec(element, this, comparer)

  private def findRec(element: T, list: CustomList[T], comparer: (T, T) => Boolean): Boolean = list match {
    case Nil() => false
    case Cons(head, tail) => comparer(element, head) || findRec(element, tail, comparer)
  def reduce[T2](seed: T2, func: (T,T2) => T2): T2 = reduceRec(this, seed, func)

  def skip(n: Int) = skipRec(this, n)

  private def skipRec(list: CustomList[T], n: Int): CustomList[T] = (n > 0, list) match {
    case (_, Nil()) => Nil()
    case (false, Cons(head, tail)) => Cons(head, tail)
    case (true, Cons(_, tail)) => skipRec(tail, n - 1)
  }

  private def reduceRec[T2](list: CustomList[T], seed: T2, func: (T,T2) => T2): T2 = list match {
    case Nil() => seed
    case Cons(head, tail) => reduceRec(tail, func(head, seed), func)
  }

  private def forEachRec(list: CustomList[T], func: T => Unit): Unit = list match {
    case Nil() => ();
    case Cons(head, tail) => {
      func(head);
      forEachRec(tail, func);
    }
  }

  private def anyRec(list: CustomList[T], predicate: T => Boolean): Boolean = list match {
    case Nil() => false
    case Cons(head, tail) => predicate(head) || anyRec(tail, predicate)
  }

  private def mergeRec(list1: CustomList[T], list2: CustomList[T]): CustomList[T] = list1 match {
    case Nil() => list2
    case Cons(head, tail) => Cons(head, mergeRec(tail, list2))
  }

  private def mapRec[T2](list: CustomList[T], func: T => T2): CustomList[T2] = list match {
    case Nil() => Nil()
    case Cons(head, tail) => Cons(func(head), mapRec(tail, func))
  }

  private def applyRec(i: Int, list: CustomList[T]): T = (list, i == 0) match {
    case (Nil(), true | false) => throw new IndexOutOfBoundsException()
    case (Cons(_,tail), false) => applyRec(i - 1, tail)
    case (Cons(head,_), true) => head
  }

  private def lengthRec(list: CustomList[T], runningCount: Int): Int = list match {
    case Nil() => runningCount
    case Cons(head, tail) => lengthRec(tail, runningCount + 1)
  }
}

object CustomList {
  def map[T1, T2](list: CustomList[T1], func: T1 => T2): CustomList[T2] = list match {
    case Nil() => Nil()
    case Cons(head, tail) => Cons(func(head), map(tail, func))
  }

  def range(start: Int, end: Int): CustomList[Int] = (start > end) match {
    case true => Nil()
    case false => Cons(start, range(start + 1, end))
  }

  def append[T](list: CustomList[T], element: T): CustomList[T] = Cons(element, list);

  def appendIf[T](list: CustomList[T], element: T, predicate: () => Boolean): CustomList[T] = predicate() match {
    case true => append(list, element);
    case false => list;
  }

  def merge[T](list1: CustomList[T], list2: CustomList[T]): CustomList[T] = list1 match {
    case Nil() => list2
    case Cons(head, tail) => Cons(head, merge(tail, list2))
  }

  def foldl[T1, T2](list: CustomList[T1], seed: T2, func: (T1,T2) => T2): T2 = list match {
    case Nil() => seed
    case Cons(head, tail) => foldl(tail, func(head, seed), func)
  }

  def find[T](value: T, list: CustomList[T]): Boolean = list match {
    case Nil() => false
    case Cons(head, tail) => head == value || find(value, tail)
  }

  def forEach[T](list: CustomList[T], func: T => Unit): Unit = list match {
    case Nil() => ();
    case Cons(head, tail) => {
      func(head);
      forEach(tail, func);
    }
  }

  def reverse[T](list: CustomList[T]): CustomList[T] = reverseRec(list, () => Nil())

  def reverseRec[T](list: CustomList[T], cont: () => CustomList[T]): CustomList[T] = list match {
    case Nil() => cont()
    case Cons(head, tail) => reverseRec(list, () => Cons(head, cont()))
  }

  def remove[T](list: CustomList[T], predicate: T => Boolean): CustomList[T] = reverse(removeRec(list, predicate, () => Nil()))

  private def removeRec[T](list: CustomList[T], predicate: T => Boolean, cont: () => CustomList[T]): CustomList[T] = list match {
    case Nil() => cont()
    case Cons(head, tail) => if(predicate(head)) removeRec(tail, predicate, () => cont()) else removeRec(tail, predicate, () => Cons(head, cont()))
  }

  def any[T](list: CustomList[T], predicate: T => Boolean): Boolean = list match {
    case Nil() => false
    case Cons(head, tail) => predicate(head) || any(tail, predicate)
  }

  def all[T](list: CustomList[T], predicate: T => Boolean): Boolean = list match {
    case Nil() => true
    case Cons(head, tail) =>predicate(head) && all(tail, predicate)
  }

  def filter[T](list: CustomList[T], predicate: T => Boolean): CustomList[T] = filterRec(list, predicate, () => Nil())

  private def filterRec[T](list: CustomList[T], predicate: T => Boolean, cont: () => CustomList[T]): CustomList[T] = list match {
    case Nil() => Nil()
    case Cons(head, tail) => if (predicate(head)) {
      Cons(head, filter(tail, predicate))
    } else {
      filter(tail, predicate)
    }
  }

  def fromScalaList[T](scalaList: List[T]): CustomList[T] = scalaList match {
    case ::(head, next) => Cons(head, fromScalaList(next))
    case immutable.Nil => Nil()
  }
}

case class Nil[T]() extends CustomList[T]
case class Cons[T](head: T, tail: CustomList[T]) extends CustomList[T]
