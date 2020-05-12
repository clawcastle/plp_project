import scala.collection.immutable

sealed abstract class CustomList[T] {
  def apply(i: Int): T = applyRec(i, this)

  def length(): Int = lengthRec(this, 0)

  def append(element: T): CustomList[T] = Cons(element, this)

  def appendIf(element: T, predicate: () => Boolean): CustomList[T] = if (predicate()) {
    append(element)
  } else {
    this
  }

  def merge(list2: CustomList[T]): CustomList[T] = mergeRec(this, list2)

  def mergeIf(list2: CustomList[T], predicate: () => Boolean): CustomList[T] = if (predicate()) {
    mergeRec(this, list2)
  } else {
    this
  }

  def map[T2](func: T => T2): CustomList[T2] = mapRec(this, func, x => x)

  def any(predicate: T => Boolean): Boolean = anyRec(this, predicate)

  def forEach(func: T => Unit): Unit = forEachRec(this, func)

  def find(element: T, comparer: (T, T) => Boolean = (e1: T, e2: T) => e1.equals(e2)): Boolean = findRec(element, this, comparer)

  @scala.annotation.tailrec
  private def findRec(element: T, list: CustomList[T], comparer: (T, T) => Boolean): Boolean = list match {
    case Nil() => false
    case Cons(head, tail) => comparer(element, head) || findRec(element, tail, comparer)
  }
  
  def reduce[T2](seed: T2, func: (T,T2) => T2): T2 = reduceRec(this, seed, func)

  def skip(n: Int): CustomList[T] = skipRec(this, n)

  @scala.annotation.tailrec
  private def skipRec(list: CustomList[T], n: Int): CustomList[T] = (n > 0, list) match {
    case (_, Nil()) => Nil()
    case (false, Cons(head, tail)) => Cons(head, tail)
    case (true, Cons(_, tail)) => skipRec(tail, n - 1)
  }

  @scala.annotation.tailrec
  private def reduceRec[T2](list: CustomList[T], seed: T2, func: (T,T2) => T2): T2 = list match {
    case Nil() => seed
    case Cons(head, tail) => reduceRec(tail, func(head, seed), func)
  }

  @scala.annotation.tailrec
  private def forEachRec(list: CustomList[T], func: T => Unit): Unit = list match {
    case Nil() => ();
    case Cons(head, tail) =>
      func(head)
      forEachRec(tail, func);
  }

  @scala.annotation.tailrec
  private def anyRec(list: CustomList[T], predicate: T => Boolean): Boolean = list match {
    case Nil() => false
    case Cons(head, tail) => predicate(head) || anyRec(tail, predicate)
  }

  private def mergeRec(list1: CustomList[T], list2: CustomList[T]): CustomList[T] = list1 match {
    case Nil() => list2
    case Cons(head, tail) => Cons(head, mergeRec(tail, list2))
  }

  @scala.annotation.tailrec
  private def mapRec[T2](list: CustomList[T], func: T => T2, cont: CustomList[T2] => CustomList[T2]): CustomList[T2] = list match {
    case Nil() => cont(Nil())
    case Cons(head, tail) => mapRec(tail, func, x => cont(Cons(func(head), x)))
  }

  @scala.annotation.tailrec
  private def applyRec(i: Int, list: CustomList[T]): T = (list, i == 0) match {
    case (Nil(), true | false) => throw new IndexOutOfBoundsException()
    case (Cons(_,tail), false) => applyRec(i - 1, tail)
    case (Cons(head,_), true) => head
  }

  @scala.annotation.tailrec
  private def lengthRec(list: CustomList[T], runningCount: Int): Int = list match {
    case Nil() => runningCount
    case Cons(_, tail) => lengthRec(tail, runningCount + 1)
  }
}

object CustomList {
  def map[T1, T2](list: CustomList[T1], func: T1 => T2): CustomList[T2] = list match {
    case Nil() => Nil()
    case Cons(head, tail) => Cons(func(head), map(tail, func))
  }

  def range(start: Int, end: Int) : CustomList[Int] = {
    rangeRec(start, end, () => Nil())
  }

  @scala.annotation.tailrec
  private def rangeRec(start: Int, end: Int, cont: () => CustomList[Int]): CustomList[Int] = if (start > end) {
    cont()
  } else {
    rangeRec(start + 1, end, () => Cons(start, cont()))
  }

  def append[T](list: CustomList[T], element: T): CustomList[T] = Cons(element, list)

  def appendIf[T](list: CustomList[T], element: T, predicate: () => Boolean): CustomList[T] = if (predicate()) {
    append(list, element)
  } else {
    list
  }

  def merge[T](list1: CustomList[T], list2: CustomList[T]): CustomList[T] = list1 match {
    case Nil() => list2
    case Cons(head, tail) => Cons(head, merge(tail, list2))
  }

  @scala.annotation.tailrec
  def foldl[T1, T2](list: CustomList[T1], seed: T2, func: (T1,T2) => T2): T2 = list match {
    case Nil() => seed
    case Cons(head, tail) => foldl(tail, func(head, seed), func)
  }

  @scala.annotation.tailrec
  def find[T](value: T, list: CustomList[T]): Boolean = list match {
    case Nil() => false
    case Cons(head, tail) => head == value || find(value, tail)
  }

  @scala.annotation.tailrec
  def forEach[T](list: CustomList[T], func: T => Unit): Unit = list match {
    case Nil() => ();
    case Cons(head, tail) =>
      func(head)
      forEach(tail, func);
  }

  def reverse[T](list: CustomList[T]): CustomList[T] = reverseRec(list, () => Nil())

  @scala.annotation.tailrec
  def reverseRec[T](list: CustomList[T], cont: () => CustomList[T]): CustomList[T] = list match {
    case Nil() => cont()
    case Cons(head, _) => reverseRec(list, () => Cons(head, cont()))
  }

  def remove[T](list: CustomList[T], predicate: T => Boolean): CustomList[T] = reverse(removeRec(list, predicate, () => Nil()))

  @scala.annotation.tailrec
  private def removeRec[T](list: CustomList[T], predicate: T => Boolean, cont: () => CustomList[T]): CustomList[T] = list match {
    case Nil() => cont()
    case Cons(head, tail) => if(predicate(head)) removeRec(tail, predicate, () => cont()) else removeRec(tail, predicate, () => Cons(head, cont()))
  }

  @scala.annotation.tailrec
  def any[T](list: CustomList[T], predicate: T => Boolean): Boolean = list match {
    case Nil() => false
    case Cons(head, tail) => predicate(head) || any(tail, predicate)
  }

  @scala.annotation.tailrec
  def all[T](list: CustomList[T], predicate: T => Boolean): Boolean = list match {
    case Nil() => true
    case Cons(head, tail) =>predicate(head) && all(tail, predicate)
  }

  def filter[T](list: CustomList[T], predicate: T => Boolean): CustomList[T] = filterRec(list, predicate, () => Nil())

  @scala.annotation.tailrec
  private def filterRec[T](list: CustomList[T], predicate: T => Boolean, cont: () => CustomList[T]): CustomList[T] = list match {
    case Nil() => cont()
    case Cons(head, tail) => if (predicate(head)) {
      filterRec(tail, predicate, () => Cons(head, cont()))
    } else {
      filterRec(tail, predicate, () => cont())
    }
  }

  def fromScalaList[T](scalaList: List[T]): CustomList[T] = scalaList match {
    case ::(head, next) => Cons(head, fromScalaList(next))
    case immutable.Nil => Nil()
  }
}

case class Nil[T]() extends CustomList[T]
case class Cons[T](head: T, tail: CustomList[T]) extends CustomList[T]
