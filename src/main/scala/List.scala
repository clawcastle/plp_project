sealed abstract class List[T];

case class Nil[T]() extends List[T];
case class Cons[T](head: T, tail: List[T]) extends List[T]

def mapC[T1, T2](list: List[T1], func: T1 => T2, cont: List[T1] => List[T2]): List[T2] = list match {
  case Nil() => cont(Nil())
  case Cons(head, tail) => mapC(tail, func, x => cont(Cons(func(head), x)))
}

def foldl[T1, T2](list: List[T1], seed: T2, func: (T1,T2) => T2): T2 = list match {
  case Nil() => seed
  case Cons(head, tail) => foldl(tail, func(head, seed), func)
}

def find[T](value: T, list: List[T]): Boolean = list match {
  case Nil() => false
  case Cons(head, tail) => head == value || find(value, tail)
}

def filter[T](list: List[T], predicate: T => Boolean) = list match {
  case Nil() => Nil()
  case Cons(head, tail) => if predicate
}
