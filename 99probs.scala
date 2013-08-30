import scala.annotation.tailrec

object Probs {
  // 01
  def last[A](list: List[A]): A = list match {
    case x :: Nil => x
    case x :: xs => last(xs)
    case Nil => throw new NoSuchElementException
  }

  // 02
  def penultimate[A](list: List[A]): A = list match {
    case x :: xs :: Nil => x
    case x :: xs => penultimate(xs)
    case _ => throw new NoSuchElementException
  }

  // 03
  def nth[A](n: Int, list: List[A]): A = n match {
    case 0 => list.head
    case _ => nth(n-1, list.tail)
  }

  // 04
  def length[A](list: List[A]): Int = {
    def count(acc: Int, xs: List[A]): Int = xs match {
      case Nil => acc
      case y :: ys => count (acc + 1, ys)
    }
    count(0, list)
  }

  // 05
  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def loop(acc: List[A], xs: List[A]): List[A] = xs match {
      case Nil => acc
      case y :: ys => loop(y :: acc, ys)
    }
    loop(List(), list)
  }

  // 06
  def isPalindrome[A](list: List[A]) = list == reverse(list)

  // 07
  def flatten(list: List[_]): List[Any] = list match {
    case Nil => Nil
    case (head: List[_]) :: tail => flatten(head) ::: flatten(tail)
    case head :: tail => head :: flatten(tail)
  }

  // 08
  def compress[A](list: List[A]): List[A] = {
    @tailrec
    def loop(acc: List[A], xs: List[A]): List[A] = xs match {
      case y :: ys => 
        if (!acc.contains(y)) loop(acc :+ y, ys) 
        else loop(acc, ys)
      case Nil => acc
    }
    loop(List(), list)
  }

  // 09
  def pack[A](list: List[A]): List[List[A]] = {
    @tailrec
    def loop[A](xs: List[A], acc: List[List[A]]): List[List[A]] = xs match {
      case elem :: tail => 
        if (acc != Nil && elem == acc.head.head) 
          loop(tail, (elem :: acc.head) :: acc.tail)
        else 
          loop(tail, List(elem) :: acc)
      case Nil => acc
    }
    loop(list, List()).reverse
  }

  // 10
  def encode[A](list: List[A]): List[(Int, A)] = 
    pack(list).map{ elem => (elem.size, elem.head) }

  // 11
  def encodeModified[A](list: List[A]): List[Any] = 
    pack(list).map{ elem: List[A] => if (elem.size > 1) (elem.size, elem.head) else elem.head }

  // 12
  def decode[A](list: List[(Int, A)]): List[A] = 
    list.flatMap( elem => List.fill(elem._1)(elem._2))

  // 13
  def encodeDirect[A](ls: List[A]): List[(Int, A)] = {
    if (ls.isEmpty) Nil
    else {
      val (packed, next) = ls span { _ == ls.head }
      (packed.length, packed.head) :: encodeDirect(next)
    }
  }

  // 14
  def duplicate[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case x :: xs => x :: x :: duplicate(xs)
  }

  // 15
  def duplicateN[A](n: Int, list: List[A]): List[A] = 
    list.flatMap( elem => List.fill(n)(elem) )

  // 16
  def drop[A](n: Int, list: List[A]): List[A] = {
    @tailrec
    def loop[A](counter: Int, xs: List[A], acc: List[A]): List[A] = xs match {
      case Nil => acc
      case head :: tail => {
        if (counter == 1) loop(n, tail, acc)
        else loop(counter-1,tail, acc :+ head)
      }
    }
    loop(n,list, Nil)
  }

  // 17
  def split[A](n: Int, list: List[A]): (List[A], List[A]) = {
    @tailrec
    def loop[A](counter: Int, xs: List[A], tup: (List[A], List[A])): 
    (List[A], List[A]) = xs match {
      case Nil => tup
      case head :: tail => {
        if (counter >= n) 
          loop(counter + 1, tail, (tup._1, tup._2 :+ head))                   
        else 
          loop(counter + 1, tail, (tup._1 :+ head, tup._2))
      }
    }
    loop(0, list, (List(), List()))
  }

  // 18
  def slice[A](i: Int, k: Int, list: List[A]): List[A] = {
    @tailrec
    def loop[A](index: Int, xs: List[A], acc: List[A]): List[A] = xs match {
      case Nil => acc
      case head :: tail => 
        if (index >= i && index < k) 
          loop(index + 1, tail, acc :+ head)
        else 
          loop(index + 1, tail, acc)
    }
    loop(0,list, Nil)
  }

  // 19
  def rotate[A](n: Int, list: List[A]): List[A] = 
    (list drop n) ++ (list take n)

  // 20
  def removeAt[A](index: Int, list: List[A]): (List[A], A) = {
    if (index < 0 || index >= list.size) throw new IndexOutOfBoundsException
    @tailrec
    def loop[A](counter: Int, xs: List[A], pair: (List[A],A)): (List[A],A) = 
    xs match {
      case Nil => pair
      case head :: tail => 
        if (counter == index) (pair._1 ++ tail, head)
        else loop(counter + 1, tail, (pair._1 :+ head, pair._2))
    }
    loop(0, list, (Nil, list.head))
  }

  // 21
  def insertAt[A](elem: A, index: Int, list: List[A]): List[A] = {
    @tailrec
    def loop[A](e: A, counter: Int, xs: List[A], acc: List[A]): List[A] = xs match {
      case head :: tail => 
        if (counter == 0) (acc :+ e) ++ tail
        else loop(e, counter - 1, tail, acc :+ head)
      case Nil => acc :+ e
    }
    loop(elem, index, list, Nil)
  }

  // 22
  def range(start: Int, end: Int): List[Int] = {
    @tailrec
    def loop(s: Int, e: Int, acc: List[Int]): List[Int] = (s, e) match {
      case (x, y) if (x > y) => acc
      case (_, y) => loop(s, e - 1, y :: acc)
    }
    loop(start, end, Nil)
  }
  
  // 23
  def randomSelect[A](n: Int, list: List[A]): List[A] = {
    @tailrec
    def loop[A](counter: Int, xs: List[A], 
      acc: List[A], r: util.Random): List[A] = counter match {
      case 0 => acc
      case _ => 
        val index = r.nextInt(xs.size)
        val ys = removeAt(index, xs)
        loop(counter - 1, ys._1, ys._2 :: acc, r)
    }
    loop(n, list, Nil, util.Random)
  }

  // 24
  def lotto(n: Int, limit: Int): List[Int] = {
    val list = range(1, limit)
    randomSelect(n, list)
  }

  // 25
  def randomPermute[A](list: List[A]): List[A] = {
    randomSelect(list.size, list)
  }
}
