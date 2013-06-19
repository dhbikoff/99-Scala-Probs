// p01
def last[T](l: List[T]): T = l.last

// p02
def penultimate[T](l: List[T]): T = l(l.size-2)

// p03
def nth[T](index: Int, l: List[T]): T = l(index)

// p04
def length[T](l: List[T]): Int = l.size

// p05
def reverse[T](l: List[T]): List[T] = l.reverse

// p06
def isPalindrome[T](l: List[T]): Boolean = l == l.reverse 

// p07
def flatten(l: List[Any]): List[Any] = l flatMap {
  case xs: List[_] => flatten(xs)
  case x => List(x)
}

// p08
def compress[T](l: List[T]): List[T] = l match {
  case Nil => Nil
  case x :: xs => x :: compress(xs.dropWhile(_ == x))
}

// p09
def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil      => Nil
  case x :: xs1 => 
    val (first, rest) = xs span (y => y == x) 
    first :: pack(rest)
}

// p10
def encode[T](xs: List[T]): List[(Int, T)] = {
  pack(xs) map (ys => (ys.length, ys.head))
}

// p11
def encodeModified[T](xs: List[T]): List[Any] = {
  encode(xs) map { x => if (x._1 == 1) x._2 else x }
  }

// p12 
def decode[T](xs: List[(Int, T)]): List[T] = {
  def fill[T](n: Int, elem: T, counter: Int, acc: List[T]): List[T] = {
    if (counter == n) acc
    else fill(n, elem, counter + 1, elem :: acc)
}
  xs flatMap { x => fill(x._1, x._2, 0, List()) }
  }

// p13
def encodeDirect[T](xs: List[T]): List[(Int, T)] = {
  def packHelper[T](ls: List[T]): List[List[T]] = {
    if (ls.isEmpty) List()  
    else { 
      val matchedElems = ls takeWhile { x => x == ls.head }
      val remaining = ls dropWhile { x => x == ls.head }
      matchedElems :: packHelper(remaining)
      }
    }
  packHelper(xs) map { x => (x.size, x.head) }
  }

// p14
def duplicate[T](xs: List[T]): List[T] = xs match {
  case Nil => Nil
  case h :: rest => h :: List(h) ++ duplicate(rest)
}

// p15
def duplicateN[T](n: Int, xs: List[T]): List[T] = {
  def dupN(n: Int, xs: List[T], count: Int): List[T] = {
    if (xs.isEmpty) xs
    else if (count != n) xs.head :: dupN(n, xs, count+1)
    else dupN(n, xs.tail, 0)
}
  dupN(n, xs, 0)
  }

// p16
def drop[T](n: Int, xs: List[T]): List[T] = {
  def loop(n: Int, ls: List[T], acc: List[T], counter: Int): List[T] = {
    if (ls.isEmpty) acc
    else if (counter != n) {
      val appended = acc :+ ls.head
      loop(n, ls.tail, appended, counter + 1)
}
else loop(n, ls.tail, acc, 0)
    }
  loop(n-1, xs, List(), 0)
  }

// p17
def split[T](n: Int, xs: List[T]): (List[T], List[T]) = {
  xs splitAt n
}

// p18
def slice[T](start: Int, end: Int, xs: List[T]): List[T] = {
  xs slice (start, end)
}

// p19
def rotate[T](n: Int, xs: List[T]): List[T] = {
  if (n >= 0)  xs.drop(n) ++ xs.take(n)
  else xs.drop(xs.size+n) ++ xs.take(xs.size+n)
}

// p20
def removeAt[T](n: Int, xs: List[T]): (List[T], T) = {
  if (xs.isEmpty) throw new NoSuchElementException
  else {
    val split = xs.splitAt(n)
    (split._1 ++ split._2.tail, split._2.head) 
  }
}

// p21
def insertAt[T](elem: T, index: Int, xs: List[T]): List[T] = {
  val begin = xs.splitAt(index)._1
  val end = xs.splitAt(index)._2
  begin.head :: (elem :: end)
}

// p22
def range(begin: Int, end: Int): List[Int] = (begin to end).toList

// p23
def randomSelect[T](n: Int, xs: List[T]): List[T] = {
  val r = new util.Random
  def randLoop(ls: List[T], acc: List[T], counter: Int): List[T] = {
    if (counter < n) {
      val lsRemoved = removeAt(r.nextInt(ls.size), ls)
      randLoop(lsRemoved._1, lsRemoved._2 :: acc, counter + 1)
    } 
    else acc
  }
  randLoop(xs, List(), 0)
}

// p24
def lotto(n: Int, setRange: Int): List[Int] = {
  randomSelect(n, List.range(1,setRange+1))
}

// p25
def randomPermute[T](xs: List[T]): List[T] = {
  val r = util.Random
  def loop(remaining: List[T], acc: List[T]): List[T] = remaining match {
    case Nil => acc
    case _ => {
      val temp = removeAt(r.nextInt(remaining.size), remaining)
      loop(temp._1, temp._2 :: acc)
    }
  }
  loop(xs, List())
}

// p26

// p27

// p28
// a)
def lsort[T](xs: List[List[T]]):List[List[T]] = {
  val sizes = xs map ( x => (x,x.size))
  val sortedSizes = sizes.sortWith( ((x, y) => x._2 < y._2))
  sortedSizes map ( x => x._1)
}
// b

// p31
def isPrime(n: Int): Boolean = {
  (2 to n/2) forall { n % _ != 0 }
}

// p32
def gcd(a: Int, b: Int): Int = {
  if (b == 0) a
  else gcd(b, a % b)
}

// p33
def isCoprimeTo(a: Int, b: Int): Boolean = {
  gcd(a,b) == 1
}

// p34
def totient(n: Int): Int = {
  def loop(xs: List[Int], count: Int): Int = {
    if (xs.isEmpty) count
    else if (isCoprimeTo(xs.head, n)) loop(xs.tail, count+1)
    else loop(xs.tail, count)
  }
  loop((1 to n).toList, 0)
}

// p35

// p36

// p37

// p38

// p39
def listPrimesInRange(r: Range): List[Int] = {
  r.toList filter { isPrime(_) }
}

// p40

// p41

// p46
