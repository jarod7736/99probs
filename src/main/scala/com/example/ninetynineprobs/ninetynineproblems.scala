package com.example.ninetynineprobs

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/**
 * Created with IntelliJ IDEA.
 * User: jarodbelshaw
 * Date: 1/22/13
 * Time: 20:53 PM
 * To change this template use File | Settings | File Templates.
 */
trait Test99 extends FlatSpec with ShouldMatchers

object P01 extends App with Test99 {
  val p01: List[Int] = List(1,1,2,3,5,8)

  def lastInList(l: List[Int]): Int = l.reverse.head
  println("lastInList value: " + lastInList(p01))
  assert(lastInList(p01) === 8)

  def lastBuiltin[A](l: List[A]): A = l.last
  println("lastBuiltin value: " + lastBuiltin(p01))
  assert(lastBuiltin(p01) === 8)

  def lastInListRecurse[A](l: List[A]): A =
    l match {
      case h :: Nil => h
      case h :: tail => lastInListRecurse(tail)
      case Nil => throw new NoSuchElementException
    }

  println("lastInListRecurse value: " + lastInListRecurse(p01))
  assert(lastInListRecurse(p01) === 8)
}

/**
 *
 */
object P02 extends App with Test99 {
  val p02: List[Int] = List(1,1,2,3,5,8)

  def penultimate[A](l: List[A]): A = l.takeRight(2).head

  def penultimate2[A](l: List[A]): A = l(l.length-2)

  println(penultimate2(p02))
  assert(penultimate2(p02) === 5)

  println(penultimate(p02))
  assert(penultimate(p02) === 5)

}

/**
 *
 */
object P03 extends App with Test99 {
  val p03: List[Int] = List(1,1,2,3,5,8)

  def findNthBuiltin[A](l: List[A], n: Int): A =
    if(n >= 0) l(n)
    else throw new NoSuchElementException

  def findNth[A](l: List[A], n: Int): A =
    (n, l) match {
      case (0, h :: _  ) => h
      case (n, _ :: tail) => findNth(tail,n-1)
      case (_, Nil) => throw new NoSuchElementException
    }

  println("find nth with builtin: " + findNthBuiltin(p03, 4))
  println("recursively find nth: " + findNth(p03, 4))

  assert(findNth(p03, 4) === 5)
}

/**
 *
 */
object P04 extends App with Test99 {
  val P04: List[Int] = List(1,1,2,3,5,8)

  def findNumElementsBuiltin[A](l: List[A]): Int = l.length

  def countElements[A](l: List[A]): Int =
    l match {
      case Nil => 0
      case _ :: tail => 1 + countElements(l.tail)
    }

  def tailRecursiveCountElements[A](l: List[A]): Int = {
    def countElements[A](i: Int, l: List[A]): Int =
      l match {
        case Nil => i
        case _ :: tail => countElements(i+1, l.tail)
      }

    countElements(0, l)
  }

  assert(findNumElementsBuiltin(P04) === 6)
  assert(countElements(P04) === 6)
  assert(tailRecursiveCountElements(P04) === 6)
}

object P05 extends App with Test99 {
  val p05: List[Int] = List(1,1,2,3,5,8)

  def reverseRecursive[A](l: List[A]): List[A] =
    l match {
      case Nil => l
      case (x :: xs) => reverseRecursive(xs) ::: List(x)
    }

  println("reverse recursive: " + reverseRecursive(p05))
  assert(reverseRecursive(p05) === List(8,5,3,2,1,1))

}

/**
 *
 */
object P06 extends App with Test99 {
  val p06 = List(1, 2, 3, 2, 1)
  val p06true = List(1,2,2,1)
  val p06false = List(2,1,3,1,9)

  def isPalindrome[A](l: List[A]): Boolean =
    l match {
      case h :: Nil => true
      case h :: _ => if(h == l.last) isPalindrome(l.tail.dropRight(1)) else false
      case Nil => true
    }

  def isPalindromeEasy[A](l: List[A]): Boolean = l == l.reverse

  val res: Boolean = true
  assert(isPalindrome(p06) === res)
  assert(isPalindrome(p06true) === true)
  assert(isPalindrome(p06false) === false)
  assert(isPalindromeEasy(p06) === true)
}

/**
 *
 */
object P07 extends App with Test99 {
  val p07 = List(List(1, 1), 2, List(3, List(5, 8)))

  def flatten(l: List[Any]): List[Any] = l.flatMap { s =>
     s match {
        case s: Int => List(s)
        case s: List[Any] => flatten(s)
        case _ => throw new NoSuchElementException
     }
  }

  println(flatten(p07))
  assert(flatten(p07) === List(1,1,2,3,5,8))

}

/**
 *
 */
object P08 extends App with Test99 {
  val p08 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  val res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

  def compress3(l: List[Any]): List[Any] = {
    l match {
      case Nil => Nil
      case x :: xs => List(x) ::: compress3(xs.dropWhile(_ == x))
    }
  }

  println(compress3(p08))
  assert(compress3(p08) === res0)
}

/**
 *
 */
object P09 extends App with Test99 {
  val p09 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  val res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c),
    List('a, 'a), List('d), List('e, 'e, 'e, 'e))

  def pack[A](l: List[A]): List[List[A]] =
    l match {
      case Nil => Nil
      case x :: xs => List(List(x)++xs.takeWhile(_ == x).toList) ::: pack(xs.dropWhile(_ == x))
    }

  def pack2(l : List[Any]): List[Any] = l.takeWhile(_ == l.head)


  println(pack(p09))
  assert(pack(p09) === res0)
}

/**
 *
 */
object P10 extends App with Test99 {
  val p10 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  val res10: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

  val p10res = P09.pack(p10) map { f => (f.length, f.head) }
  println(p10res)
  assert(p10res === res10)
}

object P11 extends App with Test99 {
  val p11 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  val res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

  val p11res =  P09.pack(p11) map(f =>
    f.length match {
      case 1 => f.head
      case _ => (f.length, f.head)
    })
  println(p11res)
  assert(p11res === res0)
}


/**
 *
 */
object P12 extends App with Test99 {
  val p12: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  val res0: List[Any] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

  def unpack[A](l: List[(Int,A)]): List[A] = {
    l match {
      case Nil => throw new NoSuchElementException
      case x :: Nil => (for (s <- 1 to x._1) yield (x._2)).toList
      case x :: xs => (for (s <- 1 to x._1) yield (x._2)).toList ::: unpack(xs)
    }
  }

  def unpack2[A](l: List[(Int,A)]): List[A] = l.map(x => for (s <- 1 to x._1) yield (x._2)).flatten

  println(unpack(p12))
  println(unpack2(p12))
  assert(unpack(p12) === res0)
}

/**
 *
 */
object P13 extends App with Test99 {
  val p13: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  val res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

  def encodeDirect(l: List[Symbol]): List[(Int, Symbol)] = {
    l match {
      case Nil => Nil
      case x :: xs => List[(Int,Symbol)](returnPair(l)) ::: encodeDirect(xs.dropWhile(_ == x))
    }
  }


  def returnPair(l: List[Symbol]): (Int,Symbol) = {
    val h = l.head
    val x = List(h)++l.tail.takeWhile(_ == h)
    (x.length, h)
  }

  println(encodeDirect(p13))
  assert(encodeDirect(p13) === res0)
}

object P14 extends App with Test99 {
  val p14 = List('a,'b,'c,'c,'d)
  val res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)

  def duplicate(s: Symbol) = Vector(s, s)
  val r = p14.flatMap(duplicate(_))
  println(r)
  assert(r === res0)

}

object P15 extends App with Test99 {
  val p15 = (3, List('a,'b,'c,'c,'d))
  val res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)

  def duplicate(s: Symbol, n: Int) = for { x <- 0 until n } yield s
  val r = p15._2.flatMap(l => duplicate(l,p15._1))
  println(r)
  assert(r === res0)

}

object P16 extends App with Test99 {
  val p16 = (3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  val res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  val l = p16._2
  val c = p16._1

  val r = l.filter(x => l.indexOf(x) != c)

  println(r)
  assert(r === res0)

}









