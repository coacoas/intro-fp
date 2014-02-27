import scala.annotation.tailrec

object done {
  val s = "some text"                             //> s  : String = some text
  val s1 = s.reverse                              //> s1  : String = txet emos
  val s2 = s.reverse                              //> s2  : String = txet emos
  val s3 = s1 + s2                                //> s3  : String = txet emostxet emos
 
  val b = new StringBuffer("some text")           //> b  : StringBuffer = some text
  val b1 = b.reverse                              //> b1  : StringBuffer = txet emos
  val b2 = b.reverse                              //> b2  : StringBuffer = some text
  val b3 = b1 append b2                           //> b3  : StringBuffer = some textsome text

  type Pred[A] = A => Boolean

  def not[A](p: Pred[A]): Pred[A] =
    a => !p(a)                                    //> not: [A](p: A => Boolean)A => Boolean
  def lift[A](f: (Boolean, Boolean) => Boolean):
    (Pred[A], Pred[A]) => Pred[A] =
      (p1, p2) => a => f(p1(a), p2(a))            //> lift: [A](f: (Boolean, Boolean) => Boolean)(A => Boolean, A => Boolean) => A
                                                  //|  => Boolean
  def and[A] = lift[A](_ && _)                    //> and: [A]=> (A => Boolean, A => Boolean) => A => Boolean
  def or[A] = lift[A](_ || _)                     //> or: [A]=> (A => Boolean, A => Boolean) => A => Boolean

  def fact(n: Int): Int =
    if (n < 1) 1
    else n * fact(n - 1)                          //> fact: (n: Int)Int

  def factTR(n: Int): Int = {
    def loop(acc: Int, remaining: Int): Int =
      if (remaining <= 1) acc
      else loop(remaining * acc, remaining - 1)
    loop(1, n)
  }                                               //> factTR: (n: Int)Int
  
  def sum(ls: List[Int]): Int = ls match {
    case Nil => 0
    case x :: xs => x + sum(xs)
  }                                               //> sum: (ls: List[Int])Int
  
  def sumTR(ls: List[Int]): Int = {
    def loop(acc: Int, remaining: List[Int]): Int =
      remaining match {
        case Nil => acc
        case x :: xs => loop(acc + x, xs)
      }
    loop(0, ls)
  }                                               //> sumTR: (ls: List[Int])Int
  
  def foldr[A,B](ls: List[A], z: B)(f: (A, B) => B): B = ls match {
    case Nil => z
    case x :: xs => f(x, foldr(xs,z)(f))
  }                                               //> foldr: [A, B](ls: List[A], z: B)(f: (A, B) => B)B
  
  def foldl[A,B](ls: List[A], z: B)(f: (B, A) => B): B = {
    def loop(acc: B, remaining: List[A]): B = remaining match {
      case Nil => acc
      case x :: xs => loop(f(acc, x), xs)
    }
    loop(z, ls)
  }                                               //> foldl: [A, B](ls: List[A], z: B)(f: (B, A) => B)B
  
  def sum_foldl(ls: List[Int]) = foldl(ls, 0)(_ + _)
                                                  //> sum_foldl: (ls: List[Int])Int
  
  def sum_foldr(ls: List[Int]) = foldr(ls, 0)(_ + _)
                                                  //> sum_foldr: (ls: List[Int])Int
  
  def product(ls: List[Int]) = foldl(ls, 1)(_ * _)//> product: (ls: List[Int])Int
  
  def concat(ls: List[String]) = foldl(ls, "")(_ + _)
                                                  //> concat: (ls: List[String])String
    def parseInt(s: String): Option[Int] =
    try {
      Some(s.toInt)
    } catch {
      case e: Exception => None
    }                                             //> parseInt: (s: String)Option[Int]
    
  def parseInt2(s: String): Either[Exception, Int] =
    try {
      Right(s.toInt)
    } catch {
      case e: Exception => Left(e)
    }                                             //> parseInt2: (s: String)Either[Exception,Int]
    
  def foldOpt[A,B](o: Option[A], z: B)(f: A => B): B =
    o match {
      case None => z
      case Some(x) => f(x)
    }                                             //> foldOpt: [A, B](o: Option[A], z: B)(f: A => B)B
    
  def foldEither[A,B,C](e: Either[A,B], z: B => C, f: A => C) =
    e match {
      case Left(a) => f(a)
      case Right(b) => z(b)
    }                                             //> foldEither: [A, B, C](e: Either[A,B], z: B => C, f: A => C)C
}