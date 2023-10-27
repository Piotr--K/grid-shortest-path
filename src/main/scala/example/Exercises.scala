package example

object Exercises extends App {
  def searchMax(lst: List[Int]): Int = {
    val el = lst.foldLeft[Int](0)((maxEle, item) => if(maxEle > item)  maxEle else item)
    el
  }

  def sumMax(lst: List[Int]): Int = {
    val e1 = searchMax(lst)
    val e2 = searchMax(lst.filter( _ != e1))
    e1 + e2
  }

  def op(): Int = {
    val res: (Int, Int) => Int = (_ * 2 + _)
    res(3,5)
  }

  def avrg(lst: List[Int]): Double = {
    //READ: interestingly below syntax doesn't work in Scala 2.xx
    //due to some scala compiler limitation, where it can't destructure hierarchical types in lambda functions,
    //it can interpret correctly only single brackets, hence work around below
    // val (sum, count) = lst.foldLeft((0.0, 0))(((accSum, accCount), el) => (accSum + el, accCount +1))
    val (sum, count) = lst.foldLeft((0.0, 0)){
      (acc, el) =>
      val (accSum, accCount) = acc 
      (accSum + el, accCount +1)
    }
    sum / count
  }

  /*
   * To iterate over map, can do via those 2 methods:
   * val x = Map (1 -> "foo", 2 -> "bar")
   *
   * x map { case (k,v) => s"$k is $v" }
   *
   * or
   *
   * for ((k,v) <- x) yield s"$k is $v"
   * 
   * There is also foreach, but never mind
   */
  
  def reverse(lst: List[Int]): List[Int] = {
    lst.foldLeft(List.empty[Int])((acc, el) => el :: acc)
  }

  //return index of the element satysfying some condition, here, index of the 
  //first occurence of the number > 7
  def index(lst: List[Int]): Int = {
    val (cnt, elIndex) = lst.foldLeft(0, 0) {
      (acc, el) =>
        val (count, elInd) = acc
        if (el > 7 && elInd == 0)
          (count + 1, count)
        else
          (count + 1, elInd)
    }
    elIndex 
  }
  
  //Remove duplicate elements from a list using foldLeft and filter
  def removeDups(lst: List[Int]): List[Int] = {
    lst.foldLeft(List.empty[Int])((retLst, el) => if (retLst.filter( _ == el).isEmpty) retLst :+ el else retLst)
  }

  def factorial(el: Int): Int = {
    val r = Range.inclusive(1, el)
    r.foldLeft(1)( _ * _)
  }

  //TODO:
  //Calculate the factorial of a number using foldLeft: n!
  //Calculate the sum of squares of all even numbers in a list using foldLeft and filter
  //Count the number of occurrences of a specific element in a list using foldLeft
  //
  //Project Ideas:
  //https://github.com/karan/Projects
}

// 1/  Create a function that returns the largest pair sum within an unsorted array.
// (1, 3, 7, 2) => 10
//
// 2/  Can assume that the array contains at least two elements.
//
//
// More on type classes:
/******************************
 * Type Class *
 *****************************/
trait Monoid[A] {
   def combine(x1: A, x2: A): A
   def empty: A
}
/******************************
 * Instances *
 *****************************/
object Monoid {
//    TODO: compare it with other known type classes approaches, eg. i think cats does implicit class
  implicit object intSumInstance extends Monoid[Int] {
     def combine(x1: Int, x2: Int) = x1 + x2
     def empty = 0
  }
//
//   //the other way
//   implicit val intSumInstance: Monoid[Int] {
//      new Monoid[Int] {
//        def combine(x1: Int, x2: Int) = x1 + x2
//        def empty = 0
//      }
//   }
//
//   //yet other way is to use implicit def to create instances based on other instances eg. Option[Int] based on Int etc
//   implicit def optionXXX[A](implicit m: TypeClass[A]): TypeClass[Option[A]]=??? - implementation expressed in terms of m
/**********
 * Trick *
 * this allow us to reduce indiraction in creating an interface
 * where we basically duplicate the type class interface
 * and in case type class implements many function
 * is quite bloated
 * Withg the apply method we can see the usage in client below
 * we can skip this indiration
 **********/
  def apply[A](implicit m: Monoid[A]): Monoid[A] = m
}

/****************************
 * Interfaces *
 ***************************/
// Interface Object: 2 ways
object MonoidOps {
  def combineMe1[A](v1: A, v2:A)(implicit m: Monoid[A]): A = m.combine(v1, v2)
  def combineMe2[A: Monoid](v1: A, v2:A): A = implicitly[Monoid[A]].combine(v1, v2)
  
  def combineMe3[A: Monoid](v1: A, v2: A): A = Monoid.apply[A].combine(v1, v2)
  //or even shorter - which really resolves to last method implemented in Client
  //which is quite nice as doesn't require listing each method of the Type Class
  def combineMe4[A: Monoid](v1: A, v2: A): A = Monoid[A].combine(v1, v2)
  //So the choice is really combineMe1 or combineMe4
  //1 is good if TypeClass is relateviley small
  //4 is good for larger TypeClasses
}

// on client side import type class instances and call relevant method
object Client {
  MonoidOps.combineMe1(1, 2)
  MonoidOps.combineMe2(1, 2)
  MonoidOps.combineMe3(1, 2)
  //and instead of using implicitly above - we can use 1 more trick from Cats
  //and add an apply method to Monoid companion object, to be able to call it like:
  Monoid[Int].combine(1, 3)
}
  
//Interface Syntax (extension method), 2 ways
//via implicit class
object MonoidSyntax {
  implicit class MonoidOpsSyntax[A](v1: A) {
    def combineMe(v2: A)(implicit m: Monoid[A]): A = m.combine(v1, v2)
  }
}
//via implicit method - usage Client3 but is not working - TODO
object MonoidSyntax2 {
  implicit def monoidOpsSyntax[A](v1: A) {
    def combineMe(v2: A)(implicit m: Monoid[A]): A = m.combine(v1, v2)
  }
}
//To call it you would import instances, and syntax and then
object Client2 {
  import MonoidSyntax._
  1.combineMe(2)
}

// object Client3 {
//   import MonoidSyntax2._
//   1.combineMe(2)
// }
