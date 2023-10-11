package example

class ExercisesSpec extends BaseSpec {
  "Say hello" should "return hello" in {
    assert(Exercises.sumMax(List(3,5,2,1,9)) == 14)
 }

 "Reverse" should "return list of items in reverse order" in {
    assert(Exercises.reverse(List(3, 5, 7, 9)) == List(9, 7, 5, 3))
 }

 "Index" should "return index of the first elem > 7" in {
   assert(Exercises.index(List(4,3,8,1,9,11,12,5)) == 2)
 }

 "RemoveDups" should "remove list without duplicated elements" in {
   assert(Exercises.removeDups(List(1,2,1,3,7,2,3,3,3,4,8,11)) == List(1,2,3,7,4,8,11))
 }

 "Factorial" should "return factorial of the number - works for n>0" in {
   assert(Exercises.factorial(5) == 2*3*4*5) }
}
