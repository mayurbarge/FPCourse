package example


object Lists {

  /**
   * This method computes the sum of all elements in the list xs. There are
   * multiple techniques that can be used for implementing this method, and
   * you will learn during the class.
   *
   * For this example assignment you can use the following methods in class
   * `List`:
   *
   *  - `xs.isEmpty: Boolean` returns `true` if the list `xs` is empty
   *  - `xs.head: Int` returns the head element of the list `xs`. If the list
   *    is empty an exception is thrown
   *  - `xs.tail: List[Int]` returns the tail of the list `xs`, i.e. the the
   *    list `xs` without its `head` element
   *
   *  ''Hint:'' instead of writing a `for` or `while` loop, think of a recursive
   *  solution.
   *
   * @param xs A list of natural numbers
   * @return The sum of all elements in `xs`
   */
    def sum(xs: List[Int]): Int = {
    def sumList(acc: Int, list: List[Int]) : Int = {
      list match {
        case Nil => acc
        case elem :: rest => sumList( (acc+ elem), rest)
      }
    }
    sumList(0,  xs)

  }
  
  /**
   * This method returns the largest element in a list of integers. If the
   * list `xs` is empty it throws a `java.util.NoSuchElementException`.
   *
   * You can use the same methods of the class `List` as mentioned above.
   *
   * ''Hint:'' Again, think of a recursive solution instead of using looping
   * constructs. You might need to define an auxiliary method.
   *22
   * @param xs A list of natural numbers
   * @return The largest element in `xs`
   * @throws java.util.NoSuchElementException if `xs` is an empty list
   */
    def max(xs: List[Int]): Int = {
      if(xs.isEmpty) throw new java.util.NoSuchElementException
      def getMax(max: Int, l: List[Int]): Int = {
        (max, l) match {
          case (d, Nil) => d
          case (d, ll) if ll.head > d => getMax(ll.head, ll.tail)
          case _=> getMax(max, l.tail)
        }
      }
      getMax(0, xs)
    }
  }

object Main extends App {
  println(Lists.max(List(1,3,2)))
}

/*
Your overall score for this assignment is 8.67 out of 10.00


The code you submitted did not pass all of our tests: your submission achieved a score of
6.67 out of 8.00 in our tests.

In order to find bugs in your code, we advise to perform the following steps:
 - Take a close look at the test output that you can find below: it should point you to
   the part of your code that has bugs.
 - Run the tests that we provide with the handout on your code.
 - The tests we provide do not test your code in depth: they are very incomplete. In order
   to test more aspects of your code, write your own unit tests.
 - Take another very careful look at the assignment description. Try to find out if you
   misunderstood parts of it. While reading through the assignment, write more tests.

Below you can find a short feedback for every individual test that failed.

Our automated style checker tool could not find any issues with your code. You obtained the maximal
style score of 2.00.

======== LOG OF FAILED TESTS ========
Your solution achieved a testing score of 25 out of 30.

Below you can see a short feedback for every test that failed,
indicating the reason for the test failure and how many points
you lost for each individual test.

Tests that were aborted took too long too complete or crashed the
JVM. Such crashes can arise due to infinite non-terminitaing
loops or recursion (StackOverflowException) or excessive mamory
consumption (OutOfMemoryException).

[Test Description] no max in empty list
[Observed Error] Expected exception java.util.NoSuchElementException to be thrown, but no exception was thrown.
[Lost Points] 5

======== TESTING ENVIRONMENT ========
Limits: memory: 256m,  total time: 850s,  per test case time: 240s

 */