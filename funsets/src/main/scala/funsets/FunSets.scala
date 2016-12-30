package funsets

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
    * We represent a set by its characteristic function, i.e.
    * its `contains` predicate.
    */
  type Set = Int => Boolean

  /**
    * Indicates whether a set contains a given element.
    */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
    * Returns the set of the one given element.
    */
  def singletonSet(elem: Int): Set = (x: Int) => {x == elem}

  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    */
  def union(s: Set, t: Set): Set = { (x: Int) => if (s(x) || t(x)) true else false }

  /*{
    (x:Int) => {
      val r1 = s(x)
      val r2 = t(x)

      if(r1) {
        if(r2)
          r1
        else r1
      }
      else if(r2) r2
      else false

    }
  }*/


  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    */
  def intersect(s: Set, t: Set): Set = {(x: Int) => if (s(x) && t(x)) true else false}


  /**
    * Returns the difference of the two given sets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff(s: Set, t: Set): Set = {(x: Int) => if (s(x) && !t(x)) true else false}

  /*{
    (x:Int) => {
      val r1 = s(x)
      val r2 = t(x)

      if(r1 && r2) {
        true
      }
      else false

    }
  }*/

  /**
    * Returns the subset of `s` for which `p` holds.
    */
  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => if (s(x) && p(x) == true) true else false


  /**
    * The bounds for `forall` and `exists` are +/- 1000.
    */
  val bound = 1000

  /**
    * Returns whether all bounded integers within `s` satisfy `p`.
    */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a>bound) true
      else if (diff(s,p)(a)) false
      else iter(a+1)
    }
    iter(-bound)

    /*def iter(a: Int): Boolean = {
      // println(a)
      if (a > bound) true
      else if (s(a) && !p(a)) false
      else iter(a + 1)
    }
    iter(-bound)*/

    /*def iter(a: Int)(result: Boolean): Boolean = {

      if (a > bound) result
      else if (s(a)) {
       // println(" -- Matched " + a , result )
        iter(a + 1)(result && p(a))
      }
      else iter(a + 1)(result)
    }
    iter(-bound)(true)*/
  }

  /**
    * Returns whether there exists a bounded integer within `s`
    * that satisfies `p`.
    */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))



  /*{
      def iter(a: Int, result: Boolean): Boolean = {
        // println(a)
        if (a > bound) result
        else if (s(a) && p(a)) true
        else iter(a + 1, result)
      }
      iter(-bound, false)
  }

*/
  /**
    * Returns a set transformed by applying `f` to each element of `s`.
    */
  def map(s: Set, f: Int => Int): Set = {newVal => exists(s, oldVal => {f(oldVal) == newVal})}

    // map(initSet, f)(10) = newSet(10) = initSet(5) = true
    // you need to implement an inverse, which suggests that you need the 'exist' function to test if there is element in initSet that satisfies f(element) in newSet.
/*
    (newVal:Int) => {
      if(exists(s, (x:Int) => {f(x)==newVal}))
        true
      else
      false
    }

  }*/

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
