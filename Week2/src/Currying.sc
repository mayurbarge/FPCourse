val list2 = List(1,2,3)
val map1 = list2.map {
  def f(x: Int) : Int = {
    333
  }

  f
}

val map2 = list2.map (
  _ => 2
)

val map3 = list2.map {
  _ => 2
}

//An expression can be a block {}
//
def fun(f: Int => Int)(y: Int) = {

}
// block {} evaluates to a function Int => Int
def partialFun: (Int) => Int = {
  (x: Int) => x+1
}
fun(partialFun)(2)
def fun2: (Int) => Unit = fun(partialFun)
// passing y to function
def fun3(y: Int): (Int) => Unit = fun(partialFun)_
