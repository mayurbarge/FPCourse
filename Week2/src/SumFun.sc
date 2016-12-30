def sumOfIntsBetweenAnB(a: Int, b:Int):Int = if(a>b) 0 else a + sumOfIntsBetweenAnB(a+1, b)
def sumOfCubes(a:Int) = a*a*a


case class Search(database: String, value : String)
val bookProfile =  List(Search("bp","1"),Search("bp","4"))
val search =  List(Search("lb","1"),Search("lb","4"),Search("lb","5"),Search("lb","6"))
val sp =  List(Search("sp","1"),Search("sp","4"),Search("sp","5"))

val mergedList = sp:::bookProfile:::search

val result = mergedList.take(4)

val (subs, bplb) = result.partition(_.database == "sp")
val (bp, lb) = bplb.partition(_.database == "bp")
subs
bp
lb

