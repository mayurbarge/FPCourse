package implicits


object Converter {

  implicit def myInt(implicit str:String) = str.toInt
}
