
trait Stack[T] {
  val elements = List[T]()
  def push(element:T):Stack[T]
  def pop:Option[T]
}

class EmptyStack[T]  extends Stack[T] {
  def push(element:T) = (new NonEmptyStack[T]()).push(element)
  def pop = None
}

class NonEmptyStack[T] extends Stack[T] {
  def push(element:T) = {
    elements.
    (new NonEmptyStack()).push(element)}
  def pop =
}
