import scala.collection.mutable.ListBuffer


/**
 * https://docs.scala-lang.org/scala3/book/scala-features.html
 */
@main def run(): Unit =

  def imperativeDouble(ints: List[Int]): List[Int] =
    val buffer = new ListBuffer[Int]()
    for i <- ints do
      buffer += i * 2
    buffer.toList

  val oldNumbers = List(1, 2, 3)
  val newNumbers = imperativeDouble(oldNumbers)
  println(newNumbers)
  assert(newNumbers == List(2, 4, 6)) // .equals
  assert(newNumbers.ne(List(2, 4, 6))) // pointer equality
  // ---------------------------------------------------------------------

  val newNumbersFP = oldNumbers.map(_ * 2)
  println(newNumbersFP)
  assert(newNumbersFP == newNumbers)
  // ---------------------------------------------------------------------

  var nums = List(1, 2, 3, 4, 5)
  //  class Person(val name: String, val age: Int)
  class Person(val prefix: String, val name: String, val age: Int) {
    def this(prefix: String, name: String) =
      this(prefix, name, 0)

    def this(name: String, age: Int) =
      this("", name, age)

    override def toString: String = if (prefix.isBlank) name else s"$prefix $name"
  }
  var p = Person("John", 25)

  assert(nums.map(i => i * 2) == nums.map(_ * 2))
  assert(nums.filter(i => i > 1) == nums.filter(_ > 1))
  // ---------------------------------------------------------------------

  trait Animal: // interface++ ??
    def speak(): Unit

  trait HasTail:
    def wagTail(): Unit

  class Dog extends Animal, HasTail:
    def speak(): Unit = println("Woof")

    def wagTail(): Unit = println("⎞⎜⎛  ⎞⎜⎛")

  val doggo = Dog()
  doggo.speak()
  doggo.wagTail()
  // ---------------------------------------------------------------------

  val s = "Hello"
  p = Person("Al", "Pacino")
  nums = (1 to 20).toList
  val sum = nums.reduceLeft(_ + _)
  val y = for i <- nums yield i * 2
  val z = nums
    .filter(_ >= 5)
    .filter(_ < 9)
    .map(x => (x, x * 2))

  def heteroFlatten(list: List[Any]): List[Any] =
    list.flatMap(
      x => x match {
        case l: List[_] => heteroFlatten(l)
        case _ => List(x)
      }
    )

  extension (list: List[Any])
    def heteroFlattenExt: List[Any] =
      list.flatMap {
        case l: List[_] => l.heteroFlattenExt
        case x => List(x)
      }
      
  println(heteroFlatten((sum, y, z).toList))
  println((sum, y, z).toList.heteroFlattenExt)
  println(s"$s $p")
  

