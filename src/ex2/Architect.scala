package ex2

sealed trait Shape
case class Triangle(a: Int, b: Int, c: Int, h: Int) extends Shape // h represent the height against the longest side of the triangle
case class Rectangle(a: Int, b: Int) extends Shape
case class Trapezoid(a: Int, b: Int, h: Int) extends Shape
case class Cube() extends Shape

class Architect {

  /*
   *  Finds the max element from given list of integers.
   *  The result is wrapped in an Option instance. The Option has two forms:
   *   - None if no element satisfies the search criteria (for example, in case an empty list is provided)
   *   - Some(x), where x if the searched element. In this case, it can be acquired with the method get. Example:
   *     val o: Option[Int] = Some(6)
   *     val n: Int = o.get
   */
  def max(xs: List[Int]): Option[Int] = {

    def myMax(xs: List[Int], max: Int): Int = {
      if (xs.isEmpty) return max;
      if(xs.head > max){
        return myMax(xs.tail, xs.head);
      }
      return myMax(xs.tail, max)
    }

    if(xs.isEmpty) return None;
    return Some(myMax(xs, 0));

  }
  // Determines the type of given triangle: "rectangular", "equilateral", "isosceles", "random"
  def triangleType(t: Triangle): String = {
    if((t.a*t.a+t.b*t.b).equals(t.c*t.c) || (t.a*t.a+t.c*t.c).equals(t.b*t.b) || (t.c*t.c+t.b*t.b).equals(t.a*t.a)){
      return "rectangular"
    }else if(t.a == t.b && t.a == t.c ){
      return "equilateral"
    }else if(t.a == t.b || t.a == t.c || t.b == t.c){
      return "isosceles"
    }
    return "random"
  }

  /*
   * Calculates the area of the provided shape, by using these formulae:
   *  - Rectangular triangle: a * b / 2, where a and b are cathetus
   *  - Any triangle except rectangular: x * h / 2, where x is the largest side of the triangle and h is the opposite height
   *  - Rectangle: a * b, where a and b are both sides
   *  - Trapezoid: (a + b) * h / 2, where a and b are the parallel sides and h is the height between them
   *  - Cube: always return -1
   *  
   *  Hint: for triangles use the max function
   */
  def area(s: Shape): Double = {
    if(s.isInstanceOf[Triangle]){
      val triangle = s.asInstanceOf[Triangle];
      if(triangleType(triangle) == "rectangular"){
        val l = List(triangle.a,triangle.b,triangle.c);
        println(l.drop(this.max(l).getOrElse(0)));

        return (l.head * l.tail.head)/2 ;
      }else{
        return ((this.max(List(triangle.a,triangle.b,triangle.c)).getOrElse(0).asInstanceOf[Double]) * triangle.h)/2;
      }
    }else if(s.isInstanceOf[Rectangle]){
      return s.asInstanceOf[Rectangle].a * s.asInstanceOf[Rectangle].b;
    }else if(s.isInstanceOf[Trapezoid]){
      val trap = s.asInstanceOf[Trapezoid];
      return ((trap.a + trap.b) * trap.h)/2;
    }
    return -1.0;
  }

  /*
   *  Returns the number of rectangular triangles in given list of shapes
   *  
   *  Hint: use the triangleType function
   */
  def findRectangulars(shapes: List[Shape]): Int = {
    def iter(shapes: List[Shape], n: Int): Int = {
      val s = shapes.head
      var a = n;
      if(s.isInstanceOf[Triangle]){
        a+=1
      }
      if (shapes.tail.isEmpty)
        return a
      else
        return iter(shapes.tail,a)
    }
    return iter(shapes, 0)
  }
}
object tt{
  def main(args: Array[String]): Unit = {
    val r = new Architect();
    println(r.getClass.toString)
    println(r.max(List(5,3,7,10,4,2)))

    val tr = new Triangle(3,4,5,5);
    println(r.triangleType(tr))
    println(r.area(new Triangle(3,4,5,5)))
  }
}
