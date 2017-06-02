package ex1

import scala.collection.mutable.ListBuffer

object Functions {

  // Връща големината на масив (без да ползва data.length!!!)
  def length(data: List[Int]) : Int = {
    def length(data: List[Int], size: Int) : Int = {
      if (data.tail.isEmpty) return size+1
      return length(data.tail, size+1)
    }
    return length(data, 0)
  }

  // Ако cond е true връща onTrue
  def ifelse(cond: Boolean, onTrue: Int, onFalse: Int) : Int = {if(cond) return onTrue else return onFalse}

  // Проверява дали скобите в даден масив от символи са балансирани.
  // Коректно: (a)asda(b)(v) | (((a))) | ()(()асдасд)
  // Грешно: )() | ((д) | ((das) (d)( 
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], c: Int) : Int = {

      if(chars.tail.isEmpty) return if(chars.head.equals('(')) c+1 else if(chars.head.equals(')')) c-1 else c
      return balance(chars.tail, if(chars.head.equals('(')) c+1 else if(chars.head.equals(')')) c-1 else c)
    }
    val b = balance(chars, 0)
    if (b==0) return true
    return false
  }

  def map(chars: List[Char], f: Any) =  ???

  def toUpperCase(chars: List[Char]) : ListBuffer[Char]= {
    def upperCase(char: Char) = {
      char.toUpper
    }
    val newChars = ListBuffer[Char]()
    chars.foreach(f => (newChars += upperCase(f)))
    return newChars
  }

  // Проверява дали съществува елемент отговарящ на f
  def exists(data: List[Int], f: Any) : Boolean = return data.contains(f)

  // Връща масив съдържащ само елементите отговарящи на f
  def filter(data: List[Int], f: Any) : ListBuffer[Int] = {
    val newList = ListBuffer[Int]();
    data.foreach(a => if(a == f) {newList += a} )
    return newList
  }

  // Проверява дали всички елементи отговарят на f
  def forall(data: List[Int], f: Any) : Boolean = {
    val newList = ListBuffer[Int]();
    data.foreach(a => if(a == f) {newList += a} )
    return newList.length == data.length
  }

  // Връща числото от триъгълника на Паскал отговарящо на съответния ред/колона
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) {
      return 1;
    } else {
      return pascal(c - 1, r - 1) + pascal(c, r - 1);
    }
  }

  def main(args: Array[String]): Unit = {
    println(length(List(1,2,3,4,5)))
    println(balance(List('(','a',')','a','s','d','a','(','b',')','(','v',')',' ','|',' ','(','(','(','a',')',')',')',' ','|','(',')','(','(','д',')')))
    val r = List('r','e','t')
    println(toUpperCase(r))
    println(exists(List(1,2,3), 2))
    println(filter(List(1,2,2,3,2,1), 2))
    println(forall(List(1,1,1,1), 1))
    println(pascal(2,4))
  }
}
