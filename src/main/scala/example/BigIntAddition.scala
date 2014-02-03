package example

import scala.util.Random

object BigIntAddition {

  def add(a:String,b:String):String = {

    val x = trimLeadingZeros(a).reverse
    val y = trimLeadingZeros(b).reverse

    if (x.length > y.length)
      addition(x,y)
    else
      addition(y,x)
  }

  private def addition(a:String,b:String):String = {
    
    var answer = a
    var i = 0
    var carry = 0
    val n = a.length
    val m = b.length

    while (i < n) {

      val x = answer.charAt(i).asDigit

      val y = if (i < m)
        b.charAt(i).asDigit
      else
        0

      var next = x + y + carry

      if (next > 9) {
        carry = 1
        next = next - 10
      } else {
        carry = 0
      }

      answer = answer.updated(i,next).mkString

      i = i + 1
    }

    answer.reverse
  }

  def trimLeadingZeros(value:String) = {

    var looking = true
    val leadingDigits = "123456789"

    value.trim().foldLeft("") {
      (str,c) =>

        val next = if (looking && !leadingDigits.contains(c))
          ""
        else {
          looking = false
          c.toString
        }

        str.concat(next)
    }
  }
}
