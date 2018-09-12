def REPEAT(command : => Unit) = new {
  def UNTIL(condition: => Boolean): Unit = {
    command

    if (condition) ()
    else UNTIL (condition)
  }
}

var x = 2

REPEAT {
  println("x = " + x)
  x -= 1
} UNTIL(x < 0)