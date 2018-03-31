package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val disc = computeDelta(a, b, c)

    def solutions: Set[Double] =
      if (disc() < 0) Set()
      else Set(
        (-b() + Math.sqrt(disc())) / (2 * a()),
        (-b() - Math.sqrt(disc())) / (2 * a())
      )

    Signal(solutions)
  }
}
