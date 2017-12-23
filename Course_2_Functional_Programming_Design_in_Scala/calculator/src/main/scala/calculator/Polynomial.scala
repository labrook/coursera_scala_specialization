package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def root: Set[Double] = {
      if (delta() < 0) Set()
      else {
        Set((-1 * b() + math.sqrt(delta())) / 2 / a(),
          (-1 * b() - math.sqrt(delta())) / 2 / a())
      }
    }
    Signal(root)
  }
}
