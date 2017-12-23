
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    val left = clamp(x - radius, 0, src.width - 1)
    val right = clamp(x + radius, 0, src.width - 1)
    val up = clamp(y - radius, 0, src.height - 1)
    val down = clamp(y + radius, 0, src.height - 1)
    val tot_neighbor = (right - left + 1) * (down - up + 1)

    // println(s"left = $left, right=$right, up=$up, down=$down, tot_neighbor=$tot_neighbor")

    var r, g, b, a = 0
    var x_neighbor = left
    while (x_neighbor <= right) {
      var y_neighbor = up
      while (y_neighbor <= down) {
        val rgbaNum = src(x_neighbor, y_neighbor)
        // println(s"x_neighbor=$x_neighbor, y_neighbor=$y_neighbor")
        // println(s"r=${red(rgbaNum)}, g=${green(rgbaNum)}, b=${blue(rgbaNum)}, a=${alpha(rgbaNum)}")
        r += red(rgbaNum)
        g += green(rgbaNum)
        b += blue(rgbaNum)
        a += alpha(rgbaNum)
        y_neighbor += 1
      }
      x_neighbor += 1
    }

    rgba(r/tot_neighbor, g/tot_neighbor, b/tot_neighbor, a/tot_neighbor)
  }

}
