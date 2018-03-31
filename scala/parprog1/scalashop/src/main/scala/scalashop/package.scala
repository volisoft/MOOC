
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
    val ymin = clamp(y - radius, 0, src.height - 1)
    val xmin = clamp(x - radius, 0, src.width - 1)
    val xmax = clamp(x + radius, 0, src.width - 1)
    val ymax = clamp(y + radius, 0, src.height - 1)

    var count, r, g, b, a = 0

    var yi = ymin

    while (yi <= ymax) {
      var xi = xmin
      while (xi <= xmax) {
        val pixel = src(xi, yi)

        r += red(pixel)
        g += green(pixel)
        b += blue(pixel)
        a += alpha(pixel)

        count += 1
        xi += 1
      }
      yi += 1
    }


    if (count == 0) src(clamp(x, 0, src.width), clamp(y, 0, src.height))
    else {
      rgba(r/count, g/count, b/count, a/count)
    }
  }

  def parallelize(src: Img, dst: Img, radius: Int, numTasks: Int, dimension: Int)(blur: (Img, Img, Int, Int, Int) => Unit): Unit = {
    val maxTasks = Math.min(numTasks, dimension) // cannot split more than number of horizontal/vertical in a rectangle
    val step = dimension / maxTasks
    val remainder = dimension % maxTasks

    val splitPoints = 0 to dimension by step

    val strips =
      if (remainder == 0) splitPoints.zip(splitPoints.tail)
      else splitPoints.zip(splitPoints.tail) :+ (dimension - remainder, dimension)

    strips
      .map { case (start, end) => task(blur(src, dst, start, end, radius)) }
      .foreach(_.join())
  }

}
