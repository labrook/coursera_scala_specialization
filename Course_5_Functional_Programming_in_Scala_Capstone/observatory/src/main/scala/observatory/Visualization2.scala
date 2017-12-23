package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    x: Double,
    y: Double,
    d00: Double,
    d01: Double,
    d10: Double,
    d11: Double
  ): Double = {
    d00 * (1 - x) * (1 - y) + d10 * x * (1 - y) + d01 * (1 - x) * y + d11 * x * y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x X value of the tile to visualize
    * @param y Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: (Int, Int) => Double,
    colors: Iterable[(Double, Color)],
    zoom: Int,
    x: Int,
    y: Int
  ): Image = {
    val pixelArray = Array.fill[Pixel](256 * 256)(Pixel(0, 0, 0, 127))

    for (yOfTile <- 0 until 256) {
      for (xOfTile <- 0 until 256) {
        val zoomX = x * 256 + xOfTile
        val zoomY = y * 256 + yOfTile
        val zoomLocation = Interaction.tileLocation(zoom + 8, zoomX, zoomY)

        val up = math.min(math.ceil(zoomLocation.lat), 90).toInt
        val down = math.max(math.floor(zoomLocation.lat), -89).toInt
        val right = math.min(math.ceil(zoomLocation.lon), 179).toInt
        val left = math.max(math.floor(zoomLocation.lon), -180).toInt

        val d00 = grid(up, left)
        val d10 = grid(up, right)
        val d01 = grid(down, left)
        val d11 = grid(down, right)

        val xBilinear: Double = zoomLocation.lon - left
        val yBilinear: Double = up - zoomLocation.lat

        val zoomLocationTemperature = bilinearInterpolation(xBilinear, yBilinear, d00, d01, d10, d11)
        val zoomLocationColor = Visualization.interpolateColor(colors, zoomLocationTemperature)
        val zoomLocationPixel = Pixel(zoomLocationColor.red, zoomLocationColor.green, zoomLocationColor.blue, 127)
        pixelArray(yOfTile * 256 + xOfTile) = zoomLocationPixel
      }
    }

    Image(256, 256, pixelArray)
  }
}
