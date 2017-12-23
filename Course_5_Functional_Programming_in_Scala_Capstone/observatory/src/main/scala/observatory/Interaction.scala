package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    import scala.math._

    // val toURI = new java.net.URI("http://tile.openstreetmap.org/"+zoom+"/"+x+"/"+y+".png")

    Location(
      toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y.toDouble / (1<<zoom))))),
      x.toDouble / (1<<zoom) * 360.0 - 180.0
    )
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    tileParallelMonix(temperatures, colors, zoom, x, y)
  }

  def tileSequential(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {

    val pixelArray = Array.fill[Pixel](256 * 256)(Pixel(0, 0, 0, 127))

    // at ${zoom} level, the world is represented by tiles of math.pow(2, zoom) times math.pow(2, zoom)
    // the (x, y) gives (x*256 to (x+1)*256) times (y*256 to (y+1)*256)
    for (yOfTile <- 0 until 256) {
      for (xOfTile <- 0 until 256) {
        val zoomX = x * 256 + xOfTile
        val zoomY = y * 256 + yOfTile
        val zoomLocation = tileLocation(zoom + 8, zoomX, zoomY)
        val zoomLocationTemperature = Visualization.predictTemperature(temperatures, zoomLocation)
        val zoomLocationColor = Visualization.interpolateColor(colors, zoomLocationTemperature)
        val zoomLocationPixel = Pixel(zoomLocationColor.red, zoomLocationColor.green, zoomLocationColor.blue, 127)
//        System.err.println(s"tile coordinates: (${zoomX}, ${zoomY}) and zoom level: ${zoom + 8} => latlongs: (${zoomLocation.lon}, ${zoomLocation.lat})")
//        System.err.print(s"calculated color: (${zoomLocationColor.red}, ${zoomLocationColor.green}, ${zoomLocationColor.blue})")
        pixelArray(yOfTile * 256 + xOfTile) = zoomLocationPixel
      }
    }
    // val pixelArray = Array.fill[Pixel](256 * 256)(Pixel(0, 0, 0, 127))

    Image(256, 256, pixelArray)
  }

  def tileParallelMonix(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {

    val pixelArray = Array.fill[Pixel](256 * 256)(Pixel(0, 0, 0, 127))

    // On evaluation a Scheduler is needed
    import monix.execution.Scheduler.Implicits.global
    // For Task
    import monix.eval._

    val items = for (subY <- (y*256) until (y+1)*256; subX <- (x*256) until ((x+1)*256)) yield (subX, subY)

    val tasks = items.map({
      case (subX, subY) => Task({
        val subLocation = tileLocation(zoom + 8, subX, subY)
        val subTemperature = Visualization.predictTemperature(temperatures, subLocation)
        val subColor = Visualization.interpolateColor(colors, subTemperature)
        val subPixel = Pixel(subColor.red, subColor.green, subColor.blue, 127)
        pixelArray((subY - y * 256) * 256 + (subX - x * 256)) = subPixel
      })
    })
    val par: Int = 256 * 256 / 2
    // Building batches of ${pars} tasks to execute in parallel:
    val batches = tasks.sliding(par,par).map(b => Task.gather(b))
    // Sequencing batches, then flattening the final result
    val aggregate = Task.sequence(batches)
    // Or no batching:
    // val aggregate = Task.gather(tasks)

    import scala.concurrent.Await
    import scala.concurrent.duration.Duration

    // wait until parallel execution finished
    Await.result(aggregate.runAsync, Duration.Inf)

    Image(256, 256, pixelArray)
  }

  def tileParallel(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val width = 256
    val height = 256

    val pixels = (0 until width * height).par.map(pos => {
      val xPos = pos % width
      val yPos = pos / height
      val zoomLocation = tileLocation(zoom + 8, x * 256 + xPos, y * 256 + yPos)
      val zoomLocationTemperature = Visualization.predictTemperature(temperatures, zoomLocation)
      val zoomLocationColor = Visualization.interpolateColor(colors, zoomLocationTemperature)
      pos -> Pixel(zoomLocationColor.red, zoomLocationColor.blue, zoomLocationColor.green, 127)
    })
      .seq
      .sortBy(_._1)
      .map(_._2)

      Image(width, height, pixels.toArray)
    }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    yearlyData.foreach({
      case (year, data) => {
        for (zoom <- 0 to 3; x <- 0 until math.pow(2, zoom).toInt; y <- 0 until math.pow(2, zoom).toInt)
          generateImage(year, zoom, x, y, data)
      }
    })
  }
}
