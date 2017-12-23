package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.annotation.tailrec

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  private val MIN_ARC_DISTANCE: Double = 1.0
  // if the distance to a station is less than 1 km, use temperature of that station.
  private val POWER_PARAMETER: Double = 3.0
  // power index for inverse distance weight
  private val EPSILON: Double = 0.001
  // epsilon value for ill weight (i.e., distance too large causing weight too small)

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {

    // Memory-efficient idw calculation, referenced from:
    // https://github.com/stephenpascoe/scala-capstone/blob/master/observatory/src/main/scala/observatory/Visualization.scala
    def idw(sample: Iterable[(Location, Double)], x: Location, p: Double): Double = {

      @tailrec
      def recIdw(values: Iterator[(Location, Double)], sumVals: Double, sumWeights: Double): Double = {
        values.next match {
          case (xi, ui) => {
            val arc_distance = x.greatCircleDistance(xi)
            if (arc_distance < MIN_ARC_DISTANCE)
              ui
            else {
              val w = 1.0 / math.pow(arc_distance, p)
              if (values.hasNext) recIdw(values, sumVals + w * ui, sumWeights + w)
              else (sumVals + w * ui) / (sumWeights + w)
            }
          }
        }
      }

      recIdw(sample.toIterator, 0.0, 0.0)
    }

    idw(temperatures, location, POWER_PARAMETER)
    // predictTemperatureAlt(temperatures, location)
  }

  def predictTemperatureAlt(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    /*
    val nearbyTemperature = temperatures
      .filter(x => math.abs(x._1.lat - location.lat) <= 10 && math.abs(x._1.lon - location.lon) <= 10)

    val stationDistanceTemperature: Iterable[(Double, Double)] = {
      if (nearbyTemperature.isEmpty) temperatures.map({
        case (stationLocation, stationTemperature) => (location.greatCircleDistance(stationLocation), stationTemperature)
      })
      else nearbyTemperature.map({
        case (stationLocation, stationTemperature) => (location.greatCircleDistance(stationLocation), stationTemperature)
      })
    }
    */

    val stationDistanceTemperature: Iterable[(Double, Double)] =
      temperatures
        .map({
          case (stationLocation, stationTemperature) => (location.greatCircleDistance(stationLocation), stationTemperature)
        })

    val stationDistanceTemperatureMin: (Double, Double) = stationDistanceTemperature.minBy(_._1)

    // if (stationDistanceTemperature(0)._1 < 1) stationDistanceTemperature(0)._2
    if (stationDistanceTemperatureMin._1 < 1) stationDistanceTemperatureMin._2
    else {
      val stationWeightsWTs: Iterable[(Double, Double)] =
        stationDistanceTemperature.map({
          case (stationDistance, stationTemperature) => {
            val stationWeight: Double = 1 / (math.pow(stationDistance, POWER_PARAMETER) + EPSILON)
            val stationWeightedTemperature: Double = stationWeight * stationTemperature
            (stationWeight, stationWeightedTemperature)
          }
        })
      val weightedSum = stationWeightsWTs.foldLeft(0.0, 0.0){ case (sum, next) => (sum._1 + next._1, sum._2 + next._2) }

      weightedSum._2 / weightedSum._1
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {

    val partitionPoints = points.partition(_._1 <= value)
    if (partitionPoints._1.isEmpty) points.minBy(_._1)._2
    else if (partitionPoints._2.isEmpty) points.maxBy(_._1)._2
    else {
      val left = partitionPoints._1.maxBy(_._1)
      val right = partitionPoints._2.minBy(_._1)

      val lColor: Color = left._2
      val rColor: Color = right._2

      val lValue: Double = left._1
      val rValue: Double = right._1

      val lWeight: Double = math.abs((rValue - value) / (rValue - lValue))
      val rWeight: Double = math.abs((value - lValue) / (rValue - lValue))

      Color(
        math.round(lColor.red * lWeight + rColor.red * rWeight).toInt,
        math.round(lColor.green * lWeight + rColor.green * rWeight).toInt,
        math.round(lColor.blue * lWeight + rColor.blue * rWeight).toInt
      )
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */

  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    visualizeParallel(temperatures, colors)
  }

  def visualizeParallel(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {

    // Alternative 1: parallel computation
    val pixelArray = Array.fill[Pixel](180 * 360)(Pixel(0, 0, 0, 255))

    // On evaluation a Scheduler is needed
    import monix.execution.Scheduler.Implicits.global
    // For Task
    import monix.eval._

    val items = for (row <- 90 to -89 by -1; col <- -180 to 179) yield (row, col)
    // The list of all tasks needed for execution
    val tasks = items.map({
      case (row, col) => Task({
        val color = interpolateColor(colors, predictTemperature(temperatures, Location(row, col)))
        val pixel = Pixel(color.red, color.green, color.blue, 255)
        // if (col == 90) println(s"Pixel of ${color.red}, ${color.green}, ${color.blue}")
        pixelArray((90 - row) * 360 + (180 + col)) = pixel
      })
    })

    val par: Int = 180 * 360 / 2
    // Building batches of 10 tasks to execute in parallel:
    val batches = tasks.sliding(par, par).map(b => Task.gather(b))
    // Sequencing batches, then flattening the final result
    val aggregate = Task.sequence(batches)

    import scala.concurrent.Await
    import scala.concurrent.duration.Duration

    // wait until parallel execution finished
    Await.result(aggregate.runAsync, Duration.Inf)

    Image(360, 180, pixelArray)
  }

  // Alternative 2: sequential computation
  def visualizeSequential(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {

    // val pixelArray = Array[Pixel](180 * 360)
    val pixelArray = Array.fill[Pixel](180 * 360)(Pixel(0, 0, 0, 255))

    // from (90, -180) to (-90, 180)
    for (row <- 90 to -89 by -1) {
      for (col <- -180 to 179) {
        val color = interpolateColor(colors, predictTemperature(temperatures, Location(row, col)))
        val index = (90 - row) * 360 + (180 + col)
        import scala.util.Try
        try {
          pixelArray((90 - row) * 360 + (180 + col))
        } catch {
          case _: ArrayIndexOutOfBoundsException =>
            println(s"ArrayIndexOutOfBoundsException happens at: row = ${row} and col = ${col} with index = ${index}")
        }

        pixelArray((90 - row) * 360 + (180 + col)) = Pixel(color.red, color.green, color.blue, 255)
      }
       println(s"current step: $row")
    }
    Image(360, 180, pixelArray)
  }

}

