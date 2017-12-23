package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {
    (lat, lon) => {
      require(lat <= 90 & lat >= -89 & lon <= 179 & lon >= -180, "Invalid latitude/longitude numbers.")
      Visualization.predictTemperature(temperatures, Location(lat, lon))
    }
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {
    (lat, lon) => {
      val temperaturesOfYears = temperaturess.map(makeGrid(_)(lat, lon))
      temperaturesOfYears.sum / temperaturesOfYears.size
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
    (lat, lon) => {
      makeGrid(temperatures)(lat, lon) - normals(lat, lon)
    }
  }
}

