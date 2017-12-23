package observatory

import java.util.Calendar

object Main extends App {

  val temperatureColors: Iterable[(Double, Color)] =
    Iterable((60, Color(255, 255, 255)),
      (60, Color(255, 255, 255)),
      (32, Color(255, 0, 0)),
      (12, Color(255, 255, 0)),
      (0, Color(0, 255, 255)),
      (-15, Color(0, 0, 255)),
      (-27, Color(255, 0, 255)),
      (-50, Color(33, 0, 107)),
      (-60, Color(0, 0,0))
    )

//  val years = 1975 to 2015

//  years.map(year => {
//    println(s"\nProcessing temperature data of year ${year}")
//
//    val records = Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv")
//    val locationAverageTemperature = Extraction.locationYearlyAverageRecords(records)
//    println(s"\tReading ${records.size} temperature data.")
//
//    println("\tGenerating tile images.")
//
//    (0 to 3).map(zoom => {
//      println(s"\t\tzoom level of $zoom:")
//      for (x <- 0 until (1 << zoom); y <- 0 until (1 << zoom)) {
//        println(s"\t\t\ttile loc = (${x}, ${y})")
//        val tileImage = Interaction.tile(locationAverageTemperature, temperatureColors, zoom, x, y)
//        val fileOutput = new java.io.File(s"target/temperatures/${year}/${zoom}/${x}-${y}.png")
//        fileOutput.getParentFile().mkdirs()
//        tileImage.output(fileOutput)
//      }
//    })
//  })


  val historicalYeas = 1975 to 1989
  println(s"Reading historical yearly temperature data @ ${Calendar.getInstance().getTime()}.")

  val yearlyAverageCombine = historicalYeas.map(year => {
    val records = Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv")
    Extraction.locationYearlyAverageRecords(records)
  })

  println(s"Creating HISTORICAL grid temperature map @ ${Calendar.getInstance().getTime()}.")
  val grid = Manipulation.average(yearlyAverageCombine)
  val latLonTemp = (0 until 180 * 360).par.map(latlon => {
    val lat = latlon / 360 - 89
    val lon = latlon % 360 - 180
    (lat, lon) -> grid(lat, lon)
  }).seq.toMap
  val newGrid = (x: Int, y: Int) => latLonTemp((x,y))

//  println(s"Generating deviation tile image @ ${Calendar.getInstance().getTime()}.")
  val deviationColors: Iterable[(Double, Color)] =
    Iterable((7, Color(0, 0, 0)),
      (4, Color(255, 0, 0)),
      (2, Color(255, 255, 0)),
      (0, Color(255, 255, 255)),
      (-2, Color(0, 255, 255)),
      (-7, Color(0, 0, 255))
    )

  val deviationYears = 1990 to 2015

  deviationYears.map(year => {
    println(s"\nProcessing temperature data of year $year @ ${Calendar.getInstance().getTime()}")

    val records = Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv")
    val locationAverageTemperature = Extraction.locationYearlyAverageRecords(records)
    println(s"Reading ${records.size} temperature data")

    println(s"\tCreating DEVIATION grid temperature map @ ${Calendar.getInstance().getTime()}")
    val deviationGrid = Manipulation.deviation(locationAverageTemperature, newGrid)
    val latLonDeviationTemp = (0 until 180 * 360).par.map(latlon => {
      val lat = latlon / 360 - 89
      val lon = latlon % 360 - 180
      (lat, lon) -> deviationGrid(lat, lon)
    }).seq.toMap
    val newDeviationGrid = (x: Int, y: Int) => latLonDeviationTemp((x, y))

    (0 to 3).map(zoom => {
      println(s"\t\tzoom level of $zoom @ ${Calendar.getInstance().getTime()}:")
      for (x <- 0 until (1 << zoom); y <- 0 until (1 << zoom)) {
        println(s"\t\t\ttile loc = (${x}, ${y}) @ ${Calendar.getInstance().getTime()}")

        val deviationTileImage = Visualization2.visualizeGrid(newDeviationGrid, deviationColors, zoom, x, y)

        val fileOutput = new java.io.File(s"target/deviations/${year}/${zoom}/${x}-${y}.png")
        fileOutput.getParentFile().mkdirs()
        deviationTileImage.output(fileOutput)
      }
    })
  })





  /*
  val year: Int = 1975

  // ==================================================================================================================
  // Test for milestone 1 - "extraction"
  val records1 = Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv")
  // val records2 = Extraction.locateTemperaturesSpark(year, "/stations.csv", s"/$year.csv")

  println(s"records1 have ${records1.size} items")
  // println(s"records2 have ${records2.size} items")

  val locationAverageTemperature1 = Extraction.locationYearlyAverageRecords(records1)
  // val result1 = locationAverageTemperature1.toArray.sortWith(_._2 > _._2)
  // val result2 = Extraction.locationYearlyAverageRecords(records2).toArray.sortWith(_._2 > _._2)

  // println(s"\nResults1 has ${result1.size} entries, and the first 20 entries are:")
  // result1.take(20).foreach(println)

  // println(s"\nResults2 has ${result2.size} entries, and the first 20 entries are:")
  // result2.take(20).foreach(println)

  // ==================================================================================================================
  // Test for milestone 2 - "visualization"

  val temperatureColors: Iterable[(Double, Color)] =
    Iterable((60, Color(255, 255, 255)),
      (60, Color(255, 255, 255)),
      (32, Color(255, 0, 0)),
      (12, Color(255, 255, 0)),
      (0, Color(0, 255, 255)),
      (-15, Color(0, 0, 255)),
      (-27, Color(255, 0, 255)),
      (-50, Color(33, 0, 107)),
      (-60, Color(0, 0,0))
    )

//  println("Start Image Creation:")
//  val myImage = Visualization.visualize(locationAverageTemperature1, temperatureColors)
//  myImage.output(new java.io.File(s"target/par-temperature-image-$year.png"))

  // ==================================================================================================================
  // Test for milestone 3 - "interaction"

  println("Interaction")

  (0 to 3).map(zoom => {
    for (x <- 0 until (1 << zoom); y <- 0 until (1 << zoom)) {
      println(s"tile loc = (${x}, ${y})")
      val tileImage = Interaction.tile(locationAverageTemperature1, temperatureColors, zoom, x, y)
      val fileOutput = new java.io.File(s"target/temperatures/${year}/${zoom}/${x}-${y}.png")
      fileOutput.getParentFile().mkdirs()
      tileImage.output(fileOutput)
    }
  })
  */
}
