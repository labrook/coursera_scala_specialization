package observatory

import java.time.LocalDate

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int,
                         stationsFile: String,
                         temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {

    val stationsStream = this.getClass.getResourceAsStream(stationsFile)
    // if(???) sys.error(s"The specified file $stationsFile does not exist!")

    val temperaturesStream = this.getClass.getResourceAsStream(temperaturesFile)
    // if(???) sys.error(s"The specified file $temperaturesFile does not exist!")

    import scala.io.Source

    val latLongLookup = Source.fromInputStream(stationsStream).getLines().map(_.split(","))
      .filter(_.length == 4)
      .filter(row => row(2) != "" && row(3) != "")
      .map(row => (row(0), row(1)) -> Location(row(2).toDouble, row(3).toDouble))
      .toMap

    Source.fromInputStream(temperaturesStream).getLines().map(_.split(","))
      .filter(_.length == 5).filter(_ (4).toDouble < 1000).filter(row => latLongLookup.contains((row(0), row(1))))
      .map(row => {
        val temperature: Double = (row(4).toDouble - 32) * 5 / 9
        val latLong: Location = latLongLookup((row(0), row(1)))
        (LocalDate.of(year, row(2).toInt, row(3).toInt),
          latLong, temperature)
      })
      .toIterable
  }

  // Spark local mode has spark.driver.memory = 1g as default,
  // which does not fit all the data when calling .collect() and gives "GC over limit" error.
  // Can not use spark.config("spark.driver.memory", "4g") to set since JVM has already started when calling it.
  // Only way to overcome it is to run "sbt -mem 4096" from terminal to set JVM memory to 4g at starting sbt.
  /*
  @deprecated
  def locateTemperaturesSpark(year: Int,
                         stationsFile: String,
                         temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {

    import java.io.File

    val stationsResource = try {
      new File(this.getClass.getResource(stationsFile).toURI).getPath
    } catch {
      case _: Exception => sys.error(s"The specified file $stationsFile does not exist!")
    }

    val temperaturesResource = try {
      new File(this.getClass.getResource(temperaturesFile).toURI).getPath
    } catch {
      case _: Exception => sys.error(s"The specified file $temperaturesFile does not exist!")
    }

    import org.apache.spark.sql._
    import org.apache.spark.sql.types._

    val stationsSchema =
      StructType(
        StructField("STN_identifier", StringType, true) ::
          StructField("WBAN_identifier", StringType, true) ::
          StructField("Latitude", DoubleType, true) ::
          StructField("Longitude", DoubleType, true) :: Nil
      )

    val temperaturesSchema =
      StructType(
        StructField("STN_identifier", StringType, true) ::
          StructField("WBAN_identifier", StringType, true) ::
          StructField("Month", StringType, true) ::
          StructField("Day", StringType, true) ::
          StructField("Temperature", DoubleType, true) :: Nil
      )


    val spark: SparkSession =
      SparkSession
        .builder()
        .appName("Extraction")
        .config("spark.master", "local[*]")
        .config("spark.executor.memory", "4g")
        .getOrCreate()

    import spark.implicits._

    val stationsDF: DataFrame = spark.read.format("csv")
      .schema(stationsSchema).load(stationsResource)
      .na.drop(Seq("Latitude", "Longitude"))
    val temperaturesDF: DataFrame = spark.read.format("csv")
      .schema(temperaturesSchema).load(temperaturesResource)
      .withColumn("TemperatureCelsius", ($"Temperature" - 32) * 5 / 9)

    val joinedDF = stationsDF
      .join(temperaturesDF,
        stationsDF("STN_identifier") <=> temperaturesDF("STN_identifier") &&
          stationsDF("WBAN_identifier") <=> temperaturesDF("WBAN_identifier")
      )
      .drop("Temperature")
      .persist()

    val locateTemperaturesData = joinedDF
      .collect()
      .map(row =>
        (LocalDate.of(year, row.getAs[String]("Month").toInt, row.getAs[String]("Day").toInt),
          Location(row.getAs[Double]("Latitude"), row.getAs[Double]("Longitude")),
          row.getAs[Double]("TemperatureCelsius"))
      )

    spark.stop()

    locateTemperaturesData
  }
  */

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    // records.groupBy(_._2).mapValues(x => x.toList.map(_._3).sum / x.size)
    // !!! mapValues is just a wrapper and only creates a view, not the concrete results.
    records.groupBy(_._2).map{ case (k, v) => (k, v.toList.map(_._3).sum / v.size) }.toList

//    // Or trying par - not working, it's view
//    records
//      .par
//      .groupBy(_._2)
//      .mapValues(
//        l => l.foldLeft(0.0)(
//          (t,r) => t + r._3) / l.size
//      )
//      .seq
  }
}

case class temperatureRow
(
  working: String,
  sex: String,
  age: String,
  primaryNeeds: Double,
  work: Double,
  other: Double
)
