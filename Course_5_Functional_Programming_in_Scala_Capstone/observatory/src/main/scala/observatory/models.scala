package observatory

case class Location(lat: Double, lon: Double) {

  /**
    * Handbook of Mathematical Functions
    * M. Abramowitz and I.A. Stegun, Ed.
    * Absolute error <= 6.7e-5
    * http://http.developer.nvidia.com/Cg/acos.html
    */
  private def fastACos(a: Double): Double = {
    val negate = if (a < 0) 1.0 else 0.0
    val x = math.abs(a)
    val y = (((-0.0187293 * x + 0.0742610) * x - 0.2121144) * x + 1.5707288) * math.sqrt(1.0 - x)
    val z = y - 2 * negate * y
    negate * 3.14159265358979 + z
  }

  private val earthRadius: Double = 6371.0

  def greatCircleDistance(that: Location): Double = {

    if ( this.lat == that.lat & this.lon == that.lon) 0.0
    else {
      val phi1: Double = this.lat.toRadians
      val lambda1: Double = this.lon.toRadians
      val phi2: Double = that.lat.toRadians
      val lambda2: Double = that.lon.toRadians

      // math.acos is only defined between -1 to 1.
      // Some calculation precision issue could cause the value to be out of range
      //val deltaSigma: Double = math.acos(
      val deltaSigma: Double = fastACos(
        (math.sin(phi1) * math.sin(phi2) + math.cos(phi1) * math.cos(phi2) * math.cos(math.abs(lambda1 - lambda2)))
          min 1.0 max -1.0
      )
      earthRadius * deltaSigma
    }
  }
}

case class Color(red: Int, green: Int, blue: Int)

