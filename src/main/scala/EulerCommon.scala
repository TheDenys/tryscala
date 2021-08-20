object EulerCommon {
  def isPrime(i: Long): Boolean = i match {
    case 1 => false
    case 2 | 3 => true
    case _ if i % 2 == 0 => false
    case _ if i < 9 => true
    case _ if i % 3 == 0 => false
    case _ => {
      var f = 5
      val sqri = math.sqrt(i)

      while (f <= sqri) {
        if ((i % f == 0) || (i % (f + 2)) == 0) {
          return false
        }
        f = f + 6
      }

      true
    }
  }
}
