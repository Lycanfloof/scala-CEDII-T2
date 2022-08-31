object T2:
  def checkInList(ls: List[String], vl: String): Boolean =
    ls match
      case Nil => false
      case hd :: tl => (hd == vl) || checkInList(tl, vl)

  def replaceInList(ls: List[Int], n1: Int, n2: Int): List[Int] =
    ls match
      case Nil => Nil
      case hd :: tl => (if (hd == n1) n2 else hd) :: replaceInList(tl, n1, n2)

  def returnOddFromList(ls: List[Int]): List[Int] =
    ls match
      case Nil => Nil
      case hd :: tl => (if (hd % 2 != 0) (hd :: returnOddFromList(tl)) else returnOddFromList(tl))

  def sumMultiplesOfFive(ls: List[Int]): Int =
    ls match
      case Nil => 0
      case hd :: tl => sumMultiplesOfFive(tl) + (if (hd % 5 == 0) hd else 0)