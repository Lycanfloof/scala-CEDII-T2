object T2:
  //I definitely can implement these algorithms using high order functions (at least a few of them).
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
      case hd :: tl =>
        if (hd % 2 != 0) hd :: returnOddFromList(tl)
        else returnOddFromList(tl)

  def sumMultiplesOfFive(ls: List[Int]): Int =
    ls match
      case Nil => 0
      case hd :: tl => (if (hd % 5 == 0) hd else 0) + sumMultiplesOfFive(tl)

  def tailCheckInList(ls: List[String], vl: String): Boolean =
    ls match
      case Nil => false
      case hd :: tl =>
        if (hd == vl) true
        else tailCheckInList(tl, vl)

  def tailReplaceInList(ls: List[Int], n1: Int, n2: Int): List[Int] =
    def tailReplaceInListIt(ls: List[Int], lsf: List[Int], i: Int, n1: Int, n2: Int): List[Int] =
      if (i >= 0)
        tailReplaceInListIt(ls, (if (ls(i) == n1) n2 else ls(i)) :: lsf, i - 1, n1, n2)
      else lsf
    tailReplaceInListIt(ls, Nil, ls.length - 1, n1, n2)

  def tailReturnOddFromList(ls: List[Int]): List[Int] =
    def tailReturnOddFromListIt(ls: List[Int], lsf: List[Int], i: Int): List[Int] =
      if (i >= 0)
        tailReturnOddFromListIt(ls, if (ls(i) % 2 != 0) ls(i) :: lsf else lsf, i - 1)
      else lsf
    tailReturnOddFromListIt(ls, Nil, ls.length - 1)

  def tailSumMultiplesOfFive(ls: List[Int]): Int =
    def tailSumMultiplesOfFiveIt(ls: List[Int], vl: Int): Int =
      ls match
        case Nil => vl
        case hd :: tl => tailSumMultiplesOfFiveIt(tl, if (hd % 5 == 0) vl + hd else vl)
    tailSumMultiplesOfFiveIt(ls, 0)