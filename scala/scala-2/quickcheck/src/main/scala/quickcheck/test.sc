val a  = Array(0, 1, 3, 7, 20, 14, 20)
val n = 34

val map = a.map(n - _).zipWithIndex.toMap


val mm = Map[Int, List[Int]]() withDefaultValue List()

a.toStream
  .zipWithIndex
  .filter(t => map.contains(t._1))
  .map {
    case (x, i) => (i, map(x))
  }
  .filter(same)
  .head


def same(pair: (Int, Int)): Boolean = pair._1 != pair._2

a.toStream
  .zipWithIndex
  .foldLeft(mm) {
    case (z, (num, i)) =>
      val complement = n - num
      val indices = z(complement)
      val updated = i :: indices
      z + (complement -> updated)
  }
  .filter{ case (x, is) =>
      is.size == 2
  }
//  .head





