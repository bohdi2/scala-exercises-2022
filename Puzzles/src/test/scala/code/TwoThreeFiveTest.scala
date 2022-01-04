package code

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

object TwoThreeFiveTest {

  def solveBelow(limit: Int, path: List[Int]): Int = {
    solveBelow(1, limit, path)
  }

  def solveBelow(current: Int, limit: Int, path: List[Int]): Int = {
    // If path is empty then stop
    // if current < limit then stop
    // Increment count.
    // solve(current * head, path)
    // solve (current, path.tail)

    if (path.isEmpty) {// || current > limit) {
    0
    }
    else {
      path match {
        case (hd :: tl) if (hd * current <= limit) => {
          solveBelow(hd * current, limit, path) + solveBelow(current, limit, tl) + 1
        }
        case _ => 0
      }
    }
  }

  def findNth(limit: Int): Long = {
    val queue = mutable.SortedSet.empty[Long]
    queue.addOne(1)

    for (i <- 1 until limit) {
      val n = queue.firstKey
      queue.remove(n)
      queue.addOne(2 * n)
      queue.addOne(3 * n)
      queue.addOne(5 * n)
    }

    queue.firstKey
  }

}

class TwoThreeFiveTest extends AnyFunSuite {

  import code.TwoThreeFiveTest._

  assert(solveBelow(15, List(2, 3, 5)) == 10)

  assert(findNth(1) == 1)
  assert(findNth(2) == 2)
  assert(findNth(3) == 3)
  assert(findNth(4) == 4)
  assert(findNth(11) == 15)
  assert(findNth(26) == 60)
  assert(findNth(1500) == 859963392)
  assert(findNth(10000) == 288325195312500000L)

  // 2
  // 2, 2
  // 2, 2, 2
  // 2, 3
  // 3
  // 3, 3

}
