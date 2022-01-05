package core

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object HeapTests {

  object Heap {
    def apply(elems: Int*): Heap = new Heap(elems)

  }


  class Heap(private val originalData: Seq[Int]) {
    private val array = ArrayBuffer(originalData: _*)

    def size: Int = array.size
    def apply(index: Int): Int = array(index)
    def top: Int = apply(0)
    def isRoot(index: Int): Boolean = index == 0
    def calcParentIndex(childIndex: Int): Int = (childIndex - 1) / 2
    def calcLeftChild(parentIndex: Int): Int = parentIndex*2 + 1
    def calcRightChild(parentIndex: Int): Int = parentIndex*2 + 2


    def update(index: Int, value: Int): Unit = array.update(index, value)

    def insert(value: Int): Heap = {
      val childIndex = size
      array.append(value)

      val holeIndex = siftUp(childIndex)
      array.update(holeIndex, value)
      this
    }

    @tailrec
    private def siftUp(childIndex: Int): Int = {
      if (isRoot(childIndex)) {
        return childIndex
      }

      val parentIndex = calcParentIndex(childIndex)
      if (array(parentIndex) >= array(childIndex)) {
        childIndex
      }
      else if (swap(parentIndex, childIndex))
        siftUp(parentIndex)
      else
        childIndex
    }

    def removeTop(): Heap = {
      update(0, array.remove(size - 1))
      array.trimToSize()
      siftDown(0)
      this
    }

    private def siftDown(parentIndex: Int): Heap = {
      val leftChildIndex = calcLeftChild(parentIndex)
      val rightChildIndex = calcRightChild(parentIndex)
      //println(s"CJH siftDown ${this}, ${x(parentIndex)} -> ${x(leftChildIndex)}, ${x(rightChildIndex)}")

      if (size - 1 == parentIndex) {
      }
      else if (size - 1 == leftChildIndex) {
        swap(parentIndex, leftChildIndex)
      }
      else if (array(leftChildIndex) < array(rightChildIndex)) {
        if (swap(parentIndex, rightChildIndex))
          siftDown(rightChildIndex)
      }
      else {
        if (swap(parentIndex, leftChildIndex))
          siftDown(leftChildIndex)
      }
      this
    }

    private def prettyIndex(n: Int): String = {
      if (n >= size) s"$n/error" else s"$n/${array(n)}"
    }


    def swap(parentIndex: Int, childIndex: Int): Boolean = {
      //println(s"  Swapping ${x(parentIndex)} - ${x(childIndex)}")
      if (array(parentIndex) < array(childIndex)) {
        val temp = array(parentIndex)
        array.update(parentIndex, array(childIndex))
        array.update(childIndex, temp)
        true
      }
      else {
        false
      }
    }

    def isValid: Boolean = {
      for (i <- array.length - 1 until 0 by -1) {
        val parent = (i - 1) / 2
        if (array(i) > array(parent)) {
          return false
        }
      }
      true
    }


    override def toString = s"Heap($array)"

    def canEqual(other: Any): Boolean = other.isInstanceOf[Heap]

    override def equals(other: Any): Boolean = other match {
      case that: Heap =>
        (that canEqual this) &&
          array == that.array
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(array)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }

}

class HeapTests extends AnyFunSuite {

  import core.HeapTests._

  test("heap validation") {
    assert(Heap(100, 50, 40, 25, 24, 20, 19).isValid)
    assert(Heap(100, 50, 40, 40, 24, 20, 19).isValid)
    assert(!Heap(100, 50, 40, 40, 24, 50, 19).isValid)
    assert(!Heap(50, 40, 3, 30, 1, 2, 4).isValid)
  }

  test("heap basic") {
    val heap = Heap(100, 50, 40, 25, 24, 20, 19)
    assert(heap(0) == 100)
    assert(heap.top == 100)
  }

  test("heap insert") {
    assert(Heap(100, 50, 40).insert(30).isValid)
    assert(Heap(100, 50, 40).insert(60).isValid)
  }

  test("heap remove") {
    assert(Heap(100, 50, 40).removeTop().isValid)
    assert(Heap(100, 40, 50, 30).removeTop().isValid)
    assert(Heap(100, 40, 50, 30, 5, 2, 3, 4).removeTop().isValid)

  }

}