package core

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ArrayBuffer

object HeapTests {

  object Heap {
    def apply(elems: Int*): Heap = new Heap(elems)

  }


  class Heap(val originalData: Seq[Int]) {
    private val data = ArrayBuffer(originalData: _*)

    def size: Int = data.size
    def apply(index: Int): Int = data(index)
    def top: Int = apply(0)
    def isRoot(index: Int): Boolean = index == 0
    def calcParentIndex(childIndex: Int): Int = (childIndex - 1) / 2
    def leftChild(parentIndex: Int): Int = parentIndex*2 + 1
    def rightChild(parentIndex: Int): Int = parentIndex*2 + 2


    def update(index: Int, value: Int): Unit = data.update(index, value)

    def insert(value: Int): Heap = {
      val childIndex = data.size
      data.append(value)

      val holeIndex = siftUp(childIndex)
      data.update(holeIndex, value)
      this
    }

    def siftUp(childIndex: Int): Int = {
      if (isRoot(childIndex)) {
        return childIndex
      }

      val parentIndex = calcParentIndex(childIndex)
      if (data(parentIndex) >= data(childIndex)) {
        childIndex
      }
      else if (swap(parentIndex, childIndex))
        siftUp(parentIndex)
      else
        childIndex
    }

    def removeTop: Heap = {
      update(0, data.remove(data.size - 1))
      data.trimToSize()
      siftDown(0)
      this
    }

    def siftDown(parentIndex: Int): Heap = {
      val leftChildIndex = leftChild(parentIndex)
      val rightChildIndex = rightChild(parentIndex)
      //println(s"CJH siftDown ${this}, ${x(parentIndex)} -> ${x(leftChildIndex)}, ${x(rightChildIndex)}")

      if (size - 1 == parentIndex) {
      }
      else if (size - 1 == leftChildIndex) {
        swap(parentIndex, leftChildIndex)
      }
      else if (data(leftChildIndex) < data(rightChildIndex)) {
        if (swap(parentIndex, rightChildIndex))
          siftDown(rightChildIndex)
      }
      else {
        if (swap(parentIndex, leftChildIndex))
          siftDown(leftChildIndex)
      }
      this
    }

    def x(n: Int): String = {
      if (n >= size) s"${n}/error" else s"${n}/${data(n)}"
    }


    def swap(parentIndex: Int, childIndex: Int): Boolean = {
      //println(s"  Swapping ${x(parentIndex)} - ${x(childIndex)}")
      if (data(parentIndex) < data(childIndex)) {
        val temp = data(parentIndex)
        data.update(parentIndex, data(childIndex))
        data.update(childIndex, temp)
        true
      }
      else {
        false
      }
    }

    def isValid: Boolean = {
      for (i <- data.length - 1 until 0 by -1) {
        val parent = (i - 1) / 2
        if (data(i) > data(parent)) {
          //println(s"CJH isValid i: ${x(i)} > ${x(parent)}")
          return false
        }
      }
      return true
    }


    override def toString = s"Heap($data)"

    def canEqual(other: Any): Boolean = other.isInstanceOf[Heap]

    override def equals(other: Any): Boolean = other match {
      case that: Heap =>
        (that canEqual this) &&
          data == that.data
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(data)
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
    assert(Heap(100, 50, 40).removeTop.isValid)
    assert(Heap(100, 40, 50, 30).removeTop.isValid)
    assert(Heap(100, 40, 50, 30, 5, 2, 3, 4).removeTop.isValid)

  }

}