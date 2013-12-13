/**
 * Abovobo DHT Implementation
 *
 * This file is provided under terms and conditions of
 * Eclipse Public License v. 1.0
 * http://www.opensource.org/licenses/eclipse-1.0
 *
 * Developed by Dmitry Zhuk for Abovobo project.
 */

package org.abovobo.integer

/**
 * This class defines inclusive range of 160-bit integers.
 *
 * @param min
 * 			Minimum value in the range
 * @param max
 * 			Maximum value in the range
 */
class Range160(val min: Integer160, val max: Integer160) {

  /** Default constructor creating range which is covers complete Integer160 value range */
  def this() = this(Integer160.zero, Integer160.maxval)

  /** Constructs range from byte array dump */
  def this(bb: Array[Byte], offset: Int) = 
    this(new Integer160(bb, offset), new Integer160(bb, offset + Integer160.bytesize))

  /** Constructs range from byte array dump */
  def this(bb: Array[Byte]) = this(bb, 0)

  /// This obvious requirement
  require(this.min < this.max)

  /**
   * Right-associativity which checks if given number fits in this range
   *
   * @param i
   * 		An integer to test
   * @return
   * 		true if id fits into the bucket range, false otherwise
   */
  def ==:(i: Integer160) = this.min <= i && i <= this.max

  /**
   * Right-associativity which checks if given id does not fit in this range
   *
   * @param i
   * 		An integer to test
   * @return
   * 		true if id does not fit into the bucket range, false otherwise
   */
  def !=:(i: Integer160) = !this.==:(i)

  /**
   * Splits range at given edge returning Pair[Range160, Range160] where
   * left part of the pair is this.min..edge and right part is edge + 1..this.max
   */
  def split(edge: Integer160): (Range160, Range160) = 
    new Range160(this.min, this.min + edge) -> new Range160(this.min + edge + 1, this.max)
  
  /** Splits this range producing two ranges each covering the half of this range. */
  def split: (Range160, Range160) = this.split((this.max - this.min) >> 2)

  /** Returns the range size which is a difference between max and min values */
  def size = this.max - this.min

  /** Returns random number in range */
  def random = Integer160.random % this.size + this.min
  
  /** Returns byte array representation of this range */
  def toArray: Array[Byte] = this.toArray(new Array[Byte](Range160.bytesize), 0)
  
  /**
   * Writes data into array of bytes. It is required that given array has at least offset + Range160#bytesize
   * bytes so all the data can fit in.
   * 
   * @param bb
   * 		An array to write data into
   * @param offset
   * 		An index of the byte in array to start writing from
   */
  def toArray(bb: Array[Byte], offset: Int): Array[Byte] = {
    this.min.toArray(bb, offset)
    this.max.toArray(bb, offset + Integer160.bytesize)
    bb
  }

  /** @see java.lang.Object#equals(java.lang.Object) */
  override def equals(that: Any) = that match {
    case r: Range160 => this.min == r.min && this.max == r.max
    case _ => false
  }

  /** @see java.lang.Object#hashCode */
  override def hashCode: Int = (this.min ^ this.max).hashCode

  /** @see java.lang.Object#toString */
  override def toString = this.min.toHexString + ".." + this.max.toHexString

}

object Range160 {
  
  /** Size in bytes of Range160 object */
  val bytesize = 2 * Integer160.bytesize

  /** Defines ordering of the Range160 values */
  trait Ordering extends scala.math.Ordering[Range160] {
    override def compare(x: Range160, y: Range160): Int = if (x.min < y.min) -1 else if (x.min > y.min) 1 else 0
  }

  /** Declares implicit object for using in sorted collections */
  implicit object OrderingImplicit extends Ordering

}
