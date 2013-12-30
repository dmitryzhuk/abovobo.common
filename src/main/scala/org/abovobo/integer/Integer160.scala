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

import java.math.BigInteger

import scala.annotation.tailrec
import scala.util.Random

import org.abovobo.codec.base32.Decoder
import org.abovobo.codec.base32.Encoder

/**
 * Integer which has exactly 160 bits and treated as unsigned.
 *
 * Thus, right shift operator (>>) fills leftmost bits with zeros.
 * This object is backed by an array of Int values.
 *
 * @param value
 * 		Backend array of integers
 */
class Integer160(private val value: Array[Int]) {

  /** Constructs Integer160 from the array of bytes. Only 20 bytes from array will be used starting from offset. */
  def this(bb: Array[Byte], offset: Int) = this(Array.tabulate[Int](Integer160.intsize) { i =>
    Integer160.toInt(bb(offset + i * 4)) << 24 | Integer160.toInt(bb(offset + i * 4 + 1)) << 16 |
      Integer160.toInt(bb(offset + i * 4 + 2)) << 8 | Integer160.toInt(bb(offset + i * 4 + 3))
  })

  /** Constructs Integer160 from the array of bytes. Only 20 bytes from array will be used. */
  def this(bb: Array[Byte]) = this(bb, 0)

  /** Constructs Integer160 from hexadecimal or base32 string */
  def this(s: String) = this(
    if (s.length == 40) Array.tabulate[Byte](Integer160.bytesize) { i =>
      (((Integer160.unhex(s(2 * i)) << 4) & 0xF0) | (Integer160.unhex(s(2 * i + 1)) & 0xF)).toByte
    }
    else if (s.length == 32) Decoder.decode(s)
    else throw new IllegalArgumentException)

  /** Constructs random Integer160 */
  def this(r: Random) = this(Array[Int](r.nextInt(), r.nextInt(), r.nextInt(), r.nextInt(), r.nextInt()))

  /** Constructs Integer160 from the single Byte placing it at low order position */
  def this(v: Byte) = this(Array[Int](0, 0, 0, 0, Integer160.toInt(v)))

  /** Constructs Integer160 from Short placing it at low order position */
  def this(v: Short) = this(Array[Int](0, 0, 0, 0, Integer160.toInt(v)))

  /** Constructs Integer160 from the single Int placing it at low order position */
  def this(v: Int) = this(Array[Int](0, 0, 0, 0, v))

  /** Constructs Integer160 from Long splitting it on 2 integers for lower order positions */
  def this(v: Long) =
    this(Array[Int](0, 0, 0, (v >>> BitSizeOf.Int).asInstanceOf[Int], (v & Integer160.longmask).asInstanceOf[Int]))

  /** Binary inversion: unary operator NOT */
  def unary_~(): Integer160 = new Integer160(Array.tabulate(this.value.length) { ~this.value(_) })

  /** Binary exclusive OR: XOR operator */
  def ^(that: Integer160): Integer160 =
    new Integer160(Array.tabulate(this.value.length) { i => this.value(i) ^ that.value(i) })

  /** Binary addition: AND operator */
  def &(that: Integer160): Integer160 =
    new Integer160(Array.tabulate(this.value.length) { i => this.value(i) & that.value(i) })

  /** Binary subtraction: OR operator */
  def |(that: Integer160): Integer160 =
    new Integer160(Array.tabulate(this.value.length) { i => this.value(i) | that.value(i) })

  /** Logical equality comparison operator */
  def ==(that: Integer160): Boolean = this.equals(that) // #equals calls Integer160#compare(this, that, 0) == 0

  /** Logical non equality comparison operator. If a == b is true than a != b is false and vice versa. */
  def !=(that: Integer160): Boolean = !this.==(that)

  /** Logical less comparison operator. */
  def <(that: Integer160): Boolean = Integer160.compare(this.value, that.value, 0) == -1

  /** Logical less or equal operator */
  def <=(that: Integer160): Boolean = Integer160.compare(this.value, that.value, 0) < 1

  /** Logical greater operator */
  def >(that: Integer160): Boolean = Integer160.compare(this.value, that.value, 0) == 1

  /** Logical greater or equal operator */
  def >=(that: Integer160): Boolean = Integer160.compare(this.value, that.value, 0) > -1

  /** Addition operator */
  def +(that: Integer160): Integer160 =
    new Integer160(Integer160.add(this.value, that.value, 0L, this.value.size - 1, new Array[Int](this.value.size)))

  /** Subtraction operator */
  def -(that: Integer160): Integer160 =
    new Integer160(Integer160.sub(this.value, that.value, 0L, this.value.size - 1, new Array[Int](this.value.size)))

  /** Division operator. This can't be optimized to fixed number size, so just delegate this to BigInteger */
  def /(that: Integer160): Integer160 =
    new Integer160(Integer160.pad(new BigInteger(1, this.toArray).divide(new BigInteger(1, this.toArray)).toByteArray))

  /** Division remainder operator. This can't be optimized to fixed number size, so just delegate this to BigInteger */
  def %(that: Integer160): Integer160 =
    new Integer160(Integer160.pad(new BigInteger(1, this.toArray).mod(new BigInteger(1, that.toArray)).toByteArray))

  /** Signed right shift works as unsigned and kept for completeness. */
  def >>(x: Int): Integer160 = this.>>>(x)

  /** Unsigned right shift */
  def >>>(x: Int): Integer160 = if (x < 0) this.<<(-x) else if (x == 0) new Integer160(this.value) else {

    // if shifting 160 bits or more that is just zero
    if (x >= BitSizeOf.Integer160) Integer160.zero

    // shifting less than 160 bits
    else {

      val l = this.value.size // array length
      val n = x / BitSizeOf.Int // number of full integers to shift
      val m = x % BitSizeOf.Int // number of bits to shift additionally to full

      // if there is no bit remainder to shift let's do simple
      if (m == 0) new Integer160(Array.tabulate(l) { i => if (i < n) 0 else this.value(i - n) })

      // otherwise things are a slightly more complicated
      else new Integer160(Array.tabulate(l) { i =>
        if (i < n) 0
        else if (i == n) this.value(0) >>> m
        else (this.value(i - n) >>> m) | (this.value(i - n - 1) << (BitSizeOf.Int - m))
      })

    }
  }

  /** Left shift */
  def <<(x: Int): Integer160 = if (x < 0) this.>>>(-x) else if (x == 0) new Integer160(this.value) else {

    // if shifting 160 bits or more that is just zero
    if (x >= BitSizeOf.Integer160) Integer160.zero

    // shifting less than 160 bits
    else {

      val l = this.value.size // array length
      val n = x / BitSizeOf.Int // number of full integers to shift
      val m = x % BitSizeOf.Int // number of bits to shift additionally to full

      // if there is no bit remainder to shift let's do simple
      if (m == 0) new Integer160(Array.tabulate(l) { i => if (i < l - n) this.value(n + i) else 0 })

      // otherwise things are a slightly more complicated
      else new Integer160(Array.tabulate(l) { i =>
        if (i < l - n - 1) (this.value(n + i) << m) | (this.value(n + i + 1) >>> (BitSizeOf.Int - m))
        else if (i == l - n - 1) this.value(n + i) << m
        else 0
      })

    }
  }

  /** @see java.lang.Object#equals(java.lang.Object) */
  override def equals(that: Any): Boolean = that match {
    case int160: Integer160 => Integer160.compare(this.value, int160.value, 0) == 0
    case _ => false
  }

  /** @see java.lang.Object#hashCode */
  override def hashCode: Int = this.value.foldLeft(0) { _ * 31 + _ }
  
  /** Converts to byte array which has exactly 20 bytes in it */
  def toArray: Array[Byte] = this.toArray(new Array[Byte](Integer160.bytesize), 0)
  
  /**
   * Writes data into array of bytes. It is required that given array has at least offset + Integer160#bytesize
   * bytes so all the data can fit in.
   * 
   * @param bb
   * 		An array to write data into
   * @param offset
   * 		An index of the byte in array to start writing from
   */
  def toArray(bb: Array[Byte], offset: Int): Array[Byte] = {
    require(bb.size >= Integer160.bytesize + offset)
    for (i <- 0 until Integer160.bytesize) {
      bb(i + offset) = (this.value(i / (Integer160.intsize - 1)) >>>
        (BitSizeOf.Int - (i % (Integer160.intsize - 1) + 1) * BitSizeOf.Byte)).toByte
    }
    bb
  }

  /** Converts to string */
  override def toString = this.toHexString

  /** Converts to base32 encoded string */
  def toBase32String = (new Encoder).encode(this.toArray)

  /** Converts to binary representation string */
  def toBinaryString = this.value map { v =>
    (for (i <- 1 until BitSizeOf.Int + 1) yield String.valueOf((v >>> (BitSizeOf.Int - i)) & 1)) reduceLeft { _ + _ }
  } reduceLeft { _ + _ }

  /** Converts to hexadecimal representation string */
  def toHexString = this.value map { v =>
    (for (i <- 0 until 8) yield Integer160.digits((v >>> ((7 - i) * 4)) & 0xF)) reduceLeft { _ + _ }
  } reduceLeft { _ + _ }

}

object Integer160 {

  import scala.language.implicitConversions

  /** Implicit conversion from Byte to Integer160 */
  implicit def byte2integer160(v: Byte): Integer160 = new Integer160(v)

  /** Implicit conversion from Short to Integer160 */
  implicit def short2integer160(v: Short): Integer160 = new Integer160(v)

  /** Implicit conversion from Int to Integer160 */
  implicit def int2integer160(v: Int): Integer160 = new Integer160(v)

  /** Implicit conversion from Long to Integer160 */
  implicit def long2integer160(v: Long): Integer160 = new Integer160(v)

  /** Size of the Integer160 in bytes */
  val bytesize = BitSizeOf.Integer160 / BitSizeOf.Byte

  /** Size of the Integer160 in integers */
  val intsize = BitSizeOf.Integer160 / BitSizeOf.Int

  /** Constructs random Integer160 */
  def random: Integer160 = new Integer160(new Random(System.currentTimeMillis))

  /** Returns 0 value */
  def zero: Integer160 = new Integer160(Array[Int](0, 0, 0, 0, 0))

  /** Returns maximum possible value */
  def maxval: Integer160 = new Integer160(Array[Int](maxint, maxint, maxint, maxint, maxint))

  /** Returns value which is 2 ** n */
  def pow(n: Int) = new Integer160(1) << n

  /** Defines ordering of the Integer160 values */
  trait Ordering extends scala.math.Ordering[Integer160] {
    override def compare(x: Integer160, y: Integer160): Int = if (x < y) -1 else if (x > y) 1 else 0
  }

  /** Declares implicit object for using in sorted collections */
  implicit object OrderingImplicit extends Ordering

  /// Array of digits for convesion to hex string
  protected val digits = Array("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F")

  /// Mask which zeros 4 higher order bytes of the Long integer
  protected val longmask: Long = 0x00000000FFFFFFFFL

  /// Mask which gives one bit right beyond Int
  protected val plusbit: Long = 0x0000000100000000L

  /// Maximum possible unsigned integer value
  protected val maxint: Int = 0xFFFFFFFF
  
  /// Produces number corresponding to given hext digit
  protected def unhex(c: Int): Int = if (Character.isDigit(c)) c - 0x30 else c - 0x37

  /// Converts Byte to Int maintaining unsigned semantics
  protected def toInt(v: Byte): Int = v.asInstanceOf[Int] & 0x000000FF
  // -- private def UI(v: Byte): Int = this.toInt(v)

  /// Converts Short to Int maintaining unsigned semantics
  protected def toInt(v: Short): Int = v.asInstanceOf[Int] & 0x0000FFFF
  protected def UI(v: Short): Int = this.toInt(v)

  /// Converts Int to Long maintaining unsigned semantics
  protected def toLong(v: Int): Long = v.asInstanceOf[Long] & this.longmask
  protected def UL(v: Int): Long = this.toLong(v)

  /// Pads an array with leading zeroes, fails if array is longer than needed
  protected def pad(a: Array[Int]): Array[Int] = {
    require(a.size <= intsize)
    if (a.size < intsize) Array.concat(new Array[Int](intsize - a.size), a) else a
  }

  /// Pads an array with leading zeroes, takes bytesize most signifcant bytes if array is longer than needed
  protected def pad(a: Array[Byte]): Array[Byte] =
    if (a.size < bytesize) Array.concat(new Array[Byte](bytesize - a.size), a) else a.take(bytesize)

  /// Recursively compares this value to that value at given index calling itself if values are equal at the index.
  ///
  /// @param left	left-hand operand
  /// @param right	right-hand operand
  /// @param index	current comparison index
  /// @return		0 if arrays are equal, 1 if left operand is greater than right one -1 otherwise
  @tailrec
  private def compare(left: Array[Int], right: Array[Int], index: Int): Int =
    // if end of array is reached return 0 (arrays are equal)
    if (index == left.size) 0
    // if left and right components are equal go to the next iteration
    else if (left(index) == right(index)) compare(left, right, index + 1)
    // unsigned less comparison: if left less than right return -1 (left array is less than right one)
    else if (UL(left(index)) < UL(right(index))) -1
    // otherwise return 1 (left array is greater than right one)
    else 1

  /// Recursively adds right array to left array starting from the end of arrays.
  /// Typical invocation is: add(l, r, 0, length - 1, new Array[Int](size))
  ///
  /// @param left	left-hand operand
  /// @param right	right-hand operand
  /// @param carry	carry digit for overflow value
  /// @param index	current index in the arrays
  /// @param result	array to collect results
  /// @return		final result
  @tailrec
  private def add(left: Array[Int], right: Array[Int], carry: Long, index: Int, result: Array[Int]): Array[Int] =
    // if index is beyond zero just return the result
    if (index == -1) result
    else {
      // add up integers as they are unsigned plus carry
      val sum = UL(left(index)) + UL(right(index)) + carry
      // set the next component into result [sum % 0x100000000]
      result(index) = (sum & this.longmask).asInstanceOf[Int]
      // call itself using new values [carry = if (sum < 0x100000000) 0 else 1]
      this.add(left, right, sum >>> BitSizeOf.Int, index - 1, result)
    }

  /// Recursively subtracts right array from left array starting from the end of arrays.
  /// Typical invocation is: sub(l, r, 0, length - 1, new Array[Int](size))
  ///
  /// @param left	left-hand operand
  /// @param right	right-hand operand
  /// @param carry	carry digit for underflow value
  /// @param index	current index in the arrays
  /// @param result	array to collect results
  /// @return		final result
  @tailrec
  private def sub(left: Array[Int], right: Array[Int], carry: Long, index: Int, result: Array[Int]): Array[Int] =
    // if index is beyond zero just return the result
    if (index == -1) result
    else {
      // subtract integers as they are unsigned plus carry
      val res = UL(left(index)) - UL(right(index)) + carry
      // set the next component into result [sum % 0x100000000]
      result(index) = (res & this.longmask).asInstanceOf[Int]
      // call itself using new values [carry = if (res >= 0) 0 else -1]
      this.sub(left, right, res >> BitSizeOf.Int, index - 1, result)
    }

}
