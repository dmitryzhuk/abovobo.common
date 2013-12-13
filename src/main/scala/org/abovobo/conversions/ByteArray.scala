/**
 * Abovobo Infrastructure Implementation
 *
 * This file is provided under terms and conditions of
 * Eclipse Public License v. 1.0
 * http://www.opensource.org/licenses/eclipse-1.0
 *
 * Developed by Dmitry Zhuk for Abovobo project.
 */

package org.abovobo.conversions

/**
 * Methods of this object allow to convert built-in integer types (Short, Int, Long)
 * into byte array and extract them back from byte array. Explicit methods allow to
 * specify offset in byte array at which to start to process bytes, while implicit
 * methods will just assume zero offset. Note that implicit conversion from integer to
 * byte array creates new instance of byte array of appropriate size.
 */
object ByteArray {

  import Unsigned._

  // --------------
  // 16 bit integer
  // --------------

  /** Dumps 16-bit integer into a given byte array at given offset */
  def short2ba(v: Short, ba: Array[Byte], off: Int): Array[Byte] = {
    if (ba.length < off + 2) throw new IndexOutOfBoundsException(String.valueOf(ba.length))
    for (i <- 0 until 2) ba(off + i) = ((v >>> (2 - i - 1) * 8) & 0xFF).toByte
    ba
  }

  /** Converts 16-bit integer into a byte array of length 2 */
  implicit def short2ba(v: Short): Array[Byte] = this.short2ba(v, new Array[Byte](2), 0)

  /** Extracts 16-bit integer from given byte array at given offset */
  def ba2short(ba: Array[Byte], off: Int): Short = {
    if (ba.length < off + 2) throw new IndexOutOfBoundsException(String.valueOf(ba.length))
    ((uint(ba(off)) << 8) | uint(ba(off + 1))).toShort
  }

  /** Converts given byte array into 16-bit integer */
  implicit def ba2short(ba: Array[Byte]): Short = this.ba2short(ba, 0)


  // --------------
  // 32 bit integer
  // --------------

  /** Dumps 32-bit integer into a given byte array at given offset */
  def int2ba(v: Int, ba: Array[Byte], off: Int): Array[Byte] = {
    if (ba.length < off + 4) throw new IndexOutOfBoundsException(String.valueOf(ba.length))
    for (i <- 0 until 4) ba(off + i) = ((v >>> (4 - i - 1) * 8) & 0xFF).toByte
    ba
  }

  /** Converts 32-bit integer into a byte array of length 4 */
  implicit def int2ba(v: Int): Array[Byte] = this.int2ba(v, new Array[Byte](4), 0)

  /** Extracts 32-bit integer from given byte array at given offset */
  def ba2int(ba: Array[Byte], off: Int): Int = {
    if (ba.length < off + 4) throw new IndexOutOfBoundsException(String.valueOf(ba.length))
    uint(ba(off)) << 24 | uint(ba(off + 1)) << 16 | uint(ba(off + 2)) << 8 | uint(ba(off + 3))
  }

  /** Converts given byte array into 32-bit integer */
  implicit def ba2int(ba: Array[Byte]): Int = this.ba2int(ba, 0)


  // --------------
  // 64-bit integer
  // --------------

  /** Dumps 64-bit integer into a given byte array at given offset */
  def long2ba(v: Long, ba: Array[Byte], off: Int): Array[Byte] = {
    if (ba.length < off + 8) throw new IndexOutOfBoundsException(String.valueOf(ba.length))
    for (i <- 0 until 8) ba(off + i) = ((v >>> (8 - i - 1) * 8) & 0xFF).toByte
    ba
  }

  /** Converts 64-bit integer into a byte array of length 8 */
  implicit def long2ba(v: Long): Array[Byte] = this.long2ba(v, new Array[Byte](8), 0)

  /** Extracts 64-bit integer from given byte array at given offset */
  def ba2long(ba: Array[Byte], off: Int): Long = {
    if (ba.length < off + 8) throw new IndexOutOfBoundsException(String.valueOf(ba.length))
    (ulong(this.ba2int(ba, off)) << 32) | ulong(this.ba2int(ba, off + 4))
  }

  /** Converts given byte array into 64-bit integer */
  implicit def ba2long(ba: Array[Byte]): Long = this.ba2long(ba, 0)

}
