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
 * Methods of this object convert integers of different sizes and byte arrays to hexadecimal string
 * representation and can convert hexadecimal string back into a byte array
 */
object Hex {

  import ByteArray._

  /** Converts single byte into a hex string */
  def byte2hex(b: Byte): String = this.digits((b >>> 4) & 0xF) + this.digits(b & 0xF)

  /** Converts byte array into a hex string */
  def ba2hex(ba: Array[Byte]): String = ba.map({ byte2hex(_) }).foldLeft[String]("") { _ + _ }

  /** Converts 16-bit integer into a hex string through ByteArray conversion */
  def short2hex(s: Short): String = this.ba2hex(s)

  /** Converts 32-bit integer into a hex string through ByteArray conversion */
  def int2hex(i: Int): String = this.ba2hex(i)

  /** Converts 64-bit integer into a hex string through ByteArray conversion */
  def long2hex(l: Long): String = this.ba2hex(l)

  /** Extracts single byte from the given string at given offset */
  def hex2byte(s: String, off: Int): Byte = {
    if (s.length < off + 2) throw new IndexOutOfBoundsException(String.valueOf(s.length))
    (((this.unhex(s.charAt(off)) << 4) & 0xF0) | (this.unhex(s.charAt(off + 1)) & 0xF)).toByte
  }

  /** Extracts single byte from the given string at the beginning of the string */
  def hex2byte(s: String): Byte = this.hex2byte(s, 0)

  /** Extracts byte array from the given string starting from given offset up to len bytes */
  def hex2ba(s: String, off: Int, len: Int): Array[Byte] = {
    if (s.length < off + len * 2) throw new IndexOutOfBoundsException(String.valueOf(s.length))
    Array.tabulate[Byte](len) { index => this.hex2byte(s, off + index * 2) }
  }

  /** Converts whole hex string into a byte array adding leading zero if needed */
  def hex2ba(s: String): Array[Byte] = s.length % 2 match {
    case 0 => this.hex2ba(s, 0, s.length / 2)
    case 1 => this.hex2ba("0" + s, 0, s.length / 2 + 1)
  }

  /** Extracts 16-bit integer from the given string at given offset */
  def hex2short(s: String, off: Int): Short = this.hex2ba(s, off, 2)

  /** Extracts 16-bit integer from the given string at the beginning */
  def hex2short(s: String): Short = this.hex2short(s, 0)

  /** Extracts 32-bit integer from the given string at given offset */
  def hex2int(s: String, off: Int): Int = this.hex2ba(s, off, 4)

  /** Extracts 32-bit integer from the given string at the beginning */
  def hex2int(s: String): Int = this.hex2int(s, 0)

  /** Extracts 64-bit integer from the given string at given offset */
  def hex2long(s: String, off: Int): Long = this.hex2ba(s, off, 8)

  /** Extracts 64-bit integer from the given string at the beginning */
  def hex2long(s: String): Long = this.hex2long(s, 0)

  /// Array of hexadecimal digits used in string representation
  private val digits = Array("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F")

  /// Converts character of hex string into a number
  private def unhex(c: Int): Int = if (Character.isDigit(c)) c - 0x30 else c - 0x37

}
