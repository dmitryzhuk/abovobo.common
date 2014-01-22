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

import scala.collection.mutable.ArrayBuffer

/**
 * Implements conversion from/to Base32 encoding.
 */
object Base32 {

  /**
   * Converts given array of bytes into a base32 encoded string.
   *
   * @param o an array to convert.
   * @return Base32 encoded string.
   */
  def encode(o: Array[Byte]): String = {
    val blocks = o.size / Block.bytes
    val modulus = o.size % Block.bytes
    val buf = new ArrayBuffer[Char]((blocks + scala.math.signum(modulus)) * Block.chars)
    for (block <- 0 until blocks) {
      buf += Alphabet( (o(block * 5 + 0) & 0xFF) >>> 3                                      & 0x1F )
      buf += Alphabet( (o(block * 5 + 1) & 0xFF) >>> 6 | ((o(block * 5 + 0) & 0xFF) << 2)   & 0x1F )
      buf += Alphabet( (o(block * 5 + 1) & 0xFF) >>> 1                                      & 0x1F )
      buf += Alphabet( (o(block * 5 + 2) & 0xFF) >>> 4 | ((o(block * 5 + 1) & 0xFF) << 4)   & 0x1F )
      buf += Alphabet( (o(block * 5 + 3) & 0xFF) >>> 7 | ((o(block * 5 + 2) & 0xFF) << 1)   & 0x1F )
      buf += Alphabet( (o(block * 5 + 3) & 0xFF) >>> 2                                      & 0x1F )
      buf += Alphabet( (o(block * 5 + 4) & 0xFF) >>> 5 | ((o(block * 5 + 3) & 0xFF) << 3)   & 0x1F )
      buf += Alphabet( (o(block * 5 + 4) & 0xFF)                                            & 0x1F )
    }
    if (modulus == 1) {
      buf += Alphabet( (o(blocks * 5 + 0) & 0xFF) >>> 3                                     & 0x1F )
      buf += Alphabet( (o(blocks * 5 + 0) & 0xFF) <<  2                                     & 0x1F )
      buf += Alphabet.pad // padding
      buf += Alphabet.pad // padding
      buf += Alphabet.pad // padding
      buf += Alphabet.pad // padding
      buf += Alphabet.pad // padding
      buf += Alphabet.pad // padding
    }
    else if (modulus == 2) {
      buf += Alphabet( (o(blocks * 5 + 0) & 0xFF) >>> 3                                     & 0x1F )
      buf += Alphabet( (o(blocks * 5 + 1) & 0xFF) >>> 6 | ((o(blocks * 5 + 0) & 0xFF) << 2) & 0x1F )
      buf += Alphabet( (o(blocks * 5 + 1) & 0xFF) >>> 1                                     & 0x1F )
      buf += Alphabet( (o(blocks * 5 + 1) & 0xFF) <<  4                                     & 0x1F )
      buf += Alphabet.pad // padding
      buf += Alphabet.pad // padding
      buf += Alphabet.pad // padding
      buf += Alphabet.pad // padding
    }
    else if (modulus == 3) {
      buf += Alphabet( (o(blocks * 5 + 0) & 0xFF) >>> 3                                     & 0x1F )
      buf += Alphabet( (o(blocks * 5 + 1) & 0xFF) >>> 6 | ((o(blocks * 5 + 0) & 0xFF) << 2) & 0x1F )
      buf += Alphabet( (o(blocks * 5 + 1) & 0xFF) >>> 1                                     & 0x1F )
      buf += Alphabet( (o(blocks * 5 + 2) & 0xFF) >>> 4 | ((o(blocks * 5 + 1) & 0xFF) << 4) & 0x1F )
      buf += Alphabet( (o(blocks * 5 + 2) & 0xFF) <<  1                                     & 0x1F )
      buf += Alphabet.pad // padding
      buf += Alphabet.pad // padding
      buf += Alphabet.pad // padding
    }
    else if (modulus == 4) {
      buf += Alphabet( (o(blocks * 5 + 0) & 0xFF) >>> 3                                     & 0x1F )
      buf += Alphabet( (o(blocks * 5 + 1) & 0xFF) >>> 6 | ((o(blocks * 5 + 0) & 0xFF) << 2) & 0x1F )
      buf += Alphabet( (o(blocks * 5 + 1) & 0xFF) >>> 1                                     & 0x1F )
      buf += Alphabet( (o(blocks * 5 + 2) & 0xFF) >>> 4 | ((o(blocks * 5 + 1) & 0xFF) << 4) & 0x1F )
      buf += Alphabet( (o(blocks * 5 + 3) & 0xFF) >>> 7 | ((o(blocks * 5 + 2) & 0xFF) << 1) & 0x1F )
      buf += Alphabet( (o(blocks * 5 + 3) & 0xFF) >>> 2                                     & 0x1F )
      buf += Alphabet( (o(blocks * 5 + 3) & 0xFF) <<  3                                     & 0x1F )
      buf += Alphabet.pad // padding
    }
    new String(buf.toArray)
  }

  /**
   * Converts given Base32 encoded string into an array of bytes.
   *
   * @param s Base32 encoded string to convert.
   * @return An array of bytes.
   */
  def decode(s: String): Array[Byte] = {
    val buf = s + new String(Array.fill[Char](Block.chars - s.size % Block.chars) { Alphabet.pad })
    val blocks = buf.size / Block.chars
    val o = new ArrayBuffer[Byte](blocks * Block.bytes)
    for (i <- 0 until blocks) {
      o += (
        (Alphabet.find(buf(i * 8 + 0)) & 0xFF) << 3 |
          (Alphabet.find(buf(i * 8 + 1)) & 0xFF) >>> 2).asInstanceOf[Byte]
      if (buf(i * 8 + 2) != Alphabet.pad) {
        o += (
          (Alphabet.find(buf(i * 8 + 1)) & 0xFF) << 6 |
            (Alphabet.find(buf(i * 8 + 2)) & 0xFF) << 1 |
            (Alphabet.find(buf(i * 8 + 3)) & 0xFF) >>> 4).asInstanceOf[Byte]
        if (buf(i * 8 + 4) != Alphabet.pad) {
          o += (
            (Alphabet.find(buf(i * 8 + 3)) & 0xFF) << 4 |
              (Alphabet.find(buf(i * 8 + 4)) & 0xFF) >>> 1).asInstanceOf[Byte]
          if (buf(i * 8 + 5) != Alphabet.pad) {
            o += (
              (Alphabet.find(buf(i * 8 + 4)) & 0xFF) << 7 |
                (Alphabet.find(buf(i * 8 + 5)) & 0xFF) << 2 |
                (Alphabet.find(buf(i * 8 + 6)) & 0xFF) >>> 3).asInstanceOf[Byte]
            if (buf(i * 8 + 7) != Alphabet.pad) {
              o += ((Alphabet.find(buf(i * 8 + 6)) & 0xFF) << 5 |
                (Alphabet.find(buf(i * 8 + 7)) & 0xFF)).asInstanceOf[Byte]
            }
          }
        }
      }
    }
    o.toArray
  }


  /** Defines base32 block */
  private object Block {
    /** Number of raw bytes */
    val bytes = 5
    /** Number of base32 characters */
    val chars = 8
    /** Number of bits in base32 character */
    val bits = 5
  }

  /** Defines base32 symbolic alphabet object */
  private object Alphabet {
    /** Symbol set for base32 encoding */
    val dictionary = Array[Char](
      // 0    1    2    3    4    5    6    7
      'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
      'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
      'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
      'Y', 'Z', '2', '3', '4', '5', '6', '7')

    /** Padding character */
    val pad = '='

    def apply(index: Int): Char = this.dictionary(index)

    /** Returns index of the character in dictionary or -1 if not found */
    def find(c: Char): Int =
      if (c > 0x40 && c < 0x5B) c - 0x41 else if (c > 0x31 && c < 0x38) c - 0x18 else if (c == this.pad) 32 else -1
  }

}
