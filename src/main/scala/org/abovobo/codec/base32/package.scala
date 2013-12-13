/**
 * Abovobo Base32 implementation
 *
 * This file is provided under terms and conditions of
 * Eclipse Public License v. 1.0
 * http://www.opensource.org/licenses/eclipse-1.0
 *
 * Developed by Dmitry Zhuk for Abovobo project.
 */

package org.abovobo.codec

package object base32 {

  /** Defines base32 block */
  private[base32] object Block {
    /** Number of raw bytes */
    val bytes = 5
    /** Number of base32 characters */
    val chars = 8
    /** Number of bits in base32 character */
    val bits = 5
  }
  
  /** Defines base32 symbolic alphabet object */
  private[base32] object Alphabet {
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
