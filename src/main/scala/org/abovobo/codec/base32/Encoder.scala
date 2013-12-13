/**
 * Abovobo Base32 implementation
 *
 * This file is provided under terms and conditions of
 * Eclipse Public License v. 1.0
 * http://www.opensource.org/licenses/eclipse-1.0
 *
 * Developed by Dmitry Zhuk for Abovobo project.
 */

package org.abovobo.codec.base32

import scala.collection.mutable.ArrayBuffer

/** Implements encoding of the array of bytes into Base32 string */
class Encoder extends org.abovobo.codec.Encoder[Array[Byte], String] {

  /** @see com.abovobo.codec.Encoder#encode */
  override def encode(o: Array[Byte]): String = {
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

}

object Encoder extends Encoder
