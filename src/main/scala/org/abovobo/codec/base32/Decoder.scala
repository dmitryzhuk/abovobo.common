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

/** Implements decoding from Base32 string into array of bytes */
class Decoder extends org.abovobo.codec.Decoder[Array[Byte], String] {

  /** @see com.abovobo.codec.Decoder#decode */
  override def decode(s: String): Array[Byte] = {
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

}

object Decoder extends Decoder
