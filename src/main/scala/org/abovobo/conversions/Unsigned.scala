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
 * Methods of this object convert smaller integers (in term of bit size)
 * into larger integers assuming unsigned semantics, so if given value have
 * zeroth bit set this will not result in negative value of converted integer.
 */
object Unsigned {

  def ushort(b: Byte): Short = (b & 0x00FF).toShort

  def uint(b: Byte):  Int = b & 0x000000FF
  def uint(s: Short): Int = s & 0x0000FFFF
  def uint(c: Char):  Int = c & 0x0000FFFF

  def ulong(b: Byte):  Long = b & 0x00000000000000FFL
  def ulong(s: Short): Long = s & 0x000000000000FFFFL
  def ulong(c: Char):  Long = c & 0x000000000000FFFFL
  def ulong(i: Int):   Long = i & 0x00000000FFFFFFFFL

}
