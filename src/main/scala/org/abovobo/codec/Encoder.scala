/**
 * Abovobo DHT Implementation
 *
 * This file is provided under terms and conditions of
 * Eclipse Public License v. 1.0
 * http://www.opensource.org/licenses/eclipse-1.0
 *
 * Developed by Dmitry Zhuk for Abovobo project.
 */

package org.abovobo.codec

/** This trait defines general encoder contract */
trait Encoder[T, B] {
  /**
   * Encodes given object using specific algorithm.
   *
   * @param o
   * 		an object to encode
   *
   * @return
   * 		buffer containing encoded bytes
   */
  def encode(o: T): B
}
