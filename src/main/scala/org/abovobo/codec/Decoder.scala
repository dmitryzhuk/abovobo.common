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

/** This trait defines general decoder contract */
trait Decoder[T, B] {
  /**
   * Decodes bytes from the given buffer using specific algorithm.
   * 
   * @param buf
   * 			a buffer to take bytes from for decoding
   * 
   * @return
   * 			decoded object
   */
  def decode(buf: B): T
}
