/**
 * Abovobo Infrastructure Implementation
 *
 * This file is provided under terms and conditions of
 * Eclipse Public License v. 1.0
 * http://www.opensource.org/licenses/eclipse-1.0
 *
 * Developed by Dmitry Zhuk for Abovobo project.
 */

package org.abovobo.jdbc

/**
 * This object defines syntax sugar for wrapping the transactions.
 */
object Transaction {

  /**
   * Wraps code block into a try/catch block to manage commits and rollbacks.
   * It executes provided code block within try section and in case of exception calls
   * Connection.rollback, otherwise calls Connection.commit.
   *
   * @param c   A connection which is used in this transaction.
   * @param f   A code block to execute.
   * @tparam T  Return type of the code block.
   * @return    A value yielded by code block.
   */
  def transaction[T](c: java.sql.Connection)(f: => T): T = {
    try {
      val t = f
      c.commit()
      t
    } catch {
      case t: Throwable =>
        c.rollback()
        throw t
    }
  }
}
