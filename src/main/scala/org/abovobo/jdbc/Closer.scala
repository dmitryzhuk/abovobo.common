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
 * Wraps [[java.lang.AutoCloseable]] providing method allowing to close
 * given object without exception being thrown.
 *
 * @param closeable An instance of [[java.lang.AutoCloseable]] to wrap.
 */
class SilentCloseableWrapper(closeable: java.lang.AutoCloseable) {

  /**
   * Calls [[java.lang.AutoCloseable#close]] within try/catch block
   * silently swallowing [[java.sql.SQLException]] if thrown.
   */
  def dispose() {
    try {
      this.closeable.close()
    } catch {
      case e: java.sql.SQLException => // do nothing
    }
  }
}

/**
 * Defines implicit conversions for JDBC objects like [[java.sql.Connection]], [[java.sql.Statement]],
 * [[java.sql.ResultSet]] into [[org.abovobo.jdbc.SilentCloseableWrapper]]. Also provides syntax sugar
 * for [[java.sql.ResultSet]] usage.
 */
object Closer {

  import scala.language.implicitConversions

  /**
   * Implicitely wraps [[java.sql.Connection]] with [[org.abovobo.jdbc.SilentCloseableWrapper]].
   *
   * @param connection JDBC connection to wrap.
   * @return Wrapping instance of [[org.abovobo.jdbc.SilentCloseableWrapper]]
   */
  implicit def wrapConnection(connection: java.sql.Connection) = new SilentCloseableWrapper(connection)

  /**
   * Implicitely wraps [[java.sql.Statement]] with [[org.abovobo.jdbc.SilentCloseableWrapper]].
   *
   * @param statement SQL statement to wrap.
   * @return Wrapping instance of [[org.abovobo.jdbc.SilentCloseableWrapper]]
   */
  implicit def wrapStatement(statement: java.sql.Statement) = new SilentCloseableWrapper(statement)

  /**
   * Implicitely wraps [[java.sql.ResultSet]] with [[org.abovobo.jdbc.SilentCloseableWrapper]].
   *
   * @param rs result set to wrap.
   * @return Wrapping instance of [[org.abovobo.jdbc.SilentCloseableWrapper]]
   */
  implicit def wrapResultSet(rs: java.sql.ResultSet) = new SilentCloseableWrapper(rs)

  /**
   * Executes given block of code, which uses [[java.sql.ResultSet]] instance,
   * within try/finally and finally disposes [[java.sql.ResultSet]] instance
   * by means of [[org.abovobo.jdbc.SilentCloseableWrapper]].
   *
   * @param rs  [[java.sql.ResultSet]] to use within code block.
   * @param f   Actual code block to execute.
   * @tparam T  Type of value returned by code block.
   * @return    A value returned by code block.
   */
  def using[T, C <% SilentCloseableWrapper](rs: C)(f: C => T): T = {
    try {
      f(rs)
    } finally {
      rs.dispose()
    }
  }
}
