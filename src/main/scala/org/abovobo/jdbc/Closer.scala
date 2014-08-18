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
class SilentCloseableWrapper(closeable: AutoCloseable) {

  /**
   * Calls [[java.lang.AutoCloseable#close]] within try/catch block
   * silently swallowing [[java.sql.SQLException]] if thrown.
   */
  def dispose() {
    try {
      this.closeable.close()
    } catch {
      case t: Throwable => // do nothing
    }
  }
}

/**
 * Defines implicit conversions for JDBC objects like [[java.sql.Connection]], [[java.sql.Statement]],
 * [[java.sql.ResultSet]] and I/O ojbects like [[java.io.InputStream]] and [[java.io.Reader]]
 * into [[org.abovobo.jdbc.SilentCloseableWrapper]].
 */
object Closer {

  import scala.language.implicitConversions

  /**
   * Implicitly wraps [[java.sql.Connection]] with [[org.abovobo.jdbc.SilentCloseableWrapper]].
   *
   * @param connection JDBC connection to wrap.
   * @return Wrapping instance of [[org.abovobo.jdbc.SilentCloseableWrapper]]
   */
  implicit def wrapConnection(connection: java.sql.Connection) = new SilentCloseableWrapper(connection)

  /**
   * Implicitly wraps [[java.sql.Statement]] with [[org.abovobo.jdbc.SilentCloseableWrapper]].
   *
   * @param statement SQL statement to wrap.
   * @return Wrapping instance of [[org.abovobo.jdbc.SilentCloseableWrapper]]
   */
  implicit def wrapStatement(statement: java.sql.Statement) = new SilentCloseableWrapper(statement)

  /**
   * Implicitly wraps [[java.sql.ResultSet]] with [[org.abovobo.jdbc.SilentCloseableWrapper]].
   *
   * @param rs result set to wrap.
   * @return Wrapping instance of [[org.abovobo.jdbc.SilentCloseableWrapper]]
   */
  implicit def wrapResultSet(rs: java.sql.ResultSet) = new SilentCloseableWrapper(rs)

  /**
   * Implicitly wraps [[java.io.InputStream]] with [[org.abovobo.jdbc.SilentCloseableWrapper]].
   *
   * @param is input stream to wrap
   * @return Wrapping instance of [[org.abovobo.jdbc.SilentCloseableWrapper]]
   */
  implicit def wrapInputStream(is: java.io.InputStream) = new SilentCloseableWrapper(is)

  /**
   * Implicitly wraps [[java.io.Reader]] with [[org.abovobo.jdbc.SilentCloseableWrapper]].
   *
   * @param r reader to wrap
   * @return Wrapping instance of [[org.abovobo.jdbc.SilentCloseableWrapper]]
   */
  implicit def wrapReader(r: java.io.Reader) = new SilentCloseableWrapper(r)

  /**
   * Implicitly wraps any [[AutoCloseable]] with [[SilentCloseableWrapper]].
   *
   * @param ac AutoCloseable instance to wrap
   * @return Wrapping instance of [[SilentCloseableWrapper]]
   */
  implicit def wrapAutoCloseable(ac: AutoCloseable) = new SilentCloseableWrapper(ac)

  /**
   * Executes given block of code, which uses [[java.sql.ResultSet]] instance,
   * within try/finally and finally disposes [[java.sql.ResultSet]] instance
   * by means of [[org.abovobo.jdbc.SilentCloseableWrapper]].
   *
   * @param param  [[SilentCloseableWrapper]] to use within code block.
   * @param f   Actual code block to execute.
   * @tparam T  Type of value returned by code block.
   * @return    A value returned by code block.
   */
  def using[T, C <% SilentCloseableWrapper](param: C)(f: C => T): T = {
    try {
      f(param)
    } finally {
      param.dispose()
    }
  }
}
