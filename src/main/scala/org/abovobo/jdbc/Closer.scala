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

class CloseableWrapper(closeable: java.lang.AutoCloseable) {
  def dispose() {
    try {
      this.closeable.close()
    } catch {
      case java.sql.SQLException => // do nothing
    }
  }
}

/**
 * Created by dmitryzhuk on 28.12.13.
 */
object Closer {

  implicit def wrapStatement(statement: java.sql.Statement) = new CloseableWrapper(statement)

  implicit def wrapConnection(connection: java.sql.Connection) = new CloseableWrapper(connection)

  implicit def wrapResultSet(rs: java.sql.ResultSet) = new CloseableWrapper(rs)

  def using[T](rs: java.sql.ResultSet)(f: java.sql.ResultSet => T): T = {
    try {
      f(rs)
    } finally {
      rs.dispose()
    }
  }
}
