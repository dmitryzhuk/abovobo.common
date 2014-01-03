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
 * A wrapper over [[java.sql.ResultSet]] instance allowing to read values in form of
 * Some instances. Thus, if value was NULL, methods will return None.
 *
 * @param rs An instance of [[java.sql.ResultSet]] to read values from.
 */
class ResultSetWrapper(rs: java.sql.ResultSet) {

  /**
   * Reads byte array at given index.
   * @param index An index to read value at.
   * @return None if value was NULL, Some otherwise.
   */
  def bytes(index: Int): Option[Array[Byte]] = {
    val result = this.rs.getBytes(index)
    if (this.rs.wasNull()) None else Some(result)
  }

  /**
   * Reads byte array from column with given name.
   * @param name A name of column to read value from.
   * @return None if value was NULL, Some otherwise.
   */
  def bytes(name: String): Option[Array[Byte]] = {
    val result = this.rs.getBytes(name)
    if (this.rs.wasNull()) None else Some(result)
  }

  /**
   * Reads [[java.util.Date]] at given index.
   * @param index An index to read value at.
   * @return None if value was NULL, Some otherwise.
   */
  def date(index: Int): Option[java.util.Date] = {
    val result = this.rs.getDate(index)
    if (this.rs.wasNull()) None else Some(result)
  }

  /**
   * Reads [[java.util.Date]] from column with given name.
   * @param name A name of column to read value from.
   * @return None if value was NULL, Some otherwise.
   */
  def date(name: String): Option[java.util.Date] = {
    val result = this.rs.getDate(name)
    if (this.rs.wasNull()) None else Some(result)
  }

  /**
   * Reads [[java.util.Date]] at given index via [[java.sql.Timestamp]] for exact value.
   * @param index An index to read value at.
   * @return None if value was NULL, Some otherwise.
   */
  def timestamp(index: Int): Option[java.util.Date] = {
    val result = this.rs.getTimestamp(index)
    if (this.rs.wasNull()) None else Some(new java.util.Date(result.getTime))
  }

  /**
   * Reads [[java.util.Date]] at given index via [[java.sql.Timestamp]] for exact value.
   * @param name A name of column to read value from.
   * @return None if value was NULL, Some otherwise.
   */
  def timestamp(name: String): Option[java.util.Date] = {
    val result = this.rs.getTimestamp(name)
    if (this.rs.wasNull()) None else Some(new java.util.Date(result.getTime))
  }
}

/**
 * Defines implicit conversions for ResultSet instance allowing to read values as optional.
 */
object Optional {

  import scala.language.implicitConversions

  /**
   * Implicitely wraps given result set by [[org.abovobo.jdbc.ResultSetWrapper]].
   *
   * @param rs An instance of [[java.sql.ResultSet]] to wrap
   * @return An instance of [[org.abovobo.jdbc.ResultSetWrapper]]
   */
  implicit def wrapResultSet(rs: java.sql.ResultSet) = new ResultSetWrapper(rs)
}
