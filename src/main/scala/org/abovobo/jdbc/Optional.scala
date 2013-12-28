package org.abovobo.jdbc

private[jdbc] class ResultSetWrapper(rs: java.sql.ResultSet) {
  def getBytesOrNone(index: Int): Option[Array[Byte]] = {
    val result = this.rs.getBytes(index)
    if (this.rs.wasNull()) None else Some(result)
  }
  def getBytesOrNone(name: String): Option[Array[Byte]] = {
    val result = this.rs.getBytes(name)
    if (this.rs.wasNull()) None else Some(result)
  }
  def getDateOrNone(index: Int): Option[java.util.Date] = {
    val result = this.rs.getDate(index)
    if (this.rs.wasNull()) None else Some(result)
  }
  def getDateOrNone(name: String): Option[java.util.Date] = {
    val result = this.rs.getDate(name)
    if (this.rs.wasNull()) None else Some(result)
  }
}

/**
 * Created by dmitryzhuk on 28.12.13.
 */
object Optional {
  implicit def wrapResultSet(rs: java.sql.ResultSet) = new ResultSetWrapper(rs)
}
