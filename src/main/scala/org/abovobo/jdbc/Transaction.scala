package org.abovobo.jdbc

/**
 * Created by dmitryzhuk on 29.12.13.
 */
object Transaction {

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
