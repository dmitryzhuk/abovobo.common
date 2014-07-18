package org.abovobo.logging.global

import org.abovobo.integer.Integer160
import GlobalLogger._


object AbovoboLogMessage {
  object Scope extends Enumeration {
    type Scope = Value
    val In, Out, Local = Value
  }
  import Scope._
  
  def apply(self: Integer160, topic: String, scope: Scope, arguments: scala.collection.Map[String, Object] = Map()) =
    new AbovoboLogMessage(self, topic, scope, arguments)
}

class AbovoboLogMessage(val self: Integer160, val topic: String, val scope: AbovoboLogMessage.Scope.Value, val arguments: scala.collection.Map[String, Object]) {
  override def toString: String = {
    val sb = new StringBuilder
    var first = true
    def addParam(name: String, value: String) = {
      if (!first) {
        sb.append(',')          
      } else {
        first = false                    
      }
      sb.append('"').append(name).append("\":\"").append(value).append('"')
    }
    sb += '{'
      addParam("self", self.toString)
      addParam("scope", scope.toString)
      addParam("topic", topic)
      arguments.foreach { a => addParam(a._1, a._2.toString) }
    sb += '}'
    sb.toString
  }
}
