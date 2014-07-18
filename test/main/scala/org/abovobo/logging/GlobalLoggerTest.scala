package org.abovobo.logging

import org.abovobo.logging.global.GlobalLogger
import org.abovobo.integer.Integer160
import org.abovobo.logging.global.AbovoboLogMessage
import org.abovobo.logging.global.AbovoboLogMessage._
import org.abovobo.logging.global.AbovoboLogMessage.Scope._

object GlobalLoggerTest extends App {


  val selfId = Integer160.random
  
  val logger = GlobalLogger.getLogger("dht.controller")
  
  logger.info("Incoming message 1 {}", AbovoboLogMessage(selfId, "find_node", Scope.In, Map("target" -> Integer160.random.toString)))
  logger.info("Incoming message 2 {}", AbovoboLogMessage(selfId, "find_node", Scope.In, Map("target" -> Integer160.random.toString)))
  logger.info("Incoming message 3 {}", AbovoboLogMessage(selfId, "find_node", Scope.In, Map("target" -> Integer160.random.toString)))
  logger.info("Incoming message 4 {}", AbovoboLogMessage(selfId, "find_node", Scope.In, Map("target" -> Integer160.random.toString)))
  logger.info("Incoming message 5 {}", AbovoboLogMessage(selfId, "find_node", Scope.In, Map("target" -> Integer160.random.toString)))
  logger.info("Incoming message 6 {}", AbovoboLogMessage(selfId, "find_node", Scope.In, Map("target" -> Integer160.random.toString)))
  Thread.sleep(1000)
  logger.info("Incoming message 7 {}", AbovoboLogMessage(selfId, "find_node", Scope.In, Map("target" -> Integer160.random.toString)))
  Thread.sleep(1000)
}
