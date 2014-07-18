package org.abovobo.logging.global

import java.util.ArrayList
import java.util.concurrent.ArrayBlockingQueue

import org.slf4j.Logger
import org.slf4j.LoggerFactory

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.UnsynchronizedAppenderBase

object GlobalLogger {
  /** 
   *  @return named logger which configured to write to a global log (e.g. remote web server)
   */
  def getLogger(name: String): Logger = {
    val logger = LoggerFactory.getLogger("org.abovobo.global." + name).asInstanceOf[ch.qos.logback.classic.Logger]
    logger.addAppender(AbovoboGlobalAppender)
    logger.setLevel(Level.ALL)
    logger.setAdditive(false)
    logger
  }
  
  protected object AbovoboGlobalAppender extends UnsynchronizedAppenderBase[ILoggingEvent] {
    type Event = ILoggingEvent
    
    import scala.concurrent._
    import ExecutionContext.Implicits.global

    val threshold = 5 // should be more for real usage 
    private val q = new ArrayBlockingQueue[Event](threshold * 3)
    
    {
      setName("Global")
      this.setContext(LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext])
      this.start
      
      Runtime.getRuntime().addShutdownHook(new Thread() {
        override def run() {
          AbovoboGlobalAppender.this.stop()
        }
      })      
    }

    def append(eventObject: Event) {
      q.add(eventObject)
      if (q.size > threshold) flush()
    }
    
    override def stop() {
      super.stop()
      doFlush()
    }
    
    private def flush() { future { doFlush() } }
    
    private def doFlush() {
      import collection.JavaConversions._
      // processing bunch of message at once: e.g. posting them to a web server
      val buf = new ArrayList[Event](threshold * 2)
      q.drainTo(buf)
      println(buf.map { e => "GlobalLog: " + e.getTimeStamp() + " " + e}.mkString("\n"))      
    }
  }
}