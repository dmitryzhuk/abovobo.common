/*
  ---------------------------------------------------------------------------
  This software is released under a BSD license, adapted from
  http://opensource.org/licenses/bsd-license.php

  Copyright (c) 2010, Brian M. Clapper
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

  * Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

  * Neither the names "clapper.org", "AVSL", nor the names of its
    contributors may be used to endorse or promote products derived from
    this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------
*/

/** Scala front-end to SLF4J API. */
package org.abovobo.logging

import scala.annotation.elidable
import scala.annotation.elidable._
import org.slf4j.bridge.SLF4JBridgeHandler

/**
 * Scala front-end to a SLF4J logger.
 * Usage:
 * <pre>
 * trace("Prints message only if trace enabled but computes it always: " + count)
 *
 * trace(() => "This message is very expensive to build, so it will be constructed" +
 * " and logged only if tracing is enabled: " + doExpensiveComputation())
 * </pre>
 *
 * @see elidable
 */
trait Logger {
  /**
   * Base level value for @elidable methods.
   * Use to: a) adjust logging to assertions (which have hardcoded elide level) and
   * b) workaround buggy IDE.
   */
  private final val EBASE = 3000

  protected val logger: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(loggerName)

  /**
   * Get the name associated with this logger.
   *
   * Override this method in a subclass to change default logger name.
   *
   * @return the name.
   */
  def loggerName = getClass.getName

  /**
   * Determine whether trace logging is enabled.
   */
  def isTraceEnabled = logger.isTraceEnabled

  @elidable(EBASE + FINER) def trace(msg: AnyRef): Unit =
    if (isTraceEnabled) logger.trace(msg.toString)

  @elidable(EBASE + FINER) def trace(msg: () ⇒ AnyRef): Unit =
    if (isTraceEnabled) logger.trace(msg().toString)

  @elidable(EBASE + FINER) def trace(msg: AnyRef, t: Throwable): Unit =
    if (isTraceEnabled) logger.trace(msg.toString, t)

  @elidable(EBASE + FINER) def trace(msg: () ⇒ AnyRef, t: () ⇒ Throwable): Unit =
    if (isTraceEnabled) logger.trace(msg().toString, t())

  /**
   * Determine whether debug logging is enabled.
   */
  def isDebugEnabled = logger.isDebugEnabled

  @elidable(EBASE + CONFIG) def debug(msg: AnyRef): Unit =
    if (isDebugEnabled) logger.debug(msg.toString)

  @elidable(EBASE + CONFIG) def debug(msg: () ⇒ AnyRef): Unit =
    if (isDebugEnabled) logger.debug(msg().toString)

  @elidable(EBASE + CONFIG) def debug(msg: AnyRef, t: Throwable): Unit =
    if (isDebugEnabled) logger.debug(msg.toString, t)

  @elidable(EBASE + CONFIG) def debug(msg: () ⇒ AnyRef, t: () ⇒ Throwable): Unit =
    if (isDebugEnabled) logger.debug(msg().toString, t())

  /**
   * Determine whether trace logging is enabled.
   */
  def isErrorEnabled = logger.isErrorEnabled

  @elidable(EBASE + SEVERE) def error(msg: AnyRef): Unit =
    if (isErrorEnabled) logger.error(msg.toString)

  @elidable(EBASE + SEVERE) def error(msg: () ⇒ AnyRef): Unit =
    if (isErrorEnabled) logger.error(msg().toString)

  @elidable(EBASE + SEVERE) def error(msg: AnyRef, t: Throwable): Unit =
    if (isErrorEnabled) logger.error(msg.toString, t)

  @elidable(EBASE + SEVERE) def error(msg: () ⇒ AnyRef, t: () ⇒ Throwable): Unit =
    if (isErrorEnabled) logger.error(msg().toString, t())

  /**
   * Determine whether trace logging is enabled.
   */
  def isInfoEnabled = logger.isInfoEnabled

  @elidable(EBASE + INFO) def info(msg: AnyRef): Unit =
    if (isInfoEnabled) logger.info(msg.toString)

  @elidable(EBASE + INFO) def info(msg: () ⇒ AnyRef): Unit =
    if (isInfoEnabled) logger.info(msg().toString)

  @elidable(EBASE + INFO) def info(msg: AnyRef, t: Throwable): Unit =
    if (isInfoEnabled) logger.info(msg.toString, t)

  @elidable(EBASE + INFO) def info(msg: () ⇒ AnyRef, t: () ⇒ Throwable): Unit =
    if (isInfoEnabled) logger.info(msg().toString, t())

  /**
   * Determine whether trace logging is enabled.
   */
  def isWarnEnabled = logger.isWarnEnabled

  @elidable(EBASE + WARNING) def warn(msg: AnyRef): Unit =
    if (isWarnEnabled) logger.warn(msg.toString)

  @elidable(EBASE + WARNING) def warn(msg: () ⇒ AnyRef): Unit =
    if (isWarnEnabled) logger.warn(msg().toString)

  @elidable(EBASE + WARNING) def warn(msg: AnyRef, t: Throwable): Unit =
    if (isWarnEnabled) logger.warn(msg.toString, t)

  @elidable(EBASE + WARNING) def warn(msg: () ⇒ AnyRef, t: () ⇒ Throwable): Unit =
    if (isWarnEnabled) logger.warn(msg.toString(), t)

}

object Logger {

  {
    // Logging setup
    val root = java.util.logging.Logger.getLogger("")
    root.info("Switching logging to SLF4J. Removing default handlers...")
    root.getHandlers.foreach(root.removeHandler)
    SLF4JBridgeHandler.install()

    // Logging of unhandled exceptions
    Thread.setDefaultUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler() {

      private val oldHandler = Thread.getDefaultUncaughtExceptionHandler

      def uncaughtException(t: Thread, e: Throwable) {
        if (oldHandler != null) {
          oldHandler.uncaughtException(t, e)
        }
        Logger
          .getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
          .error("Unhandled exception in thread [" + t.getName + "]", e)
      }
    }) // -- Thread.setDefaultUncaughtExceptionHandler
  }

  def getLogger(name: String) = new Logger {
    override val logger = org.slf4j.LoggerFactory.getLogger(name)
  }
}
