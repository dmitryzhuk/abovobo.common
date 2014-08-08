/**
 * Abovobo Infrastructure Implementation
 *
 * This file is provided under terms and conditions of
 * Eclipse Public License v. 1.0
 * http://www.opensource.org/licenses/eclipse-1.0
 *
 * Developed by Dmitry Zhuk for Abovobo project.
 */

package org.abovobo.conversions

import scala.collection.mutable

/**
 * Implements Bencoding and Bdecoding providing API similar to StAX for decoding.
 */
object Bencode {

  /** Parsing event base */
  sealed trait Event

  /**
   * Integer number occurred.
   * @param value Actual value.
   */
  case class Integer(value: Long) extends Event

  /**
   * Bytestring occurred.
   * @param value Actual value.
   */
  case class Bytestring(value: Array[Byte]) extends Event

  /**
   * Simple string occurred (can only happen in encoding procedure).
   * @param value Actual value.
   */
  case class JustString(value: String) extends Event

  /**
   * Simple character occurred (can only happen in encoding procedure).
   * @param value Actual value.
   */
  case class Character(value: Char) extends Event

  /** Indicates beginning of the list */
  case class ListBegin() extends Event

  /** Indicates end of the list */
  case class ListEnd() extends Event

  /** Indicates beginning of the dictionary */
  case class DictionaryBegin() extends Event

  /** Indicates end of the dictionary */
  case class DictionaryEnd() extends Event

  /** Indicates impossible situation */
  case class ImpossibleFailure() extends Event

  /**
   * Encodes given traversable collection of events into a sequence of bytes.
   *
   * @param events traversable collection of events to encode.
   *
   * @return sequence of bytes.
   */
  def encode(events: Traversable[Event]): IndexedSeq[Byte] = {
    val buf = new mutable.ArrayBuffer[Byte]()
    events foreach {
      case DictionaryBegin() => buf += 'd'
      case ListBegin() => buf += 'l'
      case DictionaryEnd() | ListEnd() => buf += 'e'
      case Character(c) => buf += '1' += ':' += c.toByte
      case JustString(s) => buf ++= s.length.toString.getBytes("UTF-8") += ':' ++= s.getBytes("UTF-8")
      case Integer(v) => buf += 'i' ++= v.toString.getBytes("UTF-8") += 'e'
      case Bytestring(b) => buf ++= b.length.toString.getBytes("UTF-8") += ':' ++= b
      case _ => throw new IllegalArgumentException("Invalid event")
    }
    buf
  }

  /**
   * Decodes given sequence of bytes converting it into a sequence of events happened
   * during decoding process.
   *
   * @param data a sequence of bytes to decode.
   *
   * @return Iterator through decoding events.
   */
  def decode(data: IndexedSeq[Byte]): Iterator[Event] = {

    object State extends Enumeration {
      val Begin, Dictionary, DictionaryValue, List, Integer, BytestringLength, BytestringBody = Value
    }

    val stack = new mutable.Stack[State.Value]
    stack.push(State.Begin)

    val buf = new mutable.ArrayBuffer[Byte]

    val n = data.length
    var i = 0
    var bslen = 0L

    def xthrow(b: Byte): Unit = 
      throw new IllegalArgumentException("Invalid character " + b.toChar + " at " + (i - 1))
    

    new Iterator[Event]() {
      override def hasNext = i < n
      override def next(): Bencode.Event = {
        while (i < n) {
          val b = data(i)
          i = i + 1
          stack.top match {
            case State.Begin | State.DictionaryValue => b match {
              case 'd' =>
                stack.push(State.Dictionary)
                return DictionaryBegin()
              case 'l' =>
                stack.push(State.List)
                return ListBegin()
              case 'i' => stack.push(State.Integer)
              case c: Byte if java.lang.Character.isDigit(c.toChar) =>
                stack.push(State.BytestringLength)
                buf += b
              case _ => xthrow(b)
            }
            case State.List => b match {
              case 'e' =>
                stack.pop()
                if (stack.top == State.DictionaryValue) stack.pop()
                return ListEnd()
              case 'd' =>
                stack.push(State.Dictionary)
                return DictionaryBegin()
              case 'l' =>
                stack.push(State.List)
                return ListBegin()
              case 'i' => stack.push(State.Integer)
              case c: Byte if java.lang.Character.isDigit(c.toChar) =>
                stack.push(State.BytestringLength)
                buf += b
              case _ => xthrow(b)
            }
            case State.Dictionary => b match {
              case 'e' =>
                stack.pop()
                if (stack.top == State.DictionaryValue) stack.pop()
                return DictionaryEnd()
              case c: Byte if java.lang.Character.isDigit(c.toChar) =>
                stack.push(State.BytestringLength)
                buf += b
              case _ => xthrow(b)
            }
            case State.BytestringLength => b match {
              case ':' =>
                bslen = java.lang.Long.parseLong(new String(buf.toArray, "UTF-8"))
                buf.clear()
                stack.pop()
                if (bslen == 0) {
                  // no string body will follow
                  stack.top match {
                    case State.DictionaryValue => stack.pop()
                    case State.Dictionary => stack.push(State.DictionaryValue)
                    case _ => // do nothing
                  }
                  return Bytestring(Array[Byte]()) 
                }
                stack.push(State.BytestringBody)
              case c: Byte if java.lang.Character.isDigit(c.toChar) =>
                buf += b
              case _ => xthrow(b)
            }
            case State.BytestringBody =>
              buf += b
              if (buf.length == bslen) {
                val array = buf.toArray
                buf.clear()
                bslen = 0
                stack.pop()
                stack.top match {
                  case State.DictionaryValue => stack.pop()
                  case State.Dictionary => stack.push(State.DictionaryValue)
                  case _ => // do nothing
                }
                return Bytestring(array)
              }
            case State.Integer => b match {
              case 'e' =>
                val value = java.lang.Long.parseLong(new String(buf.toArray, "UTF-8"))
                buf.clear()
                stack.pop()
                if (stack.top == State.DictionaryValue) stack.pop()
                return Integer(value)
              case '-' if buf.length == 0 =>
                buf += b
              case c: Byte if java.lang.Character.isDigit(c.toChar) =>
                buf += b
              case _ => xthrow(b)
            }
          }
        }
        if (stack.top != State.Begin) throw new IllegalArgumentException("Invalid state " + stack.top)
        ImpossibleFailure()
      }
    }
  }

}
