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

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

/**
 * Created by dmitryzhuk on 22.01.14.
 */
class BencodeTest extends WordSpec with Matchers with BeforeAndAfterAll {

  "Decoder" when {
    "`i10e` is decoded" must {
      "produce int 10" in {
        val iterator = Bencode.decode("i10e".getBytes("UTF-8"))
        iterator.next() match {
          case Bencode.Integer(v) => v should be(10)
          case _ => fail("Unexpected event")
        }
        iterator should be ('empty)
      }
    }

    "`i-10e` is decoded" must {
      "produce int -10" in {
        val iterator = Bencode.decode("i-10e".getBytes("UTF-8"))
        iterator.next() match {
          case Bencode.Integer(v) => v should be(-10)
          case _ => fail("Unexpected event")
        }
        iterator should be ('empty)
      }
    }

    "`3:cow` is decoded" must {
      "produce string `cow`" in {
        val iterator = Bencode.decode("3:cow".getBytes("UTF-8"))
        iterator.next() match {
          case Bencode.Bytestring(v) => new String(v, "UTF-8") should be("cow")
          case _ => fail("Unexpected event")
        }
        iterator should be ('empty)
      }
    }

    "`4:cow` is decoded" must {
      "produce decoding exception" in {
        val iterator = Bencode.decode("4:cow".getBytes("UTF-8"))
        intercept[java.lang.IllegalArgumentException] {
          iterator.next() match {
            case Bencode.Bytestring(v) => new String(v, "UTF-8") should be("cow")
            case _ => fail("Unexpected event")
          }
        }
        iterator should be ('empty)
      }
    }

    "`l3:cowi10e4:bange` is decoded" must {
      "produce list `cow`, 10, bang" in {
        val iterator = Bencode.decode("l3:cowi10e4:bange".getBytes("UTF-8"))
        iterator.next() match {
          case Bencode.ListBegin() => // it's ok
          case _ => fail("Unexpected event")
        }
        iterator.next() match {
          case Bencode.Bytestring(v) => new String(v, "UTF-8") should be("cow")
          case _ => fail("Unexpected event")
        }
        iterator.next() match {
          case Bencode.Integer(v) => v should be(10)
          case _ => fail("Unexpected event")
        }
        iterator.next() match {
          case Bencode.Bytestring(v) => new String(v, "UTF-8") should be("bang")
          case _ => fail("Unexpected event")
        }
        iterator.next() match {
          case Bencode.ListEnd() => // it's ok
          case _ => fail("Unexpected event")
        }
        iterator should be ('empty)
      }
    }

    "`d1:eli201e23:A Generic Error Ocurrede1:t2:aa1:y1:ee` is decoded" must {
      "produce dictionary of DHT Error message structure" in {
        val iterator = Bencode.decode("d1:eli201e23:A Generic Error Ocurrede1:t2:aa1:y1:ee".getBytes("UTF-8"))
        iterator.next() shouldBe a [Bencode.DictionaryBegin]
        iterator.next() match {
          case Bencode.Bytestring(v) => new String(v, "UTF-8") should be("e")
          case _ => fail("Unexpected event")
        }
        iterator.next() shouldBe a [Bencode.ListBegin]
        iterator.next() match {
          case Bencode.Integer(v) => v should be(201)
          case _ => fail("Unexpected event")
        }
        iterator.next() match {
          case Bencode.Bytestring(v) => new String(v, "UTF-8") should be("A Generic Error Ocurred")
          case _ => fail("Unexpected event")
        }
        iterator.next() shouldBe a [Bencode.ListEnd]
        iterator.next() match {
          case Bencode.Bytestring(v) => new String(v, "UTF-8") should be("t")
          case _ => fail("Unexpected event")
        }
        iterator.next() match {
          case Bencode.Bytestring(v) => new String(v, "UTF-8") should be("aa")
          case _ => fail("Unexpected event")
        }
        iterator.next() match {
          case Bencode.Bytestring(v) => new String(v, "UTF-8") should be("y")
          case _ => fail("Unexpected event")
        }
        iterator.next() match {
          case Bencode.Bytestring(v) => new String(v, "UTF-8") should be("e")
          case _ => fail("Unexpected event")
        }
        iterator.next() shouldBe a [Bencode.DictionaryEnd]
        iterator should be ('empty)
      }
    }

  }
}
