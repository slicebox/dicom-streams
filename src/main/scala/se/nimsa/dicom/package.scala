/*
 * Copyright 2018 Lars Edenbrandt
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package se.nimsa

import java.nio.{ByteBuffer, ByteOrder}

import akka.util.ByteString
import se.nimsa.dicom.VR.VR

package object dicom {

  private val hexDigits = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')

  def groupNumber(tag: Int): Int = tag >>> 16
  def elementNumber(tag: Int): Int = tag & '\uffff'

  def bytesToShort(bytes: ByteString, bigEndian: Boolean): Short = if (bigEndian) bytesToShortBE(bytes) else bytesToShortLE(bytes)
  def bytesToShortBE(bytes: ByteString): Short = ((bytes(0) << 8) + (bytes(1) & 255)).toShort
  def bytesToShortLE(bytes: ByteString): Short = ((bytes(1) << 8) + (bytes(0) & 255)).toShort
  def bytesToLong(bytes: ByteString, bigEndian: Boolean): Long = if (bigEndian) bytesToLongBE(bytes) else bytesToLongLE(bytes)
  def bytesToLongBE(bytes: ByteString): Long = (bytes(0).toLong << 56) + ((bytes(1) & 255).toLong << 48) + ((bytes(2) & 255).toLong << 40) + ((bytes(3) & 255).toLong << 32) + ((bytes(4) & 255).toLong << 24) + ((bytes(5) & 255) << 16).toLong + ((bytes(6) & 255) << 8).toLong + (bytes(7) & 255).toLong
  def bytesToLongLE(bytes: ByteString): Long = (bytes(7).toLong << 56) + ((bytes(6) & 255).toLong << 48) + ((bytes(5) & 255).toLong << 40) + ((bytes(4) & 255).toLong << 32) + ((bytes(3) & 255).toLong << 24) + ((bytes(2) & 255) << 16).toLong + ((bytes(1) & 255) << 8).toLong + (bytes(0) & 255).toLong
  def bytes4ToLong(bytes: ByteString, bigEndian: Boolean): Long = if (bigEndian) bytes4ToLongBE(bytes) else bytes4ToLongLE(bytes)
  def bytes4ToLongBE(bytes: ByteString): Long = (bytes(0).toLong << 56) + ((bytes(1) & 255).toLong << 48) + ((bytes(2) & 255).toLong << 40) + ((bytes(3) & 255).toLong << 32) + ((bytes(4) & 255).toLong << 24) + ((bytes(5) & 255) << 16).toLong + ((bytes(6) & 255) << 8).toLong + (bytes(7) & 255).toLong
  def bytes4ToLongLE(bytes: ByteString): Long = (bytes(7).toLong << 56) + ((bytes(6) & 255).toLong << 48) + ((bytes(5) & 255).toLong << 40) + ((bytes(4) & 255).toLong << 32) + ((bytes(3) & 255).toLong << 24) + ((bytes(2) & 255) << 16).toLong + ((bytes(1) & 255) << 8).toLong + (bytes(0) & 255).toLong
  def bytesToDouble(bytes: ByteString, bigEndian: Boolean): Double = if (bigEndian) bytesToDoubleBE(bytes) else bytesToDoubleLE(bytes)
  def bytesToDoubleBE(bytes: ByteString): Double = java.lang.Double.longBitsToDouble(bytesToLongBE(bytes))
  def bytesToDoubleLE(bytes: ByteString): Double = java.lang.Double.longBitsToDouble(bytesToLongLE(bytes))
  def bytesToFloat(bytes: ByteString, bigEndian: Boolean): Float = if (bigEndian) bytesToFloatBE(bytes) else bytesToFloatLE(bytes)
  def bytesToFloatBE(bytes: ByteString): Float = java.lang.Float.intBitsToFloat(bytesToIntBE(bytes))
  def bytesToFloatLE(bytes: ByteString): Float = java.lang.Float.intBitsToFloat(bytesToIntLE(bytes))
  def bytesToUShort(bytes: ByteString, bigEndian: Boolean): Int = if (bigEndian) bytesToUShortBE(bytes) else bytesToUShortLE(bytes)
  def bytesToUShortBE(bytes: ByteString): Int = ((bytes(0) & 255) << 8) + (bytes(1) & 255)
  def bytesToUShortLE(bytes: ByteString): Int = ((bytes(1) & 255) << 8) + (bytes(0) & 255)
  def bytesToTag(bytes: ByteString, bigEndian: Boolean): Int = if (bigEndian) bytesToTagBE(bytes) else bytesToTagLE(bytes)
  def bytesToTagBE(bytes: ByteString): Int = bytesToIntBE(bytes)
  def bytesToTagLE(bytes: ByteString): Int = (bytes(1) << 24) + ((bytes(0) & 255) << 16) + ((bytes(3) & 255) << 8) + (bytes(2) & 255)
  def bytesToVR(bytes: ByteString): Int = bytesToUShortBE(bytes)
  def bytesToInt(bytes: ByteString, bigEndian: Boolean): Int = if (bigEndian) bytesToIntBE(bytes) else bytesToIntLE(bytes)
  def bytesToIntBE(bytes: ByteString): Int = (bytes(0) << 24) + ((bytes(1) & 255) << 16) + ((bytes(2) & 255) << 8) + (bytes(3) & 255)
  def bytesToIntLE(bytes: ByteString): Int = (bytes(3) << 24) + ((bytes(2) & 255) << 16) + ((bytes(1) & 255) << 8) + (bytes(0) & 255)

  def shortToBytes(i: Short, bigEndian: Boolean): ByteString = if (bigEndian) shortToBytesBE(i) else shortToBytesLE(i)
  def shortToBytesBE(i: Short): ByteString = ByteString((i >> 8).toByte, i.toByte)
  def shortToBytesLE(i: Short): ByteString = ByteString(i.toByte, (i >> 8).toByte)
  def intToBytes(i: Int, bigEndian: Boolean): ByteString = if (bigEndian) intToBytesBE(i) else intToBytesLE(i)
  def intToBytesBE(i: Int): ByteString = ByteString((i >> 24).toByte, (i >> 16).toByte, (i >> 8).toByte, i.toByte)
  def intToBytesLE(i: Int): ByteString = ByteString(i.toByte, (i >> 8).toByte, (i >> 16).toByte, (i >> 24).toByte)
  def longToBytes(i: Long, bigEndian: Boolean): ByteString = if (bigEndian) longToBytesBE(i) else longToBytesLE(i)
  def longToBytesBE(i: Long): ByteString = ByteString((i >> 56).toByte, (i >> 48).toByte, (i >> 40).toByte, (i >> 32).toByte, (i >> 24).toByte, (i >> 16).toByte, (i >> 8).toByte, i.toByte)
  def longToBytesLE(i: Long): ByteString = ByteString(i.toByte, (i >> 8).toByte, (i >> 16).toByte, (i >> 24).toByte, (i >> 32).toByte, (i >> 40).toByte, (i >> 48).toByte, (i >> 56).toByte)
  def floatToBytes(f: Float, bigEndian: Boolean): ByteString = intToBytes(java.lang.Float.floatToIntBits(f), bigEndian)
  def doubleToBytes(d: Double, bigEndian: Boolean): ByteString = ByteString(ByteBuffer.wrap(new Array[Byte](8)).order(if (bigEndian) ByteOrder.BIG_ENDIAN else ByteOrder.LITTLE_ENDIAN).putDouble(d).array)
  def tagToBytes(tag: Int, bigEndian: Boolean): ByteString = if (bigEndian) tagToBytesBE(tag) else tagToBytesLE(tag)
  def tagToBytesBE(tag: Int): ByteString = intToBytesBE(tag)
  def tagToBytesLE(tag: Int): ByteString = ByteString((tag >> 16).toByte, (tag >> 24).toByte, tag.toByte, (tag >> 8).toByte)

  def intToUnsignedLong(i: Int): Long = i & 0xFFFFFFFFL
  def shortToUnsignedInt(i: Short): Int = i & 0xFFFF

  def tagToString(tag: Int): String = new String(Array('(',
    hexDigits(tag >>> 28), hexDigits(tag >>> 24 & 15), hexDigits(tag >>> 20 & 15), hexDigits(tag >>> 16 & 15), ',',
    hexDigits(tag >>> 12 & 15), hexDigits(tag >>> 8 & 15), hexDigits(tag >>> 4 & 15), hexDigits(tag >>> 0 & 15), ')'))
  def byteToHexString(b: Byte) = new String(Array(hexDigits(b >>> 4 & 15), hexDigits(b >>> 0 & 15)))
  def shortToHexString(s: Short): String = new String(Array(hexDigits(s >>> 12 & 15), hexDigits(s >>> 8 & 15), hexDigits(s >>> 4 & 15), hexDigits(s >>> 0 & 15)))
  def intToHexString(i: Int, bigEndian: Boolean): String = new String(Array(
    hexDigits(i >>> 28), hexDigits(i >>> 24 & 15), hexDigits(i >>> 20 & 15), hexDigits(i >>> 16 & 15),
    hexDigits(i >>> 12 & 15), hexDigits(i >>> 8 & 15), hexDigits(i >>> 4 & 15), hexDigits(i >>> 0 & 15)))

  def padToEvenLength(bytes: ByteString, tag: Int): ByteString = padToEvenLength(bytes, Dictionary.vrOf(tag))
  def padToEvenLength(bytes: ByteString, vr: VR): ByteString = {
    val padding = if ((bytes.length & 1) != 0) ByteString(vr.paddingByte) else ByteString.empty
    bytes ++ padding
  }

}
