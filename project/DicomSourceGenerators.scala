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

import scala.xml.{Elem, NodeSeq, XML}

object DicomSourceGenerators {

  val xml: Elem = XML.loadFile("project/part06.xml")
  val chapters: NodeSeq = xml \ "chapter"

  case class Attribute(tagString: String, name: String, keyword: String, vr: String, vm: String, retired: Boolean)

  case class UID(uidValue: String, uidName: String, uidType: String, retired: Boolean)

  val nonAlphaNumeric = "[^a-zA-Z0-9_]"
  val nonHex = "[^a-fA-F0-9]"
  val nonUID = "[^0-9.]"

  val attributes: Seq[Attribute] = {
    val meta = chapters
      .find(_ \@ "label" == "7")
      .map(_ \\ "tbody" \ "tr")
    val data = chapters
      .find(_ \@ "label" == "6")
      .map(_ \\ "tbody" \ "tr")
    val rows = meta.flatMap(m => data.map(d => m ++ d)).getOrElse(NodeSeq.Empty)

    rows.map { row =>
      val cells = row \ "td"
      Attribute(
        cells.head.text.trim,
        cells(1).text.trim,
        cells(2).text.trim.replaceAll(nonAlphaNumeric, ""),
        cells(3).text.trim,
        cells(4).text.trim,
        cells(5).text.trim.nonEmpty)
    }
  }

  val uids: Seq[UID] = {
    chapters
      .find(_ \@ "label" == "A")
      .map(_ \\ "tbody" \ "tr")
      .map(rows => rows.map { row =>
        val cells = row \ "td"
        val uid = cells.head.text.trim.replaceAll(nonUID, "")
        val name = cells(1).text.trim
        UID(
          uid,
          name,
          cells(2).text.trim,
          name.endsWith("(Retired)"))
      })
      .getOrElse(Seq.empty)
  }

  def generateTag(): String =
    s"""package se.nimsa.dicom
       |
       |object Tag {
       |
       |${attributes.filter(_.keyword.nonEmpty).map(a => s"""  final val ${a.keyword} = 0x${a.tagString.replaceAll(nonHex, "").replaceAll("x", "0")}${if (a.retired) " // retired" else ""}""").mkString("\r\n")}
       |
       |}""".stripMargin

  def generateUID(): String = {
    val pairs = uids
      .map { uid =>
        val name = uid.uidName
        val hasDescription = name.indexOf(':') > 0
        val truncName = if (hasDescription) name.substring(0, name.indexOf(':')) else name
        val cleanedName = truncName
          .replaceAll(nonAlphaNumeric, "")
          .replaceAll("^12", "Twelve")
        (cleanedName, uid.uidValue, uid.retired)
      }
      .filter(_._1.nonEmpty)
      .filter(_._1 != "Retired")
      .distinct

    s"""package se.nimsa.dicom
       |
       |object UID {
       |
       |${pairs.map(p => s"""  final val ${p._1} = "${p._2}"${if (p._3) " // retired" else ""}""").mkString("\r\n")}
       |
       |}""".stripMargin
  }

  def generateDictionary(): String = {
    val split = 2000

    val tagVrMappings = attributes
      .filter(_.keyword.nonEmpty)
      .map { a =>
        val tag = s"0x${a.tagString.replaceAll(nonHex, "")}"
        val vr = a.vr match {
          case s if s == "CS" && a.keyword != "SourceImageIDs" => s
          case s if s.contains("OW") => "OW"
          case s if s.contains("SS") => "SS"
          case s => s
        }
        (tag, vr)
      }
      .filter(_._2.length == 2)
      .filter(_._1.length == 10)
      .sortBy(_._1)
      .splitAt(split)

    val splitValue = tagVrMappings._2.head._1

    s"""package se.nimsa.dicom
       |
       |import VR._
       |import scala.annotation.switch
       |
       |object Dictionary {
       |
       |  def vrOf(tag: Int): VR = tag match {
       |    case t if (t & 0x0000FFFF) == 0 => VR.UL
       |    case t if (t & 0x00010000) != 0 => if ((tag & 0x0000FF00) == 0 && (tag & 0x000000F0) != 0) VR.LO else VR.UN
       |    case t if (t & 0xFFFFFF00) == Tag.SourceImageIDs => VR.CS
       |    case t =>
       |      val t2 =
       |        if ((t & 0xFFE00000) == 0x50000000 || (t & 0xFFE00000) == 0x60000000)
       |          t & 0xFFE0FFFF
       |        else if ((t & 0xFF000000) == 0x7F000000 && (t & 0xFFFF0000) != 0x7FE00000)
       |          t & 0xFF00FFFF
       |        else
       |          t
       |      if (t2 < $splitValue) vrOfLow(t2) else vrOfHigh(t2)
       |  }
       |
       |  private def vrOfLow(tag: Int) = (tag: @switch) match {
       |    ${tagVrMappings._1.map(p => s"""case ${p._1} => ${p._2}""").mkString("\r\n    ")}
       |    case _ => UN
       |  }
       |
       |  private def vrOfHigh(tag: Int) = (tag: @switch) match {
       |    ${tagVrMappings._2.map(p => s"""case ${p._1} => ${p._2}""").mkString("\r\n    ")}
       |    case _ => UN
       |  }
       |
       |}""".stripMargin
  }

}
