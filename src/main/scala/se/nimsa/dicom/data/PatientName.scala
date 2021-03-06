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

package se.nimsa.dicom.data

case class ComponentGroup(alphabetic: String, ideographic: String, phonetic: String) {
  override def toString: String = s"$alphabetic=$ideographic=$phonetic".replaceAll("=+$", "")
}

case class PatientName(familyName: ComponentGroup, givenName: ComponentGroup, middleName: ComponentGroup, prefix: ComponentGroup, suffix: ComponentGroup) {
  override def toString: String = s"$familyName^$givenName^$middleName^$prefix^$suffix".replaceAll("\\^+$", "")
}

object PatientName {
  def parse(s: String): Seq[PatientName] = Value.parsePN(s)
}