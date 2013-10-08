/**
 * Attachment type for TABuddy-Model - a human-centric K,V framework
 *
 * Copyright (c) 2013 Alexey Aksenov ezh@ezh.msk.ru
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

package org.digimead.tabuddy.model.dsl.attachment

import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.Record
import org.digimead.tabuddy.model.dsl.DSL
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.predef.Note
import org.digimead.tabuddy.model.predef.Task

import scala.language.implicitConversions

object TestDSL extends DSL
  with Model.DSL
  with Record.DSL
  with Note.DSL
  with Task.DSL {
  implicit def e2DSL(e: Element) = new ElementGenericDSL(e)
  implicit def me2DSL(me: Element.Relative[_ <: Element]) = new ElementGenericDSL(me.absolute)
  implicit def eRecord2DSL[A <: Record.Like](e: A) = new RecordSpecificDSL(e)
  implicit def meRecord2DSL[A <: Record.Like](me: Element.Relative[A]) = new RecordSpecificDSL(me.absolute)
  implicit def eNote2DSL[A <: Note.Like](e: A) = new NoteSpecificDSL(e)
  implicit def meNote2DSL[A <: Note.Like](me: Element.Relative[A]) = new NoteSpecificDSL(me.absolute)
  implicit def eTask2DSL[A <: Task.Like](e: A) = new TaskSpecificDSL(e)
  implicit def meTask2DSL[A <: Task.Like](me: Element.Relative[A]) = new TaskSpecificDSL(me.absolute)
  implicit def eModel2DSL[A <: Model.Like](e: A) = new ModelSpecificDSL(e)
  implicit def meModel2DSL[A <: Model.Like](me: Element.Relative[A]) = new ModelSpecificDSL(me.absolute)

  implicit val modelStashClass: Class[_ <: Model.Stash] = classOf[Model.Stash]
  implicit val recordStashClass: Class[_ <: Record.Stash] = classOf[Record.Stash]
  implicit val noteStashClass: Class[_ <: Note.Stash] = classOf[Note.Stash]
  implicit val taskStashClass: Class[_ <: Task.Stash] = classOf[Task.Stash]

  implicit def relative2absolute[A <: Element](relative: Element.Relative[A]): A = relative.absolute
}
