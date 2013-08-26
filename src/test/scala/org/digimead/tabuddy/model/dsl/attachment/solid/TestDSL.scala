/**
 * SolidAttachment type for TABuddy-Model - a human-centric K,V framework
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

package org.digimead.tabuddy.model.dsl.attachment.solid

import org.digimead.tabuddy.model.Record
import org.digimead.tabuddy.model.dsl.DSL
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.predef.Note
import org.digimead.tabuddy.model.predef.Task

class TestDSL(val DLS_element: Element.Generic)
  extends DSL.RichElement
  with Record.DSL.RichElement
  with Note.DSL.RichElement
  with Task.DSL.RichElement

object TestDSL
  extends DSL[TestDSL](e => new TestDSL(e))
  with Record.DSL
  with Note.DSL
  with Task.DSL
