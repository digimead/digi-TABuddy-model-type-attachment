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

package org.digimead.tabuddy.model.dsl

import org.digimead.tabuddy.model.dsl.attachment.Attachment
import org.yaml.snakeyaml.constructor.Construct
import org.yaml.snakeyaml.representer.Represent

import com.escalatesoft.subcut.inject.NewBindingModule

/**
 * This is the DSL Type extension for TA Buddy model.
 */
package object attachment {
  lazy val default = new NewBindingModule(module ⇒ {
    module.bind[Construct] identifiedBy ("YAML.Construct.DSLType.PlainAttachment") toSingle { new Plain.Construct }
    module.bind[Represent] identifiedBy ("YAML.Represent.DSLType.PlainAttachment") toSingle { new Plain.Represent }
    module.bind[DSLType] identifiedBy "DSLType.PlainAttachment" toSingle { new Plain.Type }
  })
}
