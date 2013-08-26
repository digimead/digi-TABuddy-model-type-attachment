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

import java.io.File
import java.io.IOException
import java.io.PrintWriter
import java.net.URI

import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.lib.test.LoggingHelper
import org.digimead.lib.test.StorageHelper
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.Model.model2implementation
import org.digimead.tabuddy.model.Record
import org.digimead.tabuddy.model.dsl.DSLType
import org.digimead.tabuddy.model.dsl.attachment.solid.TestDSL.model2rich
import org.digimead.tabuddy.model.element.Context
import org.digimead.tabuddy.model.element.Reference
import org.digimead.tabuddy.model.element.Value
import org.digimead.tabuddy.model.element.Value.value2x
import org.digimead.tabuddy.model.serialization.BuiltinSerialization
import org.digimead.tabuddy.model.serialization.YAMLSerialization
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

import com.escalatesoft.subcut.inject.NewBindingModule

class SolidAttachmentTypeSpec_j1 extends FunSpec with ShouldMatchers with StorageHelper with LoggingHelper with Loggable {
  @volatile private var folder: Option[File] = None
  val config = new NewBindingModule(module => {
    module.bind[Reference => Option[URI]] identifiedBy ("Element.Storage") toSingle { (ref: Reference) =>
      SolidAttachmentTypeSpec_j1.this.folder flatMap { folder =>
        Model.e(ref) match {
          case Some(element) =>
            val storage = new File(folder, element.eId.name + " attachments")
            if (!storage.exists())
              storage.mkdirs()
            Option(storage.toURI())
          case None =>
            None
        }
      }
    }
    module.bind[DSLType] identifiedBy "SolidAttachment" toSingle { new SolidAttachment.Type }
  }) ~ default ~ org.digimead.tabuddy.model.default ~ org.digimead.digi.lib.default

  after { adjustLoggingAfter }
  before {
    DependencyInjection(config, false)
    adjustLoggingBefore
  }

  describe("A Solid File Type") {
    it("should support the interface API") {
      withTempFolder { folder =>
        SolidAttachmentTypeSpec_j1.this.folder = Some(folder)
        Model.reset()
        val record = Model.record('test) { record => }

        intercept[IOException] { // create the resource without an exists attachment
          SolidAttachment("test", record)
        }

        // create an attachment
        val attachment = new File(folder, "test.txt")
        Some(new PrintWriter(attachment)).foreach { p => p.write("hello world"); p.close }

        val resource = SolidAttachment("test", record, attachment)
        val solidFileValue = new Value.Static(resource, Context.empty)
        val previousValue = record.eSet('file, Some(solidFileValue))
        previousValue should be('empty)
        val newValue = record.eGet[SolidAttachment]('file)
        newValue should not be ('empty)

        // test an empty data
        val data = newValue.get.get
        val storage = data.storage()
        storage.get.toASCIIString() should startWith("file:/tmp/")
        storage.get.toASCIIString() should endWith("/test%20attachments/")
        val storageFile = new File(storage.get)
        storageFile should be('exists)
        storageFile should be('isDirectory)
        data.attachment() should not be ('empty)

        val otherData = SolidAttachment(data.name, record, attachment)
        val attachment1 = otherData.attachment()
        attachment1 should not be ('empty)
        attachment1.get.close

        val attachment2 = data.attachment()
        attachment2 should not be ('empty)
        attachment2.get.close

        data.available should be(true)

        data.digest.get should be("5eb63bbbe01eeed093cb22bb8f5acdc3")
      }
    }
    it("should rename an attachement") {
      withTempFolder { folder =>
        SolidAttachmentTypeSpec_j1.this.folder = Some(folder)
        Model.reset()
        val attachment = new File(folder, "test.txt")
        Some(new PrintWriter(attachment)).foreach { p => p.write("hello world"); p.close }
        val record = Model.record('test) { record => }
        val data = SolidAttachment("test1", record, attachment)
        val stream = data.attachment()
        stream should not be ('empty)
        stream.get.close
        val storage = new File(data.storage().get)
        storage should be('isDirectory)
        val attachment1 = new File(storage, "test1")
        attachment1 should be('isFile)
        attachment1.length() should be(attachment.length())
        // copy to new
        val newData = SolidAttachment("test2", record, data.attachment.get)
        val attachment2 = new File(storage, "test2")
        attachment2 should be('isFile)
        attachment2.length() should be(attachment.length())
        // remove old
        SolidAttachment.clear(data)
        data.attachment() should be('empty)
        val attachment3 = newData.attachment()
        attachment3 should not be ('empty)
        attachment3.get.close()

        attachment1 should not be ('exists)
      }
    }
    it("should copy and move attachement between elements") {
      withTempFolder { folder =>
        SolidAttachmentTypeSpec_j1.this.folder = Some(folder)
        Model.reset()
        val attachment = new File(folder, "test.txt")
        Some(new PrintWriter(attachment)).foreach { p => p.write("hello world"); p.close }
        val record1 = Model.record('test1) { record => }
        val record2 = Model.record('test2) { record => }
        val record3 = Model.record('test3) { record => }
        val data1 = SolidAttachment("test1", record1, attachment)
        val stream1 = data1.attachment()
        stream1 should not be ('empty)
        stream1.get.close
        data1.attachment() should not be ('empty)
        val data2 = SolidAttachment.copy(data1, record2)
        val stream2 = data2.attachment()
        stream2 should not be ('empty)
        stream2.get.close
        val data3 = SolidAttachment.move(data1, record3)
        val stream3 = data3.attachment()
        stream3 should not be ('empty)
        stream3.get.close
        data1.available should be(false)
        data1.digest should be('empty)
        data1.attachment should be('empty)
      }
    }
    it("should serialize and deserialize value") {
      withTempFolder { folder =>
        SolidAttachmentTypeSpec_j1.this.folder = Some(folder)
        Model.reset()
        val attachment = new File(folder, "test.txt")
        Some(new PrintWriter(attachment)).foreach { p => p.write("hello world"); p.close }
        val record1 = Model.record('test1) { record => }
        val value1 = Value.static(record1, SolidAttachment("test1", record1, attachment))
        val str = DSLType.inner.convertToString(value1.get).get
        val data = DSLType.inner.convertFromString[SolidAttachment](str)
        assert(data.get === value1.get)
        record1.eSet('file, Some(value1))
        val serialized = YAMLSerialization.to(record1)
        val deserialized = YAMLSerialization.from(serialized)
        attachment should be('exists)
        deserialized should not be ('empty)
        deserialized.get.eGet('file, 'SolidAttachment).get.get should be(value1.get)
        val record2 = Model.record('test2) { record => }
        record2.eSet('file, Some(value1))
        val a = new BuiltinSerialization
        var frozen: Seq[Array[Byte]] = Seq()
        a.freeze(record2, (element, data) => { frozen = frozen :+ data })
        val b = new BuiltinSerialization
        val frozenIterator = frozen.iterator
        val test = b.acquire[Record[Record.Stash], Record.Stash](() => { if (frozenIterator.hasNext) Some(frozenIterator.next) else None })
        assert(test.get === record2)
        assert(test.get.eGet('file, 'SolidAttachment).get.get === record2.eGet('file, 'SolidAttachment).get.get)
        attachment should be('exists)
        val attachmentValue = test.get.eGet[SolidAttachment]('file).get
        attachmentValue should be(value1)
        value1.get.digest should not be ('empty)
        attachmentValue.get.digest should not be ('empty)
        test.get.eGet[SolidAttachment]('file).get.get.digest should not be ('empty)
        assert(test.get.eGet[SolidAttachment]('file).get.get.digest === record2.eGet[SolidAttachment]('file).get.get.digest)

        val serialized2 = BuiltinSerialization.to(record2)
        val deserialized2 = BuiltinSerialization.from(serialized2)
        assert(record2 === deserialized2.get)

        val serialized1 = BuiltinSerialization.to(record1)
        SolidAttachment.clear(record1.eGet[SolidAttachment]('file).get) // delete
        val deserialized1 = BuiltinSerialization.from(serialized1)
        assert(record1 === deserialized1.get)
        deserialized1.get.eGet[SolidAttachment]('file).get.get.attachment should be('empty) // attachment is deleted
        deserialized1.get.eGet[SolidAttachment]('file).get.get.digest should be('empty)
      }
    }
  }
}
