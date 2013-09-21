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

import java.io.File
import java.io.IOException
import java.io.PrintWriter
import java.util.UUID

import org.digimead.digi.lib.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.digi.lib.util.Hash
import org.digimead.lib.test.LoggingHelper
import org.digimead.lib.test.StorageHelper
import org.digimead.tabuddy.model.Model
import org.digimead.tabuddy.model.dsl.DSLType
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Reference
import org.digimead.tabuddy.model.element.Reference.reference2implementation
import org.digimead.tabuddy.model.element.Value.string2someValue
import org.digimead.tabuddy.model.element.Value.value2x
import org.digimead.tabuddy.model.graph.Graph
import org.digimead.tabuddy.model.graph.Graph.graph2interface
import org.digimead.tabuddy.model.serialization.Serialization
import org.digimead.tabuddy.model.serialization.YAMLSerialization
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

import com.escalatesoft.subcut.inject.NewBindingModule

class AttachmentTypeSpec extends FunSpec with ShouldMatchers with StorageHelper with LoggingHelper with Loggable {
  @volatile private var folder: Option[File] = None
  val config = new NewBindingModule(module ⇒ {
    module.bind[DSLType] identifiedBy "Attachment" toSingle { new Attachment.Type }
    module.bind[File] identifiedBy "Temp" toSingle {
      val temp = File.createTempFile("TA-Buddy-", "-temp")
      if (!temp.delete())
        throw new IOException("Could not delete temp file: " + temp.getAbsolutePath())
      if (!temp.mkdir())
        throw new IOException("Could not create temp directory: " + temp.getAbsolutePath())
      temp.deleteOnExit()
      temp
    }
  }) ~ default ~ org.digimead.tabuddy.model.default ~ org.digimead.digi.lib.default

  after { adjustLoggingAfter }
  before {
    DependencyInjection(config, false)
    adjustLoggingBefore
  }

  describe("An Attachment Type") {
    it("should support the interface API") {
      withTempFolder { folder ⇒
        import TestDSL._

        // create an attachment
        val attachmentOrigin = new File(folder, "test.txt")
        Some(new PrintWriter(attachmentOrigin)).foreach { p ⇒ p.write("hello world"); p.close }

        val graph = Graph[Model]('john1, 'john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID(), Element.timestamp(1, 1))
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        val record = model.record('test).eRelative
        val attachment = Attachment(record, attachmentOrigin.toURI, None)
        record.eSet[Attachment]('test, Some(attachment))
        val stored = record.eGet[Attachment]('test).get.get

        val stream = stored.open
        stream.available() should not be (true)
        stored.digest() should be("5eb63bbbe01eeed093cb22bb8f5acdc3")
        stored.get.eq(stored) should be(true)
        stored.internal.left.map(_._2.isFile()) should be(Left(true))
        stored.length should be(attachment.length())
        Hash.digest(stream, Attachment.digestType) should be(Some(stored.digest()))
        stream.close()

        graph.storages = graph.storages :+ folder.getAbsoluteFile().toURI()
        val timestamp = Serialization.freeze(graph)
        val local = Serialization.perScheme("file")
        val ancestorsNSelf = record.eNode.safeRead(node ⇒ node.ancestors.reverse :+ node)
        val elementFolder = new File(local.acquireElementLocation(ancestorsNSelf, record.eBox, folder.getAbsoluteFile().toURI()))
        elementFolder.getAbsolutePath() should startWith(folder + "/john1/model")
        val attachmentFile = new File(elementFolder, Seq(Attachment.directoryName, attachmentOrigin.getName()).mkString(File.separator))
        log.debug("Test attachment file " + attachmentFile)
        attachmentFile should be('file)

        val graph2 = Serialization.acquire(graph.origin, folder.toURI)
        val record2 = graph2.model.e(record.eReference).get
        val attachment2 = record2.eGet[Attachment]('test).get

        evaluating { attachment2.## } should produce[IllegalStateException]
        evaluating { attachment2.## } should produce[IllegalStateException]
        evaluating { attachment2.## } should produce[IllegalStateException]

        Reference.register(graph2)
        attachment2.stored should be(graph2.modified)
        attachment2 should be(attachment)
        graph.node.safeRead { node ⇒
          graph2.node.safeRead { node2 ⇒
            node.iteratorRecursive().corresponds(node2.iteratorRecursive()) { (a, b) ⇒ a.ne(b) && a.modified == b.modified && a.elementType == b.elementType }
          }
        } should be(true)

        log.___glance("DUMP: " + Graph.dump(graph2, false))
        val graph3 = graph2.copy()
        val test2 = (graph3.model | RecordLocation('test2)).eRelative
        test2.eSet[Attachment]('anotherAttachmentReference, Some(attachment2))
        test2.eGraph.eq(graph3) should be(true)
        test2.eGraph.ne(graph2) should be(true)
        graph3.nodes.size should be(3)
        graph2.nodes.size should be(2)
        graph3 should not be (graph)
        graph3 should not be (graph2)

        test2.eGet[Attachment]('anotherAttachmentReference).get should be(record2.eGet[Attachment]('test).get)
        test2.eGet[Attachment]('anotherAttachmentReference).get.eq(record2.eGet[Attachment]('test).get) should be(true)

        val timestamp1 = Serialization.freeze(graph3)
        val graphA = Serialization.acquire(graph3.origin, folder.toURI, timestamp)
        val graphB = Serialization.acquire(graph3.origin, folder.toURI, timestamp1)
        val attA = (graphA.model & RecordLocation('test)).eRelative
        val attB = (graphB.model & RecordLocation('test2)).eRelative
        attA.eGet[Attachment]('test).get should be(attB.eGet[Attachment]('anotherAttachmentReference).get)
        attA.eGet[Attachment]('test).get.ne(attB.eGet[Attachment]('anotherAttachmentReference).get) should be(true)

        attA.eGet[Attachment]('test).get.get.## should be(record.eGet[Attachment]('test).get.get.##)
        attA.eGet[Attachment]('test).get.get.uri should be(record.eGet[Attachment]('test).get.get.uri)
        attB.eGet[Attachment]('anotherAttachmentReference).get.get.## should be(record.eGet[Attachment]('test).get.get.##)
        attB.eGet[Attachment]('anotherAttachmentReference).get.get.uri should be(record.eGet[Attachment]('test).get.get.uri)

        Hash.digest(attA.eGet[Attachment]('test).get.get.open, Attachment.digestType) should be(Some(stored.digest()))
        Hash.digest(attB.eGet[Attachment]('anotherAttachmentReference).get.get.open, Attachment.digestType) should be(Some(stored.digest()))
      }
    }
    it("should rename an attachement") {
      withTempFolder { folder ⇒
        /*        AttachmentTypeSpec_j1.this.folder = Some(folder)
        Model.reset()
        val attachment = new File(folder, "test.txt")
        Some(new PrintWriter(attachment)).foreach { p ⇒ p.write("hello world"); p.close }
        val record = Model.record('test) { record ⇒ }
        val data = Attachment("test1", record, attachment)
        val stream = data.attachment()
        stream should not be ('empty)
        stream.get.close
        val storage = new File(data.storage().get)
        storage should be('isDirectory)
        val attachment1 = new File(storage, "test1")
        attachment1 should be('isFile)
        attachment1.length() should be(attachment.length())
        // copy to new
        val newData = Attachment("test2", record, data.attachment.get)
        val attachment2 = new File(storage, "test2")
        attachment2 should be('isFile)
        attachment2.length() should be(attachment.length())
        // remove old
        Attachment.clear(data)
        data.attachment() should be('empty)
        val attachment3 = newData.attachment()
        attachment3 should not be ('empty)
        attachment3.get.close()

        attachment1 should not be ('exists)*/
      }
    }
    it("should copy and move attachement between elements") {
      withTempFolder { folder ⇒
        /*        AttachmentTypeSpec_j1.this.folder = Some(folder)
        Model.reset()
        val attachment = new File(folder, "test.txt")
        Some(new PrintWriter(attachment)).foreach { p ⇒ p.write("hello world"); p.close }
        val record1 = Model.record('test1) { record ⇒ }
        val record2 = Model.record('test2) { record ⇒ }
        val record3 = Model.record('test3) { record ⇒ }
        val data1 = Attachment("test1", record1, attachment)
        val stream1 = data1.attachment()
        stream1 should not be ('empty)
        stream1.get.close
        data1.attachment() should not be ('empty)
        val data2 = Attachment.copy(data1, record2)
        val stream2 = data2.attachment()
        stream2 should not be ('empty)
        stream2.get.close
        val data3 = Attachment.move(data1, record3)
        val stream3 = data3.attachment()
        stream3 should not be ('empty)
        stream3.get.close
        data1.available should be(false)
        data1.digest should be('empty)
        data1.attachment should be('empty)*/
      }
    }
    it("should serialize and deserialize value") {
      withTempFolder { folder ⇒
        /*        AttachmentTypeSpec_j1.this.folder = Some(folder)
        Model.reset()
        val attachment = new File(folder, "test.txt")
        Some(new PrintWriter(attachment)).foreach { p ⇒ p.write("hello world"); p.close }
        val record1 = Model.record('test1) { record ⇒ }
        val value1 = Value.static(record1, Attachment("test1", record1, attachment))
        val str = DSLType.inner.convertToString(value1.get).get
        val data = DSLType.inner.convertFromString[Attachment](str)
        assert(data.get === value1.get)
        record1.eSet('file, Some(value1))
        val serialized = YAMLSerialization.to(record1)
        val deserialized = YAMLSerialization.from(serialized)
        attachment should be('exists)
        deserialized should not be ('empty)
        deserialized.get.eGet('file, 'Attachment).get.get should be(value1.get)
        val record2 = Model.record('test2) { record ⇒ }
        record2.eSet('file, Some(value1))
        val a = new BuiltinSerialization
        var frozen: Seq[Array[Byte]] = Seq()
        a.freeze(record2, (element, data) ⇒ { frozen = frozen :+ data })
        val b = new BuiltinSerialization
        val frozenIterator = frozen.iterator
        val test = b.acquire[Record[Record.Stash], Record.Stash](() ⇒ { if (frozenIterator.hasNext) Some(frozenIterator.next) else None })
        assert(test.get === record2)
        assert(test.get.eGet('file, 'Attachment).get.get === record2.eGet('file, 'Attachment).get.get)
        attachment should be('exists)
        val attachmentValue = test.get.eGet[Attachment]('file).get
        attachmentValue should be(value1)
        value1.get.digest should not be ('empty)
        attachmentValue.get.digest should not be ('empty)
        test.get.eGet[Attachment]('file).get.get.digest should not be ('empty)
        assert(test.get.eGet[Attachment]('file).get.get.digest === record2.eGet[Attachment]('file).get.get.digest)

        val serialized2 = BuiltinSerialization.to(record2)
        val deserialized2 = BuiltinSerialization.from(serialized2)
        assert(record2 === deserialized2.get)

        val serialized1 = BuiltinSerialization.to(record1)
        Attachment.clear(record1.eGet[Attachment]('file).get) // delete
        val deserialized1 = BuiltinSerialization.from(serialized1)
        assert(record1 === deserialized1.get)
        deserialized1.get.eGet[Attachment]('file).get.get.attachment should be('empty) // attachment is deleted
        deserialized1.get.eGet[Attachment]('file).get.get.digest should be('empty)*/
      }
    }
  }
}
