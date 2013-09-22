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
    module.bind[DSLType] identifiedBy "PlainAttachment" toSingle { new Plain.Type }
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

        evaluating { Attachment[Plain](record, "asdf:sdaf", null, None) } should produce[IllegalArgumentException]

        val attachment = Attachment[Plain](record, attachmentOrigin.toURI, None)
        record.eSet[Plain]('test, Some(attachment))
        val stored = record.eGet[Plain]('test).get.get

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
        val attachment2 = record2.eGet[Plain]('test).get

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
        test2.eSet[Plain]('anotherAttachmentReference, Some(attachment2))
        test2.eGraph.eq(graph3) should be(true)
        test2.eGraph.ne(graph2) should be(true)
        graph3.nodes.size should be(3)
        graph2.nodes.size should be(2)
        graph3 should not be (graph)
        graph3 should not be (graph2)

        test2.eGet[Plain]('anotherAttachmentReference).get should be(record2.eGet[Plain]('test).get)
        test2.eGet[Plain]('anotherAttachmentReference).get.eq(record2.eGet[Plain]('test).get) should be(true)

        val timestamp1 = Serialization.freeze(graph3)
        val graphA = Serialization.acquire(graph3.origin, folder.toURI, timestamp)
        val graphB = Serialization.acquire(graph3.origin, folder.toURI, timestamp1)
        val attA = (graphA.model & RecordLocation('test)).eRelative
        val attB = (graphB.model & RecordLocation('test2)).eRelative
        attA.eGet[Plain]('test).get should be(attB.eGet[Plain]('anotherAttachmentReference).get)
        attA.eGet[Plain]('test).get.ne(attB.eGet[Plain]('anotherAttachmentReference).get) should be(true)

        attA.eGet[Plain]('test).get.get.## should be(record.eGet[Plain]('test).get.get.##)
        attA.eGet[Plain]('test).get.get.uri should be(record.eGet[Plain]('test).get.get.uri)
        attB.eGet[Plain]('anotherAttachmentReference).get.get.## should be(record.eGet[Plain]('test).get.get.##)
        attB.eGet[Plain]('anotherAttachmentReference).get.get.uri should be(record.eGet[Plain]('test).get.get.uri)

        Hash.digest(attA.eGet[Plain]('test).get.get.open, Attachment.digestType) should be(Some(stored.digest()))
        Hash.digest(attB.eGet[Plain]('anotherAttachmentReference).get.get.open, Attachment.digestType) should be(Some(stored.digest()))
      }
    }
    it("should copy and move, rename an attachement") {
      withTempFolder { folder ⇒
        import TestDSL._

        // create an attachment
        val attachmentOrigin = new File(folder, "test.txt")
        Some(new PrintWriter(attachmentOrigin)).foreach { p ⇒ p.write("hello world"); p.close }

        val graph = Graph[Model]('john1, 'john1, Model.scope, YAMLSerialization.Identifier, UUID.randomUUID(), Element.timestamp(1, 1))
        Reference.register(graph)
        val model = graph.model.eSet('AAAKey, "AAA").eSet('BBBKey, "BBB").eRelative
        val record = model.record('test).eRelative
        val attachment = Attachment[Plain](record, attachmentOrigin.toURI, None)
        record.eSet[Plain]('test, Some(attachment))
        val stored = record.eGet[Plain]('test).get.get
        stored.digest() should be("5eb63bbbe01eeed093cb22bb8f5acdc3")
        stored.name should be("test.txt")
        stored.uri should be('empty)

        graph.storages = graph.storages :+ folder.getAbsoluteFile().toURI()
        stored.uri.head.toString() should endWith("test.txt")

        val recordM1 = Attachment.move[Plain](record, 'test, record, 'test, "test2.txt")
        recordM1.eGet[Plain]('test).get.get.name should be("test2.txt")
        val recordM2 = Attachment.copy[Plain](recordM1, 'test, recordM1, 'test2)
        val attM1 = recordM2.eGet[Plain]('test).get.get
        val attM2 = recordM2.eGet[Plain]('test2).get.get
        attM1.## should be(attM2.##)
        attM1 should be(attM2)
        val recordM3 = Attachment.rename[Plain](recordM2, 'test2, "test321.txt")
        recordM3.eGet[Plain]('test2).get.get.name should be("test321.txt")
        recordM2.eGet[Plain]('test2).get.get.name should be("test2.txt")
        Hash.digest(recordM3.eGet[Plain]('test2).get.get.open, Attachment.digestType) should be(Some("5eb63bbbe01eeed093cb22bb8f5acdc3"))
        Hash.digest(recordM2.eGet[Plain]('test2).get.get.open, Attachment.digestType) should be(Some("5eb63bbbe01eeed093cb22bb8f5acdc3"))
        val ts = Serialization.freeze(graph)
        val graph2 = Serialization.acquire(graph.origin, graph.storages.head, ts)
        val recordFromGraph2 = graph2.model | RecordLocation('test)
        //System.err.println(graph2.model.eDump(false))
        Hash.digest(recordFromGraph2.eGet[Plain]('test).get.get.open, Attachment.digestType) should be(Some("5eb63bbbe01eeed093cb22bb8f5acdc3"))
        Hash.digest(recordFromGraph2.eGet[Plain]('test2).get.get.open, Attachment.digestType) should be(Some("5eb63bbbe01eeed093cb22bb8f5acdc3"))
      }
    }
  }
}
