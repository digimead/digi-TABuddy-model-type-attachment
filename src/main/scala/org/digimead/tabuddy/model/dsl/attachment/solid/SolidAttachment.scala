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
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.IOException
import java.io.InputStream
import java.io.ObjectInputStream
import java.net.URI

import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.future

import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.digi.lib.util.FileUtil
import org.digimead.digi.lib.util.Hash
import org.digimead.tabuddy.model.ReferenceProtos
import org.digimead.tabuddy.model.dsl.DSLType
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Reference
import org.digimead.tabuddy.model.serialization.ProtobufSerialization

class SolidAttachment(
  /** The file/attachment name */
  val name: String,
  /** The attachment creation time from the model point of view */
  val created: Long,
  /** The attachment modification time from the model point of view */
  val modified: Long,
  /** The reference to owner of the attachment (the reference to the property container/element) */
  val owner: Reference) extends Equals with java.io.Serializable {
  private val file = SolidAttachment.DI.getStorage(owner) match {
    case Some(storage) =>
      val container = new File(storage, SolidAttachment.DI.storageName)
      val file = new File(container, name)
      if (!file.exists || !file.canRead)
        throw new IOException(s"element $owner attachment '$name' is not accessible")
      file
    case None =>
      throw new IOException(s"element $owner storage is unreachable")
  }
  /** Attachment size */
  private val attachmentSize = file.length
  /** Attachment modification time */
  private val lastModified = file.lastModified()
  /** MD5 digest of attachment. */
  @transient private lazy val md5 = future { Hash.digest(file, "MD5") }
  md5 // initiate md5 calculation
  assert(created > 0)
  assert(modified > 0)
  assert(modified >= created)

  /** Returns a stream with attachment */
  def attachment(): Option[InputStream] = try {
    if (available && !outdated) Option(new FileInputStream(file)) else None
  } catch {
    case e: Throwable =>
      None
  }
  /** Flag indicating whether the resource is available */
  def available() = file.exists && file.canRead
  /** Throw an exception if something wrong */
  def digest(): Option[String] = try {
    if (available && !outdated) Option(md5.value.get.get.get) else None
  } catch {
    case e: Throwable =>
      None
  }
  /** Returns the flag indicating whether the attachment is not actual */
  def outdated() = file.lastModified() != lastModified
  /** Returns the attachment size */
  def size() = if (attachmentSize <= 0) None else Option(attachmentSize)
  /** Returns a storage location. */
  def storage(): Option[URI] = SolidAttachment.DI.storageFn(owner)

  /** Remove an attachment */
  private[SolidAttachment] def clear() = file.delete()
  /** Recalculate md5 hash after deserialization */
  private def readObject(in: ObjectInputStream) {
    in.defaultReadObject()
    md5 // initiate md5 calculation
  }
  def canEqual(other: Any) = other.isInstanceOf[SolidAttachment]
  override def equals(other: Any) = other match {
    case that: SolidAttachment =>
      (this eq that) || {
        that.canEqual(this) &&
          name == that.name &&
          owner == that.owner &&
          lastModified == that.lastModified &&
          file == that.file
      }
    case _ => false
  }
  override def hashCode() = {
    /*
       * Of the remaining four, I'd probably select P(31), as it's the cheapest to calculate on a
       * RISC machine (because 31 is the difference of two powers of two). P(33) is
       * similarly cheap to calculate, but it's performance is marginally worse, and
       * 33 is composite, which makes me a bit nervous.
       */
    val p = 31
    p * (p * (p * (p + name.hashCode) + owner.hashCode) + file.hashCode) + lastModified.hashCode
  }
}

object SolidAttachment extends Loggable {
  // Valid file name
  val validName = """[^/?*:;{}\\]+""".r

  def apply(name: String, created: Long, modified: Long, owner: Reference, attachment: InputStream): SolidAttachment = {
    val toFile = SolidAttachment.DI.storageFn(owner) match {
      case Some(uri) =>
        val container = new File(new File(uri), SolidAttachment.DI.storageName)
        if (!container.exists())
          if (!container.mkdirs())
            throw new IOException(s"Unable to create container '${container.getAbsolutePath()}' for $owner")
        new File(container, name)
      case None =>
        throw new IOException(s"Storage for $owner is undefined")
    }
    log.debug(s"create SolidAttachment {name: $name, created: $created, modified: $modified, owner: $owner} at " + toFile.getAbsolutePath())
    var to: FileOutputStream = null // Stream to write to destination
    try {
      if (toFile.exists)
        toFile.delete()
      if (!toFile.createNewFile())
        throw new IOException(s"Unable to create attachment '${toFile.getAbsolutePath()}' for $owner")
      to = new FileOutputStream(toFile) // Create output stream
      FileUtil.writeToStream(attachment, to)
    } // Always close the streams, even if exceptions were thrown
    finally {
      if (attachment != null) try {
        attachment.close()
      } catch {
        case e: IOException =>
      }
      if (to != null) try {
        to.close()
      } catch {
        case e: IOException =>
      }
    }
    SolidAttachment(name, created, modified, owner)
  }
  def apply(name: String, owner: Reference): SolidAttachment = {
    val now = System.currentTimeMillis()
    new SolidAttachment(name, now, now, owner)
  }
  def apply(name: String, created: Long, modified: Long, owner: Reference): SolidAttachment =
    new SolidAttachment(name, created, modified, owner)
  def apply(name: String, element: Element.Generic): SolidAttachment = {
    val now = System.currentTimeMillis()
    apply(name, now, now, element.eReference)
  }
  def apply(name: String, created: Long, modified: Long, element: Element.Generic): SolidAttachment =
    apply(name, created, modified, element.eReference)
  def apply(name: String, element: Element.Generic, attachment: InputStream): SolidAttachment = {
    val now = System.currentTimeMillis()
    apply(name, now, now, element.eReference, attachment)
  }
  def apply(name: String, created: Long, modified: Long, element: Element.Generic, attachment: InputStream): SolidAttachment =
    apply(name, created, modified, element.eReference, attachment)
  def apply(name: String, owner: Reference, attachment: File): SolidAttachment = {
    val now = System.currentTimeMillis()
    apply(name, now, now, owner, new FileInputStream(attachment))
  }
  def apply(name: String, created: Long, modified: Long, owner: Reference, attachment: File): SolidAttachment =
    apply(name, created, modified, owner, new FileInputStream(attachment))
  def apply(name: String, element: Element.Generic, attachment: File): SolidAttachment = {
    val now = System.currentTimeMillis()
    apply(name, now, now, element.eReference, new FileInputStream(attachment))
  }
  def apply(name: String, created: Long, modified: Long, element: Element.Generic, attachment: File): SolidAttachment =
    apply(name, created, modified, element.eReference, new FileInputStream(attachment))
  /** clear the resource attachment */
  def clear(resource: SolidAttachment): Boolean = resource.clear()
  /** copy the resource attachment to other element */
  def copy(from: SolidAttachment, to: Element.Generic): SolidAttachment =
    copy(from, to.eReference)
  /** copy the resource attachment to other element */
  def copy(from: SolidAttachment, to: Reference): SolidAttachment =
    SolidAttachment(from.name, from.created, from.modified, to, from.attachment.get)
  /** move the resource attachment to other element */
  def move(from: SolidAttachment, to: Element.Generic): SolidAttachment =
    move(from, to.eReference)
  /** move the resource attachment to other element */
  def move(from: SolidAttachment, to: Reference): SolidAttachment = {
    val result = copy(from, to)
    clear(from)
    result
  }

  class Type extends DSLType with Loggable {
    protected lazy val typeClassSymbolMap = immutable.HashMap[Class[_], Symbol](classOf[SolidAttachment] -> 'SolidAttachment)

    /**
     * Convert value from string
     */
    def convertFromString: PartialFunction[(Symbol, String), _ <: AnyRef with java.io.Serializable] = {
      case ('SolidAttachment, valueData) =>
        val (name, created, modified, data) = {
          val name = valueData.takeWhile(_ != '/')
          val modifiedIndex = valueData.indexOf("/", name.length() + 1)
          val contextIndex = valueData.indexOf("/", modifiedIndex + 1)
          val created = valueData.substring(name.length() + 1, modifiedIndex)
          val modified = valueData.substring(modifiedIndex + 1, contextIndex)
          (name, created, modified, new sun.misc.BASE64Decoder().decodeBuffer(valueData.drop(contextIndex + 1)))
        }
        SolidAttachment(name, created.toLong, modified.toLong, ProtobufSerialization.unpackReference(ReferenceProtos.Reference.parseFrom(data)).get)
    }
    /**
     * Save value to string
     */
    def convertToString: PartialFunction[(Symbol, _ <: AnyRef with java.io.Serializable), String] = {
      case ('SolidAttachment, valueData) =>
        val data = valueData.asInstanceOf[SolidAttachment]
        val encoded = new sun.misc.BASE64Encoder().encode(ProtobufSerialization.packReference(data.owner).toByteArray())
        data.name + "/" + data.created + "/" + data.modified + "/" + encoded
    }
  }
  /**
   * Dependency injection routines
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    /** The storage location Fn DI cache. */
    lazy val storageFn = inject[Reference => Option[URI]]("Element.Storage")
    /** The attachment directory name. */
    lazy val storageName = injectOptional[String]("SolidAttachment") getOrElse "attachmentSolid"

    def getStorage(ref: Reference): Option[File] =
      storageFn(ref).map(new File(_))
  }
}
