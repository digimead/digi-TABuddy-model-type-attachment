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

import java.io.BufferedInputStream
import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.io.InputStream
import java.net.URI

import org.digimead.digi.lib.NotNothing
import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.digi.lib.util.FileUtil
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Reference
import org.digimead.tabuddy.model.element.Value

object Attachment extends Loggable {
  /** Attachment cache directory. */
  protected lazy val attachmentCache = {
    val cacheDirectory = new File(DI.temporaryDirectory, "cache")
    new File(cacheDirectory, Attachment.directoryName)
  }
  /** Directory name. */
  val directoryName = "attachment"
  /** Description suffix. */
  val descriptionSuffix = ".description.yaml"
  /** Attachment scheme. */
  val scheme = "attachment"
  /** Valid file name. */
  val validName = """^[^/?*:;{}\\]+$""".r

  /** Create new attachment that points to external resource. */
  def apply[A <: Attachment.Like[A]: NotNothing: Manifest](container: Element, external: URI,
    previous: Option[_ <: Attachment.Like[_]]): A = {
    val name = external.getScheme() match {
      case null ⇒
        throw new IllegalArgumentException(s"Unable to create attachment for URI ${external} without scheme.")
      case schema ⇒
        external.getPath().split("/").lastOption getOrElse {
          throw new IllegalArgumentException(s"Unable to create attachment for URI ${external} without name.")
        }
    }
    apply[A](container, name, external.toURL().openStream(), previous)
  }
  /** Create new attachment that points to external resource. */
  def apply[A <: Attachment.Like[A]: NotNothing: Manifest](container: Element, name: String, external: InputStream,
    previous: Option[_ <: Attachment.Like[_]])(implicit m: Manifest[A]): A = {
    if (m == implicitly[Manifest[Nothing]])
      throw new IllegalArgumentException("Attachment type is unspecified.")
    if ((validName findFirstIn name).isEmpty)
      throw new IllegalArgumentException(s"Illegal name with characters [/?*:;{}\\]: ${name}.")
    val ref = Reference(container.eGraph.origin, container.eGraph.node.unique, container.eNode.unique, container.eCoordinate)
    val temp = new File(cache, s"TABuddy-Attachment-${container.eNode.unique}-${System.currentTimeMillis()}-${System.nanoTime()}-$name")
    temp.deleteOnExit()
    val is = new BufferedInputStream(external)
    val os = new FileOutputStream(temp)
    try {
      FileUtil.writeToStream(is, os)
    } finally {
      try { is.close } catch { case e: Throwable ⇒ }
      try { os.close } catch { case e: Throwable ⇒ }
    }
    val internal = Left(container, temp)
    val attachmentCtor = m.runtimeClass.getConstructors.find(_.getParameterTypes() match {
      case Array(internalArg, nameArg, ownerArg, previousArg, manifestArg) ⇒
        internalArg.isAssignableFrom(internal.getClass) && nameArg.isAssignableFrom(name.getClass()) &&
          ownerArg.isAssignableFrom(ref.getClass()) && previousArg.isAssignableFrom(previous.getClass())
      case _ ⇒ false
    }) getOrElse {
      throw new NoSuchMethodException(s"Unable to find proper constructor for attachment ${m.runtimeClass}.")
    }
    val attachment = attachmentCtor.newInstance(internal, name, ref, previous, m).asInstanceOf[A]
    if (!attachment.isInstanceOf[Value[_]])
      throw new IllegalArgumentException(s"${m.runtimeClass} is not an isntance of Value[${m.runtimeClass}]")
    attachment
  }
  /** Get attachment cache directory. */
  def cache: File = {
    if (!attachmentCache.isDirectory())
      if (!attachmentCache.mkdirs())
        throw new IOException(s"Could not create directory ${attachmentCache}.")
    attachmentCache
  }
  /** Copy the resource attachment to another element. */
  def copy[A <: Attachment.Like[A]: NotNothing](from: Element, fromValueId: Symbol, to: Element,
    _toValueId: Symbol = null, _name: String = null)(implicit m: Manifest[A]): Element = {
    if (m == implicitly[Manifest[Nothing]])
      throw new IllegalArgumentException("Attachment type is unspecified.")
    if ((from eq to) && _toValueId == null && _name == null)
      return from
    val source = from.eGet[A](fromValueId) match {
      case Some(value) ⇒
        value.get
      case None ⇒
        throw new IllegalArgumentException(s"Unable to find attachment at ${from} for ${fromValueId} with type ${m.runtimeClass.getName()}.")
    }
    val toValueId = if (_toValueId == null) fromValueId else _toValueId
    val name = if (_name == null) source.get.name else _name
    val destination = Attachment[A](to, name, source.get.open, source.previous)
    to.eSet[A](toValueId, Some(destination))
  }
  /** Get digest type. */
  def digestType() = DI.digestType
  /** Move the resource attachment to another element. */
  def move[A <: Attachment.Like[A]: NotNothing](from: Element, fromValueId: Symbol, to: Element,
    _toValueId: Symbol = null, _name: String = null)(implicit m: Manifest[A]): Element = {
    if (m == implicitly[Manifest[Nothing]])
      throw new IllegalArgumentException("Attachment type is unspecified.")
    if ((from eq to) && _toValueId == null && _name == null)
      return from
    val updated = copy(from, fromValueId, to, _toValueId, _name)
    if (from eq to) {
      if (_toValueId != null && _toValueId != fromValueId)
        updated.eRemove[A](fromValueId)
      else
        updated
    } else
      from.eRemove[A](fromValueId)
  }
  /** Rename an attachment. */
  def rename[A <: Attachment.Like[A]: NotNothing: Manifest](element: Element, valueId: Symbol, name: String): Element =
    move[A](element, valueId, element, valueId, name)

  /**
   * Attachment base trait.
   */
  trait Like[A <: Like[A]] extends Value[A] with Equals with java.io.Serializable {
    this: A ⇒
    val internal: Either[(Element, File), Element.Timestamp]
    val name: String
    val owner: Reference
    val previous: Option[_ <: Like[_]]

    /** Get an attachment digest. */
    def digest(): String
    /** Get an attachment value. */
    def get(): A = this
    /** Returns the attachment length. */
    def length(): Long
    /** Returns a stream with an attachment. */
    def open(): InputStream
    /** Get timestamp of the model when element was stored. */
    def stored: Element.Timestamp
    /** Get resource URIs. */
    def uri: Seq[URI]

    override def equals(other: Any) = other match {
      case that: Like[_] ⇒ (this eq that) || (this.canEqual(that) && that.canEqual(this) && this.## == that.##)
      case _ ⇒ false
    }
    override def hashCode() = lazyHashCode
    protected lazy val lazyHashCode = java.util.Arrays.hashCode(Array[AnyRef](this.owner, this.name, this.previous))
    override def toString = s"""Attachment("${name}" at ${owner})"""
  }
  /**
   * Dependency injection routines.
   */
  private object DI extends DependencyInjection.PersistentInjectable {
    /** Attachment digest function. */
    lazy val digestType = injectOptional[String]("DSL.Type.Attachment.Digest") getOrElse "MD5"
    /** Model temporary directory. */
    lazy val temporaryDirectory = inject[File]("Temp")
  }
}
