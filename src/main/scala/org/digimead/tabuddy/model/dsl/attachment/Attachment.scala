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
import java.io.BufferedOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.IOException
import java.io.InputStream
import java.net.URI
import java.util.UUID

import scala.collection.immutable
import scala.collection.mutable

import org.digimead.digi.lib.api.DependencyInjection
import org.digimead.digi.lib.log.api.Loggable
import org.digimead.digi.lib.util.FileUtil
import org.digimead.digi.lib.util.Hash
import org.digimead.tabuddy.model.dsl.DSLType
import org.digimead.tabuddy.model.element.Axis
import org.digimead.tabuddy.model.element.Coordinate
import org.digimead.tabuddy.model.element.Element
import org.digimead.tabuddy.model.element.Reference
import org.digimead.tabuddy.model.element.Value
import org.digimead.tabuddy.model.serialization.Serialization
import org.digimead.tabuddy.model.serialization.transport.Transport
import org.digimead.tabuddy.model.serialization.yaml
import org.digimead.tabuddy.model.serialization.yaml.YAML
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.Node
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.representer.{ Represent ⇒ YAMLRepresent }
import org.yaml.snakeyaml.util.UriEncoder

/**
 * Implementation of an attachment object that is serialized to URI:
 * scheme:origin:model-id:model-timestamp-when-created:node-id/name/coordinate1/coordinate2/coord...
 * coordinate is sequence of symbol -> URL encoded value
 *
 * @param name the file/attachment name
 * @param owner a reference to owner of the attachment (the reference to the property container/element)
 * @param _internal temporary file with external resource or timestamp of saved attachment
 */
class Attachment(val internal: Either[(Element, File), Element.Timestamp], val name: String, val owner: Reference,
  val previous: Option[Attachment])(implicit val valueType: Manifest[Attachment]) extends Value[Attachment] with Equals with java.io.Serializable {
  /** Locally cached attachment. */
  @volatile protected var cache: Option[File] = None
  /** Description content. */
  protected lazy val (description, descriptionLocation) = internal match {
    case Left((element, file)) ⇒
      val digest = Hash.digest(file, Attachment.digestType) getOrElse
        { throw new IllegalStateException(s"Unable to calculate digest for ${file}.") }
      (Attachment.Description(digest, file.length()), None)
    case Right(timestamp) ⇒
      getInternalURI(timestamp, Attachment.directoryName, name + Attachment.descriptionSuffix).collectFirst {
        case (storageURI, descriptionURI) ⇒
          Serialization.perScheme.get(descriptionURI.getScheme()) match {
            case Some(transport) ⇒
              Attachment.log.debug(s"Acquire descriptor from ${descriptionURI}.")
              val descriptionContent = transport.read(descriptionURI)
              (yaml.YAML.loadAs(new String(descriptionContent, io.Codec.UTF8.charSet),
                classOf[Attachment.Description]).asInstanceOf[Attachment.Description], Some(storageURI, descriptionURI))
            case None ⇒
              throw new IllegalStateException(s"Transport for the specified scheme '${descriptionURI.getScheme()}' not found.")
          }
      }.getOrElse { throw new IllegalStateException("Unable to load description.") }
  }
  /** External (outside of model) content URI that is hidden from a consumer. */
  protected lazy val externalContentLocation: Option[URI] =
    internal.left.toOption.map(_._2.toURI)
  /** Internal (inside of model) content URI that is hidden from a consumer. */
  protected lazy val internalContentLocation: Option[Seq[(URI, URI)]] =
    internal.right.toOption.map(getInternalURI(_, Attachment.directoryName, name))
  /** Value with a graph modification time. */
  @volatile protected var storedTimestamp = internal.right.toOption

  /** Get an attachment digest. */
  def digest(): String = description.digest
  /** Get a model value. */
  def get() = this
  /** Returns the attachment length. */
  def length() = description.length
  /** Returns a stream with an attachment. */
  def open(): InputStream = synchronized {
    cache.map(new FileInputStream(_)) getOrElse {
      val attachmentURI = externalContentLocation.getOrElse {
        // open internal attachment
        descriptionLocation match {
          case Some((descriptionStorageURI, descriptionURI)) ⇒
            val (storageURI: URI, internalURI: URI) = internalContentLocation.flatMap(_.find { case (storageURI, internalURI) ⇒ storageURI == descriptionStorageURI }).
              getOrElse { throw new IllegalStateException(s"Unable to find suitable attachment URI vs ${descriptionStorageURI}.") }
            internalURI
          case None ⇒
            throw new IllegalStateException("Unable to open attachment without description.")
        }
      }
      Serialization.perScheme.get(attachmentURI.getScheme()) match {
        case Some(transport) ⇒
          if (attachmentURI.getScheme() == "file") {
            /*
             * This is a local file.
             * Creating an additional copy of the local resource makes no sense.
             */
            cache = Some(new File(attachmentURI))
          } else {
            val cacheFile = new File(Attachment.getAttachmentCache, s"${description.digest}-${description.length}")
            cacheFile.deleteOnExit()
            if (!cacheFile.exists()) {
              // There is no cached file. Downloading...
              val bis = new BufferedInputStream(transport.open(attachmentURI))
              val bos = new BufferedOutputStream(new FileOutputStream(cacheFile))
              val buffer = new Array[Byte](4096)
              try {
                Stream.continually(bis.read).takeWhile(_ != -1).foreach(i ⇒ bos.write(buffer, 0, i))
              } finally {
                try { bis.close() } catch { case e: IOException ⇒ }
                try { bos.close() } catch { case e: IOException ⇒ }
              }
            }
            cache = Some(cacheFile)
          }
          new FileInputStream(cache.get)
        case None ⇒
          throw new IllegalStateException(s"Transport for the specified scheme '${attachmentURI.getScheme()}' not found.")
      }
    }
  }
  /** Get timestamp of the model when element was stored. */
  def stored = storedTimestamp getOrElse internal.left.get._1.eGraph.modified
  /** Get resource URIs. */
  def uri: Seq[URI] = internalContentLocation match {
    case Some(seq) ⇒
      seq.map(_._2)
    case None ⇒
      // this value is not saved
      // calculate URIs with current model modification timestamp
      getInternalURI(internal.left.get._1.eGraph.modified, Attachment.directoryName, name).map(_._2)
  }

  /**
   * Calculate sequence of URIs for subject with the specific timestamp.
   *
   * @param modelModificationTimestamp model modification timestamp
   * @param parts path to subject
   * @return Sequence of tuples (storage URI, subject URI)
   */
  protected def getInternalURI(modelModificationTimestamp: Element.Timestamp, parts: String*): Seq[(URI, URI)] =
    Reference.resolve(owner, modelModificationTimestamp) match {
      case Some(element: Element) ⇒
        element.eGraph.storages.map { storageURI ⇒
          Serialization.perScheme.get(storageURI.getScheme()) match {
            case Some(transport) ⇒
              val ancestorsNSelf = element.eNode.safeRead(node ⇒ node.ancestors.reverse :+ node)
              (storageURI, transport.acquireElementLocation(ancestorsNSelf, element.eBox, storageURI, parts: _*))
            case None ⇒
              throw new IllegalStateException(s"Transport for the specified scheme '${storageURI.getScheme()}' not found.")
          }
        }
      case None ⇒
        throw new IllegalStateException(s"Unable to resolve (${owner},${modelModificationTimestamp}).")
    }

  def canEqual(other: Any) = other.isInstanceOf[Attachment]
  override def equals(other: Any) = other match {
    case that: Attachment ⇒ (this eq that) || (that.canEqual(this) && this.## == that.##)
    case _ ⇒ false
  }
  override def hashCode() = lazyHashCode
  protected lazy val lazyHashCode = java.util.Arrays.hashCode(Array[AnyRef](this.description, this.owner, this.name, this.previous))
  override def toString = s"""Attachment("${name}" at ${owner})"""
}

object Attachment extends Loggable {
  /** Attachment cache directory. */
  lazy val attachmentCache = {
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
  val validName = """[^/?*:;{}\\]+""".r

  /** Create new attachment that points to external resource. */
  def apply(container: Element, external: URI, previous: Option[Attachment]): Attachment = {
    val name = external.getScheme() match {
      case null ⇒
        throw new IllegalArgumentException(s"Unable to create attachment for URI ${external} without scheme.")
      case schema ⇒
        external.getPath().split("/").lastOption getOrElse {
          throw new IllegalArgumentException(s"Unable to create attachment for URI ${external} without name.")
        }
    }
    apply(container, name, external.toURL().openStream(), previous)
  }
  /** Create new attachment that points to external resource. */
  def apply(container: Element, name: String, external: InputStream, previous: Option[Attachment]): Attachment = {
    val ref = Reference(container.eGraph.origin, container.eGraph.node.unique, container.eNode.unique, container.eCoordinate)
    val temp = new File(getAttachmentCache, s"TABuddy-Attachment-${container.eNode.unique}-${System.currentTimeMillis()}-${System.nanoTime()}-$name")
    temp.deleteOnExit()
    val is = new BufferedInputStream(external)
    val os = new FileOutputStream(temp)
    try {
      FileUtil.writeToStream(is, os)
    } finally {
      try { is.close } catch { case e: Throwable ⇒ }
      try { os.close } catch { case e: Throwable ⇒ }
    }
    /* pass reference for
     *   origin
     *   modelId
     *   nodeId
     *   coordinate
     * name
     * created as None (current model modification time)
     * modified as None (current model modification time)
     * temporary file
     */
    new Attachment(Left(container, temp), name, ref, previous)
  }
  /** Copy the resource attachment to other element */
  def copy(from: Attachment, to: Element): Attachment =
    copy(from, to.eReference)
  /** Copy the resource attachment to other element */
  def copy(from: Attachment, to: Reference): Attachment =
    new Attachment(from.internal, from.name, to, from.previous)
  /** Get digest type. */
  def digestType() = DI.digestType

  /** Get attachment cache directory. */
  protected def getAttachmentCache(): File = {
    if (!attachmentCache.isDirectory())
      if (!attachmentCache.mkdirs())
        throw new IOException(s"Could not create directory ${attachmentCache}.")
    attachmentCache
  }

  class Construct extends YAML.constructor.CustomConstruct {
    protected val keyTypes = immutable.HashMap[String, PartialFunction[Node, Unit]]()

    YAML.constructor.getYAMLConstructors.put(new Tag(classOf[Description]), this)

    def constructCustom(map: mutable.HashMap[String, AnyRef]): AnyRef =
      Description(map("digest").asInstanceOf[String], map("length") match {
        case i: java.lang.Integer ⇒ i.toLong
        case l: java.lang.Long ⇒ l
        case n ⇒ throw new YAMLException(s"Unexpected length type ${n.getClass}.")
      })
  }
  class Represent extends YAMLRepresent {
    YAML.representer.getMultiRepresenters.put(classOf[Description], this)

    def representData(data: AnyRef): Node = {
      val description = data.asInstanceOf[Description]
      val map = new java.util.TreeMap[String, AnyRef]()
      map.put("digest", description.digest)
      map.put("length", description.length: java.lang.Long)
      YAML.representer.representMapping(Tag.MAP, map, null)
    }
  }
  /**
   * Attachment description.
   *
   * @param attachment digest
   * @param attachment length
   */
  case class Description(val digest: String, val length: Long)
  class Type extends DSLType with Loggable {
    protected lazy val typeClassSymbolMap = immutable.HashMap[Class[_], Symbol](classOf[Attachment] -> 'Attachment)

    /** Commit complex property (if needed) while saving. */
    def commit(typeSymbol: Symbol, value: AnyRef with java.io.Serializable,
      element: Element, transport: Transport, elementDirectoryURI: URI) = synchronized {
      value match {
        case attachment: Attachment ⇒
          if (element.eReference == attachment.owner && attachment.internal.isLeft) {
            Attachment.log.debug(s"Commit value ${this} for ${element}.")
            val attachmentURI = transport.append(elementDirectoryURI, Attachment.directoryName, attachment.name)
            log.debug(s"Freeze attachment to ${attachmentURI}.")
            transport.write(attachment.open(), attachmentURI)
            val descriptionURI = transport.append(elementDirectoryURI, Attachment.directoryName, attachment.name + Attachment.descriptionSuffix)
            log.debug(s"Freeze description to ${descriptionURI}.")
            transport.write(YAML.dump(attachment.description).getBytes(io.Codec.UTF8.charSet), descriptionURI)
            if (attachment.storedTimestamp.isEmpty)
              attachment.storedTimestamp = Some(element.eGraph.modified)
          }
        case other ⇒
          throw new IllegalArgumentException(s"Unable to commit unknown value ${value}.")
      }
    }
    /** Load value from string. */
    def convertFromString: PartialFunction[(Symbol, String), _ <: AnyRef with java.io.Serializable] = {
      case ('Attachment, valueData) ⇒
        val uri = URI.create(valueData)
        if (uri.getScheme != Attachment.scheme)
          throw new IllegalStateException(s"Unknown attachment scheme for ${uri}.")
        uri.getSchemeSpecificPart().split("/") match {
          case Array(prefix, name, coords @ _*) ⇒
            val Array(origin, model, stored, node) = prefix.split(":")
            val coordinates = coords.map { coordinate ⇒
              val Array(idName, value) = coordinate.split("→")
              val Array(valueTypeName, valueData) = value.split(":")
              val valueTypeSymbol = Symbol(valueTypeName)
              DSLType.convertFromString(valueTypeSymbol, UriEncoder.decode(valueData)) match {
                case Some(value) ⇒
                  Axis(Symbol(idName), value)(Manifest.classType(DSLType.symbolClassMap(valueTypeSymbol)))
                case None ⇒ throw new IllegalStateException(s"Unable to load value '${UriEncoder.decode(valueData)}'  with unknown type ${valueTypeName}.")
              }
            }
            new Attachment(Right(yaml.Timestamp.load(stored)), UriEncoder.decode(name),
              Reference(Symbol(origin), UUID.fromString(model), UUID.fromString(node), Coordinate(coordinates: _*)),
              None)
        }
    }
    /** Save value to string. */
    def convertToString: PartialFunction[(Symbol, _ <: AnyRef with java.io.Serializable), String] = {
      case ('Attachment, valueData) ⇒
        val data = valueData.asInstanceOf[Attachment]
        val coordinate = data.owner.coordinate.coordinate.toSeq.map { coordinate ⇒
          val coordinateValue = DSLType.classSymbolMap.get(coordinate.m.runtimeClass).
            map(symbolType ⇒ DSLType.convertToString(symbolType, coordinate.value) match {
              case Some(value) ⇒
                coordinate.id + "→" + symbolType.name + ":" + UriEncoder.encode(value)
              case None ⇒
                throw new IllegalStateException(s"Unable to save value '${coordinate.value}' with unknown type ${symbolType.name}.")
            })
        }
        val schemeSpecificPath = Seq(Seq(Seq(data.owner.origin.name, data.owner.model, yaml.Timestamp.dump(data.stored),
          data.owner.node).mkString(":")), UriEncoder.encode(data.name) +: coordinate).flatten.mkString("/")
        new URI(Attachment.scheme, schemeSpecificPath, null).toString
    }
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
