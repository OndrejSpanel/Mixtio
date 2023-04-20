package com.github.opengrabeso.mixtio

import java.io._
import java.net.{URLDecoder, URLEncoder}
import java.nio.channels.Channels

import com.google.auth.oauth2.GoogleCredentials

import scala.jdk.CollectionConverters._
import com.google.cloud.storage.{Option => GCSOption, _}
import com.google.cloud.storage.Storage._
import org.apache.commons.io.IOUtils

import scala.reflect.ClassTag

object Storage extends FileStore {


  // from https://cloud.google.com/appengine/docs/standard/java/using-cloud-storage

  final val bucket = "mixtio.appspot.com"

  // full name combined - namespace, filename, user Id
  object FullName {
    def apply(namespace: String, filename: String, userId: String): FullName = {
      // user id needed so that files from different users are not conflicting
      FullName(userId + "/" + namespace + "/" + filename)
    }
    def withMetadata(namespace: String, filename: String, userId: String, metadata: Seq[(String, String)]): FullName = {
      // metadata stored as part of the filename are much quicker to access, filtering is done from them only
      FullName(userId + "/" + namespace + "/" + filename + metadataEncoded(metadata))
    }
  }

  case class FullName(name: String)

  private def fileId(filename: String) = BlobId.of(bucket, filename)

  private def userFilename(namespace: String, filename: String, userId: String) = FullName.apply(namespace, filename, userId)

  def metadataEncoded(metadata: Seq[(String, String)]): String = {
    if (metadata.nonEmpty) {
      metadata.flatMap(kv => Seq(kv._1, kv._2)).map(URLEncoder.encode(_, "UTF-8")).mkString("//","/","")
    } else {
      ""
    }
  }

  def metadataFromFilename(filename: String): Map[String, String] = {
    val split = filename.split("//")
    if (split.size > 1) {
      val md = split(1).split("/")
      def decode(x: String) = URLDecoder.decode(x, "UTF-8")
      md.grouped(2).map {
        case Array(k, v) => decode(k) -> decode(v)
      }.toMap
    } else {
      Map.empty
    }
  }

  val credentials = GoogleCredentials.getApplicationDefault
  val storage = StorageOptions.newBuilder().setCredentials(credentials).build().getService

  def output(filename: FullName, metadata: Seq[(String, String)]): OutputStream = {
    val fid = fileId(filename.name)
    val instance = BlobInfo.newBuilder(fid)
      .setMetadata(metadata.toMap.asJava)
      .setContentType("application/octet-stream").build()
    val channel = storage.writer(instance)
    Channels.newOutputStream(channel)
  }

  def input(filename: FullName): InputStream = {
    // TODO: check if any prefetch or other large data optimization can be used for GCS
    // we used openPrefetchingReadChannel with GAE / gcsService
    //val bufferSize = 1 * 1024 * 1024
    //val readChannel = gcsService.openPrefetchingReadChannel(fileId(filename.name), 0, bufferSize)
    val fid = fileId(filename.name)
    val readChannel = storage.reader(fid)

    Channels.newInputStream(readChannel)
  }

  def store(name: FullName, obj: AnyRef, metadata: (String, String)*) = {
    println(s"store '$name'")
    val os = output(name, metadata)
    val oos = new ObjectOutputStream(os)
    oos.writeObject(obj)
    oos.close()
  }

  def store(
    namespace: String, filename: String, userId: String, obj1: AnyRef, obj2: AnyRef,
    metadata: Seq[(String, String)] = Seq.empty, priorityMetaData: Seq[(String, String)] = Seq.empty
  ) = {
    println(s"store to $namespace: '$filename' - '$userId'")
    val os = output(FullName.withMetadata(namespace, filename, userId, priorityMetaData), metadata)
    val oos = new ObjectOutputStream(os)
    oos.writeObject(obj1)
    oos.writeObject(obj2)
    oos.close()
    os.close()
  }

  def getFullName(stage: String, filename: String, userId: String): FullName = {
    val prefix = FullName(stage, filename, userId)

    val blobs = storage.list(bucket, BlobListOption.prefix(prefix.name))

    val matches = for (iCandidate <- blobs.iterateAll().asScala) yield {
      // do something with the blob
      assert(iCandidate.getName.startsWith(prefix.name))
      iCandidate.getName
    }

    // multiple matches possible, because of -1 .. -N variants added
    // select only real matches
    val realMatches = matches.toList.filter { name =>
      name == prefix.name || name.startsWith(prefix.name + "//")
    }

    if (realMatches.size == 1) {
      FullName(realMatches.head)
    } else prefix
  }



  private def readSingleObject[T: ClassTag](ois: ObjectInputStream) = {
    try {
      val read = ois.readObject()
      read match {
        case Main.NoActivity => None
        case r: T => Some(r)
        case null => None
        case any =>
          val classTag = implicitly[ClassTag[T]]
          throw new InvalidClassException(s"Read class ${any.getClass.getName}, expected ${classTag.runtimeClass.getName}")
      }
    } catch {
      case x: StorageException if x.getCode == 404 =>
        // reading a file which does not exist
        None
    }
  }

  def loadRawName[T : ClassTag](filename: FullName): Option[T] = {
    //println(s"load '$filename' - '$userId'")
    val is = input(filename)
    try {
      val ois = new ObjectInputStream(is)
      readSingleObject[T](ois)
    } finally {
      is.close()
    }

  }

  def load[T : ClassTag](fullName: FullName): Option[T] = {
    object FormatChanged {
      def unapply(arg: Exception): Option[Exception] = arg match {
        case _: java.io.InvalidClassException => Some(arg) // bad serialVersionUID
        case _: ClassNotFoundException => Some(arg) // class / package names changed
        case _: ClassCastException => Some(arg) // class changed (like Joda time -> java.time)
        case _ => None
      }
    }
    try {
      loadRawName(fullName)
    } catch {
      case x: StorageException if x.getCode == 404 =>
        None
      case FormatChanged(x) =>
        println(s"load error ${x.getMessage} - $fullName")
        storage.delete(fileId(fullName.name))
        None
      case x: Exception =>
        x.printStackTrace()
        None
    }
  }

  def load2nd[T : ClassTag](fullName: FullName): Option[T] = {
    //println(s"load '$filename' - '$userId'")
    load[AnyRef, T](fullName).map(_._2)
  }

  def load[T1: ClassTag, T2: ClassTag](fullName: FullName): Option[(T1, T2)] = {
    println(s"Load from '$fullName'")
    val is = input(fullName)
    try {
      val ois = new ObjectInputStream(is)
      val obj1 = readSingleObject[T1](ois)
      obj1.flatMap { o1 =>
        val obj2 = readSingleObject[T2](ois)
        obj2.map(o2 => (o1, o2))
      }.orElse(None)
    } finally {
      is.close()
    }
  }

  def delete(toDelete: FullName): Boolean = {
    storage.delete(fileId(toDelete.name))
  }


  def enumerate(namespace: String, userId: String, filter: Option[String => Boolean] = None): Iterable[(FullName, String)] = {

    def filterByMetadata(name: String, filter: String => Boolean): Option[String] = {
      // filtering can be done only by "priority" (filename) metadata, accessing real metadata is too slow and brings almost no benefit
      Some(name).filter(filter)
    }

    val prefix = userFilename(namespace, "", userId)
    val blobs = storage.list(bucket, BlobListOption.prefix(prefix.name))
    val list = blobs.iterateAll().asScala
    val actStream = for {
      iCandidate <- list
      iName <- filter.map(f => filterByMetadata(iCandidate.getName, f)).getOrElse(Some(iCandidate.getName))
    } yield {
      assert(iName.startsWith(prefix.name))
      FullName(iName) -> iName.drop(prefix.name.length)
    }
    actStream.toVector  // toVector to avoid debugging streams, we are always traversing all of them anyway
  }

  def enumerateAll(): Iterable[String] = {
    val prefix = ""
    val blobs = storage.list(bucket, BlobListOption.prefix(prefix))
    val list = blobs.iterateAll().asScala
    for (i <- list) yield {
      assert(i.getName.startsWith(prefix))
      val name = i.getName.drop(prefix.length)
      name
    }
  }

  def metadata(namespace: String, userId: String, path: String): Seq[(String, String)] = {
    val prefix = userFilename(namespace, path, userId)
    val blobs = storage.list(bucket, BlobListOption.prefix(prefix.name))
    val found = blobs.iterateAll().asScala

    // there should be at most one result
    found.toSeq.flatMap { i =>
      assert(i.getName.startsWith(prefix.name))
      val m = try {
        val md = storage.get(bucket, i.getName, BlobGetOption.fields(BlobField.METADATA))
        md.getMetadata.asScala.toSeq
      } catch {
        case e: Exception =>
          e.printStackTrace()
          Nil
      }
      m
      //println(s"enum '$name' - '$userId': md '$m'")
    }
  }

  def metadataValue(namespace: String, userId: String, path: String, name: String): Option[String] = {
    val md = metadata(namespace, userId, path)
    md.find(_._1 == name).map(_._2)
  }

  def digest(namespace: String, userId: String, path: String): Option[String] = {
    metadataValue(namespace, userId, path, "digest")
  }

  // return true when the digest is matching (i.e. file does not need to be updated)
  def check(namespace: String, userId: String, path: String, digestToCompare: String): Boolean = {
    val oldDigest = digest(namespace, userId, path)
    oldDigest.contains(digestToCompare)
  }

  def updateMetadata(file: String, metadata: Seq[(String, String)]): Boolean = {
    val blobId = fileId(file)
    val md = storage.get(blobId, BlobGetOption.fields(BlobField.METADATA))
    val userData = Option(md.getMetadata).getOrElse(new java.util.HashMap[String, String]).asScala
    val matching = metadata.forall { case (key, name) =>
      userData.get(key).contains(name)
    }
    if (!matching) {
      val md = userData ++ metadata
      val blobInfo = BlobInfo
        .newBuilder(blobId)
        .setMetadata(md.asJava)
        .build()
      storage.update(blobInfo)
    }
    !matching
  }

  def move(oldName: String, newName: String) : Unit = {
    if (oldName != newName) {
      val gcsFilenameOld = fileId(oldName)
      val gcsFilenameNew = fileId(newName)

      // read metadata
      val in = input(FullName(oldName))

      val md = storage.get(bucket, oldName, BlobGetOption.fields(BlobField.METADATA))
      val metadata = md.getMetadata

      val instance = BlobInfo.newBuilder(gcsFilenameNew)
        .setMetadata(metadata)
        .setContentType("application/octet-stream").build()
      val channel = storage.writer(instance)

      val output = Channels.newOutputStream(channel)
      try {
        IOUtils.copy(in, output)
        storage.delete(gcsFilenameOld)
      } finally {
        output.close()
      }
    }
  }

  type FileItem = Blob

  def listAllItems(): Iterable[FileItem] = {

    val blobs = storage.list(bucket)
    val list = blobs.iterateAll().asScala
    list
  }

  def itemModified(item: FileItem) = Option(new java.util.Date(item.getUpdateTime))

  def deleteItem(item: FileItem) = storage.delete(bucket, item.getName)

  def itemName(item: FileItem): String = item.getName

}
