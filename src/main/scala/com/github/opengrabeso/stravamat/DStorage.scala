package com.github.opengrabeso.stravamat

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import Main._
import org.joda.time.{Days, DateTime => ZonedDateTime}
import com.google.appengine.api.datastore._

import scala.reflect.ClassTag
import scala.util.Try
import scala.collection.JavaConverters._

object DStorage {

  private val datastore = DatastoreServiceFactory.getDatastoreService

  private def userFilename(namespace: String, filename: String, userId: String) = {
    userId + "/" + namespace + "/" + filename
  }


  def transaction[T](f: Transaction => T): T = {
    val txn = datastore.beginTransaction()
    try {
      val ret = f(txn)
      txn.commit()
      ret
    } catch {
      case ex: java.util.ConcurrentModificationException =>
        // may be contention or optimistic lock failure, we do not care, retry
        // should we have some upper limit on retries?
        println(ex.getMessage)
        if (ex.getMessage.startsWith("too much contention")) {
          Thread.sleep(500)
        }
        transaction[T](f)

    } finally {
      if (txn.isActive) txn.rollback()
    }
  }


  private def storeToBlob(obj: AnyRef) = {
    val baos = new ByteArrayOutputStream()
    val os = new ObjectOutputStream(baos)
    os.writeObject(obj)
    os.close()

    new Blob(baos.toByteArray)
  }

  private def initializeInTransaction(namespace: String, filename: String, userId: String, init: Entity => Unit = _ => ())(implicit t: Transaction): Key = {
    val fName = userFilename(namespace, filename, userId)
    // create as needed
    val userIdKey = KeyFactory.createKey("filename", fName)
    val user = try {
      val user = datastore.get(userIdKey)
      init(user)
      user
    } catch {
      case _: EntityNotFoundException =>
        val user = new Entity("filename", fName)
        init(user)
        user
    }
    datastore.put(t, user)
    userIdKey
  }

  private def storeInTransaction(fileKey: Key, obj: AnyRef)(t: Transaction) = {

    val cls = obj.getClass.getName

    val objData = new Entity(cls, "data", fileKey)

    val blob = storeToBlob(obj)

    objData.setProperty("blob", blob)
    setModifiedNow(objData)
    datastore.put(t, objData)
  }

  private def setModifiedNowInTransaction(fileKey: Key)(t: Transaction): Unit = {
    val fileEntity = datastore.get(t, fileKey)
    setModifiedNow(fileEntity)
    datastore.put(t, fileEntity)

  }

  private def setModifiedNow(e: Entity): Unit = {
    val now = new java.util.Date()
    e.setProperty("modified", now)
  }


  private def loadInTransaction[T <: AnyRef : ClassTag](fileKey: Key)(t: Transaction): T = {
    val classTag = implicitly[ClassTag[T]]
    val cls = classTag.runtimeClass.getName

    val dataKey = KeyFactory.createKey(fileKey, cls, "data")
    val objData = datastore.get(t, dataKey)

    val dataBlob = objData.getProperty("blob").asInstanceOf[Blob]

    val bais = new ByteArrayInputStream(dataBlob.getBytes)
    val is = new ObjectInputStream(bais)
    val obj = is.readObject().asInstanceOf[T]
    obj
  }

  def store(namespace: String, filename: String, userId: String, obj: AnyRef) = {
    transaction { t =>
      val key = initializeInTransaction(namespace, filename, userId, setModifiedNow)(t)
      storeInTransaction(key, obj)(t)
    }
  }


  def load[T <: AnyRef: ClassTag](namespace: String, filename: String, userId: String): Option[T] = {
    Try {
      transaction { t =>
        val key = initializeInTransaction(namespace, filename, userId)(t)
        loadInTransaction[T](key)(t)
      }
    }.toOption
  }

  def modify[T <: AnyRef: ClassTag](namespace: String, filename: String, userId: String)(mod: T => T) = {
    transaction { t =>
      val key = initializeInTransaction(namespace, filename, userId, setModifiedNow)(t)
      val obj = loadInTransaction[T](key)(t)
      val objMod = mod(obj)
      storeInTransaction(key, objMod)(t)
    }
  }

  def cleanup(): Int = {
    // DRY: Storage.cleanup
    val query = new Query("filename")
    val prepQuery = datastore.prepare(query)
    val list = prepQuery.asIterable().asScala
    val now = new ZonedDateTime()
    for (i <- list) yield {
      val key = i.getKey

      //println(s"enum '$name' - '$userId': md '$m'")
      val name = key.getName
      val maxAgeInDays = if (name.contains("/" + namespace.uploadProgress + "/")) {
        Some(2)
      } else None

      val cleaned = for {
        maxDays <- maxAgeInDays
        modProp <- Option(i.getProperty("modified").asInstanceOf[java.util.Date])
      } yield {
        val fileTime = new ZonedDateTime(modProp)
        val age = Days.daysBetween(fileTime, now).getDays
        if (age >= maxDays) {
          datastore.delete(key)
          println(s"Cleaned $name age $age, date $fileTime")
          1
        } else {
          0
        }


      }
    }
    0
  }

}