package com.github.opengrabeso.mixtio
package requests

import java.io.{ByteArrayInputStream, InputStream, ObjectInputStream}

import Main.NoActivity
import shared.Timing
import org.apache.commons.io.IOUtils

import scala.util.Try

object Upload extends ActivityStorage {
  def storeFromStreamWithDigest(userId: String, name: String, timezone: String, stream: InputStream, digest: String): Seq[ActivityEvents] = {
    import MoveslinkImport._
    val timing = Timing.start()

    val extension = name.split('.').last
    val actData: Seq[ActivityEvents] = extension.toLowerCase match {
      case "fit" =>
        FitImport(name, digest, stream).toSeq
      case "sml" =>
        loadSml(name, digest, stream).toSeq
      case "gpx" =>
        GpxImport(name, digest, stream).toOption.toSeq
      case "tcx" =>
        TcxImport(name, digest, stream).toOption.toSeq
      case "xml" =>
        loadXml(name, digest, stream, timezone).zipWithIndex.flatMap { case (act,index) =>
          // some activities (Quest) have more parts, each part needs a distinct name
          val nameWithIndex = if (index > 0) s"$name-$index" else name
          loadFromMove(nameWithIndex, digest, act)
        }
      case _ =>
        // unknown extension, try deserialization, might be a file extracted from the server Cloud Storage
        Try {
          val ois = new ObjectInputStream(stream)
          val _ = ois.readObject()
          val obj = ois.readObject()
          obj match {
            case act: ActivityEvents =>
              act
          }
        }.toOption.toSeq
    }
    timing.logTime("Import file")
    val ret = if (actData.nonEmpty) {
      actData.flatMap { act =>
        val oldDigest = Storage.digest(Main.namespace.stage, userId, act.id.id.filename)
        if (oldDigest.contains(act.id.digest)) {
          // exact duplicate
          None
        } else {
          // same name, different content - create a unique name
          val actOpt = act.cleanPositionErrors.copy(id = act.id.copy(name = act.id.name + System.currentTimeMillis().toString))
          storeActivity(Main.namespace.stage, actOpt, userId)
          Some(actOpt)
        }
      }
    } else {
      Storage.store(Main.namespace.stage, name, userId, NoActivity, NoActivity, Seq("digest" -> digest))
      Nil
    }
    timing.logTime("Store file")
    ret
  }

  def storeFromStream(userId: String, name: String, timezone: String, streamOrig: InputStream): Seq[ActivityEvents] = {
    val fileBytes = IOUtils.toByteArray(streamOrig)
    val digest = Main.digest(fileBytes)

    val stream = new ByteArrayInputStream(fileBytes)

    storeFromStreamWithDigest(userId, name, timezone, stream, digest)
  }

}
