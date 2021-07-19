package com.github.opengrabeso.mixtio
package requests

import com.avsystem.commons.JStringBuilder
import com.avsystem.commons.serialization.GenCodec
import com.avsystem.commons.serialization.json.JsonStringOutput

import java.util.concurrent.{PriorityBlockingQueue, ThreadFactory}
import com.google.appengine.api.ThreadManager
import com.google.appengine.api.utils.SystemProperty
import com.google.cloud.tasks.v2._
import com.google.protobuf.{ByteString, Timestamp}

import collection.JavaConverters._
import java.nio.charset.StandardCharsets

object BackgroundTasks {
  trait TaskDescription[Parameters] {
    def execute(pars: Parameters): Unit
    // TODO: derive programatically?
    def path: String
  }

  trait Tasks {
    def addTask[T: GenCodec](t: TaskDescription[T], pars: T, eta: Long): Unit
  }

  object LocalTaskQueue extends Runnable with Tasks with ThreadFactory {
    private val logging = false

    case class QueueItem(task: TaskDescription[_], pars: Any, eta: Long) extends Comparable[QueueItem] {
      override def compareTo(o: QueueItem) = {
        if (this.eta < o.eta) -1
        else if (this.eta > o.eta) +1
        else 0
      }
    }

    val q = new PriorityBlockingQueue[QueueItem]

    // initialization will be called on a first access (when first task is added)
    val thread = new Thread(this)
    thread.setDaemon(true)
    thread.start()

    def addTask[T: GenCodec](t: TaskDescription[T], pars: T, eta: Long) = {
      if (logging) println(s"addTask ${t.path}, $eta (${eta - System.currentTimeMillis()} from now)")
      q.add(QueueItem(t, pars, eta))
    }

    @scala.annotation.tailrec
    def run() = {
      val item = q.take()
      val now = System.currentTimeMillis()
      if (item.eta > now) {
        if (logging) {
          println(s"Queue wait ${item.eta - now}")
        }
        // even the most urgent task is not needed yet, wait for a while before trying again
        // not very sophisticated, but should work reasonably well
        Thread.sleep(100)
        q.add(item)
      } else {
        if (logging) println(s"Run task ${item.task.path}")
        try {
          item.task.asInstanceOf[TaskDescription[Any]].execute(item.pars)
        } catch {
          case ex: Exception =>
            println("Exception while processing a task")
            ex.printStackTrace()
        }
      }
      run()
    }
    def newThread(r: Runnable) = {
      val thread = new Thread(r)
      thread
    }
  }

  object CloudTaskQueue extends Tasks {
    val client = CloudTasksClient.create()

    def addTask[T: GenCodec](task: TaskDescription[T], pars: T, eta: Long): Unit = {
      // see https://cloud.google.com/tasks/docs/creating-appengine-tasks
      // TODO: obtain location from some configuration / API?
      val queuePath = QueueName.of("mixtio", "europe-west3", "default").toString
      val codec = implicitly[GenCodec[T]]

      val builder = new JStringBuilder
      val output = new JsonStringOutput(builder)
      codec.write(output, pars)
      val body =
        s"""{
           |"pars" : ${builder.toString}
           |}""".stripMargin

      val taskBuilder = Task.newBuilder()
          .setAppEngineHttpRequest(
            AppEngineHttpRequest.newBuilder()
              .setBody(ByteString.copyFrom(body, StandardCharsets.UTF_8))
              .putAllHeaders(Map("Content-Type" -> "application/json").asJava)
              .setRelativeUri(task.path)
              .setHttpMethod(HttpMethod.POST)
              .build()
          )

      val now = System.currentTimeMillis()
      if (eta > now) {
        taskBuilder.setScheduleTime(Timestamp.newBuilder.setSeconds(eta / 1000))
      }

      val cloudTask = client.createTask(queuePath, taskBuilder.build)
    }
  }

  // Cloud tasks are suppoted only on the real (production) environment
  private val appEngine = SystemProperty.environment.value() == SystemProperty.Environment.Value.Production

  def addTask[T: GenCodec](task: TaskDescription[T], pars: T, eta: Long): Unit = {
    if (appEngine) {
      CloudTaskQueue.addTask[T](task, pars, eta)
    } else {
      LocalTaskQueue.addTask[T](task, pars, eta)
    }
  }

  def currentRequestThreadFactory: ThreadFactory = {
    if (appEngine) {
      ThreadManager.currentRequestThreadFactory
    } else {
      LocalTaskQueue
    }
  }
}
