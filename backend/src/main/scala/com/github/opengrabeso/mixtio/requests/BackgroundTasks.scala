package com.github.opengrabeso.mixtio
package requests

import java.util.concurrent.{ConcurrentLinkedQueue, Semaphore, ThreadFactory}
import com.google.appengine.api.ThreadManager
import com.google.appengine.api.utils.SystemProperty
import com.google.cloud.tasks.v2._
import com.google.protobuf.ByteString

import java.nio.charset.Charset

object BackgroundTasks {
  trait TaskDescription[Parameters] {
    def execute(pars: Parameters): Unit
    // TODO: derive programatically?
    def path: String
  }

  trait Tasks {
    def addTask[T](t: TaskDescription[T], pars: T, eta: Long): Unit
  }

  object LocalTaskQueue extends Runnable with Tasks with ThreadFactory {
    val q = new ConcurrentLinkedQueue[(TaskDescription[_], Any, Long)]
    val issued = new Semaphore(0)

    // initialization will be called on a first access (when first task is added)
    val thread = new Thread(this)
    thread.setDaemon(true)
    thread.start()

    def addTask[T](t: TaskDescription[T], pars: T, eta: Long) = {
      q.add((t, pars, eta))
      issued.release()
    }

    @scala.annotation.tailrec
    def run() = {
      issued.acquire(1)
      val t = q.poll()
      t match {
        case (task, pars, eta) =>
          val now = System.currentTimeMillis()
          if (eta > now) {
            q.add((task, pars, eta))
          }
          try {
            task.asInstanceOf[TaskDescription[Any]].execute(pars)
          } catch {
            case ex: Exception =>
              println("Exception while processing a task")
              ex.printStackTrace()
          }
          run()
      }
    }
    def newThread(r: Runnable) = {
      val thread = new Thread(r)
      thread
    }
  }

  object CloudTaskQueue extends Tasks {
    val client = CloudTasksClient.create()

    def addTask[T](task: TaskDescription[T], pars: T, eta: Long): Unit = {
      // see https://cloud.google.com/tasks/docs/creating-appengine-tasks
      // TODO: obtain location from some configuration / API?
      val queuePath = QueueName.of("mixtio", "europe-west3", "default").toString

      /*
      val taskBuilder = Task.newBuilder()
          .setAppEngineHttpRequest(
            AppEngineHttpRequest.newBuilder()
              .setBody(ByteString.copyFrom(payload, Charset.defaultCharset()))
              .setRelativeUri("/tasks/create")
              .setHttpMethod(HttpMethod.POST)
              .build()
          )
      */
      //val queue = QueueFactory.getDefaultQueue
      //queue add TaskOptions.Builder.withPayload(task)
      ???
    }
  }

  private val appEngine = SystemProperty.environment.value() != null

  def addTask[T](task: TaskDescription[T], pars: T, eta: Long): Unit = {
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
