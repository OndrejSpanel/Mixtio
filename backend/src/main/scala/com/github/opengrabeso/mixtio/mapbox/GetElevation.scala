package com.github.opengrabeso.mixtio
package mapbox

import java.util.concurrent.{Executor, ThreadFactory}

import com.google.code.appengine.awt.image.BufferedImage
import com.google.code.appengine.imageio.ImageIO

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}


object GetElevation {
  import scala.concurrent.ExecutionContext

  // from https://notepad.mmakowski.com/Tech/Scala%20Futures%20on%20a%20Single%20Thread
  // single threaded executor, used to map futures
  implicit val synchronousExecutionContext = ExecutionContext.fromExecutor(new Executor {
    def execute(task: Runnable) = task.run()
  })

  object DefaultThreadFactory {
    implicit object SimpleThreadFactory extends ThreadFactory {
      def newThread(runnable: Runnable) = new Thread(runnable)
    }

  }

  class TileCache {
    val tiles = mutable.Map.empty[(Long, Long, Long), Future[BufferedImage]]

    def tileImage(x: Long, y: Long, z: Long)(implicit threadFactory: ThreadFactory): Future[BufferedImage] = {

      tiles.getOrElseUpdate((x, y, z), {
        val promise = Promise[BufferedImage]()

        val runnable = new Runnable {
          def run() = {
            val timing = shared.Timing.start()
            val domain = "https://api.mapbox.com/v4/"
            val source = s"""mapbox.terrain-rgb/$z/$x/$y.pngraw"""

            // request
            val pars = Map("access_token" -> Main.secret.mapboxToken)

            //timing.logTime(s"Start request $source")
            val request = RequestUtils.buildGetRequest(domain + source, pars)

            val response = request.execute().getContent
            // load PNG
            promise success ImageIO.read(response)
            timing.logTime(s"Read image $source")
          }
        }

        try {
          threadFactory.newThread(runnable).start()
        } catch {
          // when async not working (e.g. over GAE thread per request limit), do the work sync
          case ex: IllegalStateException =>
            println(ex.getMessage)
            runnable.run()
          case ex: Exception =>
            ex.printStackTrace()
            runnable.run()
        }

        promise.future
      })
    }

    private def imageHeight(image: BufferedImage, x: Int, y: Int): Double = {
      val rgb = image.getRGB(x, y)

      val height = -10000 + (rgb & 0xffffff) * 0.1

      height
    }

    private def tileCoord(lon: Double, lat: Double): (Array[Long], Double, Double) = {
      // 16 is max. where neighbourghs have a different value
      // in Europe zoom 16 corresponds approx. 1 px ~ 1m
      val zoom = 13
      //val zoom = 20
      val tf = TileBelt.pointToTileFraction(lon, lat, zoom)
      val tile = tf.map(Math.floor(_).toLong)

      val xp = tf(0) - tile(0)
      val yp = tf(1) - tile(1)
      (tile, xp, yp)
    }

    def apply(lon: Double, lat: Double)(implicit threadFactory: ThreadFactory): Future[Double] = {
      // TODO: four point bilinear interpolation
      val (tile, xp, yp) = tileCoord(lon, lat)

      val image = tileImage(tile(0), tile(1), tile(2))

      image.map { image =>
        val x = Math.floor(xp * image.getWidth).toInt
        val y = Math.floor(yp * image.getHeight).toInt

        imageHeight(image, x, y)
      }
    }

    def possibleRange(lon: Double, lat: Double)(implicit threadFactory: ThreadFactory): Future[(Double, Double)] = {

      val (tile, xp, yp) = tileCoord(lon, lat)

      val image = tileImage(tile(0), tile(1), tile(2))
      image.map { image =>

      val x = Math.floor(xp * image.getWidth).toInt
      val y = Math.floor(yp * image.getHeight).toInt

      // TODO: handle edge pixels correctly
      val x1 = (x + 1) min (image.getWidth - 1)
      val y1 = (y + 1) min (image.getHeight - 1)

      val candidates = Seq(imageHeight(image, x, y), imageHeight(image, x1 , y), imageHeight(image, x, y1), imageHeight(image, x1, y1))
      (candidates.min, candidates.max)
    }
    }

  }

  def apply(lon: Double, lat: Double, cache: TileCache = new TileCache)(implicit threadFactory: ThreadFactory): Double = {
    val ret = cache(lon, lat)
    concurrent.Await.result(ret, Duration.Inf)
  }
}


