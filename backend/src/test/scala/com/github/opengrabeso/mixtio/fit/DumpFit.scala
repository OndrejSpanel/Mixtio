package com.github.opengrabeso.mixtio.fit

import java.io.{FileOutputStream, InputStream, PrintStream}

import com.garmin.fit._

import scala.collection.JavaConverters._
import org.scalatest.{FlatSpec, Matchers}

class DumpFit extends FlatSpec with Matchers {

  behavior of "Decoder"

  it should "dump a fit file" in {
    val in = getClass.getResourceAsStream("/decodeTest.fit")
    decodeFile(in)
  }

  it should "dump extra information from a fit file" in {
    val in = getClass.getResourceAsStream("/decodeTestExt.fit")
    decodeFile(in, "record")
  }

  it should "dump device information from a Quest fit file" in {
    val in = getClass.getResourceAsStream("/decodeTestQuest.fit")
    decodeFile(in) //, "record")
  }

  ignore should "ignore" in {

    it should "dump a fit file exported from Movescount" in {
      val in = getClass.getResourceAsStream("/decodeFitMC.fit")
      decodeFileToFile("decodeFitMC.txt", in)
    }

    it should "dump a fit file exported from this app" in {
      val in = getClass.getResourceAsStream("/decodeFitMy.fit")
      decodeFileToFile("decodeFitMy.txt", in)
    }



    "Output fit" should "contain laps" in {
      val in = getClass.getResourceAsStream("/testoutputLaps.fit")
      decodeFile(in, "record")
    }

    "Exported fit" should "contain laps" in {
      val in = getClass.getResourceAsStream("/exportedLaps.fit")
      decodeFile(in, "record")
    }
  }

  def decodeFileToOutput(output: String => Unit, in: InputStream, ignoreMessages: String*): Unit = {
    val decode = new Decode
    try {
      val listener = new MesgListener {
        override def onMesg(mesg: Mesg): Unit = {
          output(s"${mesg.getName}")
          if (!ignoreMessages.contains(mesg.getName)) {
            val fields = mesg.getFields.asScala
            for (f <- fields) {
              f.getName match {
                case "timestamp" | "start_time" =>
                  val time = new DateTime(f.getLongValue)
                  output(s"  ${f.getName}:${time.toString}")
                case _ =>
                  output(s"  ${f.getName}:${f.getValue}")
              }
            }
          }
        }
      }
      decode.read(in, listener)
    } finally {
      in.close()
    }
  }

  def decodeFile(in: InputStream, ignoreMessages: String*): String = {
    val builder = new StringBuilder
    decodeFileToOutput(builder ++= _, in, ignoreMessages:_*)
    builder.toString
  }

  def decodeFileToFile(outName: String, in: InputStream, ignoreMessages: String*): Unit = {
    val out = new FileOutputStream(outName)
    val print = new PrintStream(out)
    decodeFileToOutput(print.println, in, ignoreMessages:_*)
    out.close()
    print.close()
  }

}
