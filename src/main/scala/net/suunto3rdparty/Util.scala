package net.suunto3rdparty

import java.io.File
import java.time.format.DateTimeFormatter

object Util {
  val dateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss")

  def kiloCaloriesFromKilojoules(kj: Double): Int = (kj / 4184).toInt

  def isWindows: Boolean = {
    val OS = System.getProperty("os.name").toLowerCase
    OS.contains("win")
  }
  def isMac: Boolean = {
    val OS = System.getProperty("os.name").toLowerCase
    OS.contains("mac")
  }
  def isUnix: Boolean = {
    val OS = System.getProperty("os.name").toLowerCase
    OS.contains("nix") || OS.contains("nux") || OS.contains("aix")
  }
  def getSuuntoHome: File = {
    if (Util.isWindows) {
      val appData = System.getenv("APPDATA")
      return new File(new File(appData), "Suunto")
    }
    if (Util.isMac) {
      val userHome = System.getProperty("user.home")
      return new File(new File(userHome), "Library/Application Support/Suunto/")
    }
    if (Util.isUnix) {
      val userHome = System.getProperty("user.home")
      return new File(new File(userHome), "Suunto")
    }
    throw new UnsupportedOperationException("Unknown operating system")
  }
}
