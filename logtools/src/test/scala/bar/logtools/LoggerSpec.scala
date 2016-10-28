// See LICENSE for license details.

package bar.logtools

import java.io.{PrintStream, ByteArrayOutputStream}

import org.scalatest.{FreeSpec, Matchers}

class LoggerSpec extends FreeSpec with Matchers {
  def setStream(): ByteArrayOutputStream = {
    val byteStream = new ByteArrayOutputStream()
    Logger.setStream(new PrintStream(byteStream))
    byteStream
  }

  "should be able to send output to other places" in {
    val byteStream = setStream()

    class StreamTest extends LazyLogging {
      logger.error("Error")
    }
    new StreamTest

    byteStream.toString should include ("Error")
  }

  "The default level is error" in {
    val byteStream = setStream()

    class StreamTest extends LazyLogging {
      logger.error("Error")
      logger.debug("Debug")
      logger.warn("Warn")
      logger.info("Info")
    }

    new StreamTest

    byteStream.toString should include ("Error")
    byteStream.toString should not include "Debug"
    byteStream.toString should not include "Warn"
    byteStream.toString should not include "Info"
  }

  "The log level can be set on fly" in {
    val byteStream = setStream()
    Logger.setGlobalLevel(LogLevel.Info)

    class StreamTest extends LazyLogging {
      logger.error("Error")
      logger.debug("Debug")
      logger.warn("Warn")
      logger.info("Info")
    }

    new StreamTest

    byteStream.toString should include ("Error")
    byteStream.toString should not include "Debug"
    byteStream.toString should include ("Warn")
    byteStream.toString should include ("Info")
  }

  "The log level can be set to a specific level for a specific class" in {
    val byteStream = setStream()
    Logger.setClassLogLevels(Map("bar.logtools.StreamTest2" -> LogLevel.Debug))

    new StreamTest1
    new StreamTest2

    byteStream.toString should include ("Error1")
    byteStream.toString should not include "Debug1"
    byteStream.toString should not include "Warn1"
    byteStream.toString should not include "Info1"

    byteStream.toString should include ("Error2")
    byteStream.toString should include ("Debug2")
    byteStream.toString should include ("Warn2")
    byteStream.toString should include ("Info2")
  }
}

class StreamTest1 extends LazyLogging {
  logger.error("Error1")
  logger.debug("Debug1")
  logger.warn("Warn1")
  logger.info("Info1")
}
class StreamTest2 extends LazyLogging {
  //println(s"Class name is ${this.getClass.getName}")
  logger.error("Error2")
  logger.debug("Debug2")
  logger.warn("Warn2")
  logger.info("Info2")
}


