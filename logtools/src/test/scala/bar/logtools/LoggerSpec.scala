// See LICENSE for license details.

package bar.logtools

import java.io.{PrintStream, ByteArrayOutputStream}

import org.scalatest.{FreeSpec, Matchers}

class LoggerSpec extends FreeSpec with Matchers {

  "should be able to send output to other places" in {
    val byteStream = new ByteArrayOutputStream()

    Logger.setStream(new PrintStream(byteStream))

    class StreamTest extends LazyLogging {
      logger.error("Error String")
    }

    val st = new StreamTest

    println(s"stream is ${byteStream.toString}")
  }
}
