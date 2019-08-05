import io.maana.PEGTestParser
import org.parboiled2._
import org.scalatest._

import scala.util.{Failure, Success}

class ParserSpec extends WordSpec with Matchers {
  "A PEGTestParser" should {
    "add" in {
      val parser: PEGTestParser = new PEGTestParser("-123")
      parser.ExpressionInputLine.run() match {
        case Success(x) => {
          x shouldBe -123
        }
        case Failure(e: ParseError) => fail("Expression is not valid: " + parser.formatError(e))
        case Failure(e) => fail("Unexpected error during parsing run: " + e)
      }
    }
  }
}
