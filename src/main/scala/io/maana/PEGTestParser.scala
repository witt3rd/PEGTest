package io.maana

import org.parboiled2._

class PEGTestParser(val input: ParserInput) extends Parser {

  def LabelInputLine = rule {
    Label ~ EOI
  }

  def ExpressionInputLine = rule {
    Expression ~ EOI
  }

  // ---

  def Label: Rule1[String] = rule {
    capture(optional('_') ~ oneOrMore(d))
  }

  def Expression: Rule1[Int] = rule {
    Term ~ zeroOrMore(
      '+' ~ Term ~> ((_: Int) + _)
        | '-' ~ Term ~> ((_: Int) - _))
  }

  def Term = rule {
    Factor ~ zeroOrMore(
      '*' ~ Factor ~> ((_: Int) * _)
        | '/' ~ Factor ~> ((_: Int) / _))
  }

  def Factor = rule {
    Number | Parens
  }

  def Parens = rule {
    '(' ~ Expression ~ ')'
  }

  def Number = rule {
    capture(optional('-') ~ Digits) ~> (_.toInt)
  }

  def Digits = rule {
    oneOrMore(CharPredicate.Digit)
  }
}