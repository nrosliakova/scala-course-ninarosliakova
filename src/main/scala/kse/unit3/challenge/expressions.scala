package kse.unit3.challenge

import scala.annotation.{tailrec, targetName}

object expressions:

  sealed trait Expression:
    def evaluate: Expression
    def substitute(variable: Variable, expression: Expression): Expression

  sealed trait Boolean extends Expression:
    val evaluate: Expression                                                 = this
    def substitute(variable: Variable, substitution: Expression): Expression = this

  type True = True.type
  case object True extends Boolean

  type False = False.type
  case object False extends Boolean

  case class Variable(name: String) extends Expression:
    val evaluate: Expression                                               = this
    def substitute(variable: Variable, expression: Expression): Expression =
      if this == variable then expression
      else this

  case class Negation(expression: Expression) extends Expression:
    def evaluate: Expression                                                 =
      expression.evaluate match
        case True => False
        case False => True
        case Negation(exp) => exp.evaluate
        case exp => Negation(exp)

    def substitute(variable: Variable, substitution: Expression): Expression =
      Negation(expression.substitute(variable, substitution))
    override def toString: String = s"!$expression"

  case class Conjunction(left: Expression, right: Expression) extends Expression:
    def evaluate: Expression                                                 =
      val l = left.evaluate
      val r = right.evaluate
      (l, r) match
        case (False, _) => False
        case (_, False) => False
        case (True, x) => x
        case (x, True) => x
        case (x, y) => Conjunction(x, y)

    def substitute(variable: Variable, substitution: Expression): Expression =
      Conjunction(left.substitute(variable, substitution), right.substitute(variable, substitution))
    override def toString: String = s"$left ∧ $right"

  case class Disjunction(left: Expression, right: Expression) extends Expression:
    def evaluate: Expression                                                 =
      val l = left.evaluate
      val r = right.evaluate
      (l, r) match
        case (True, _) => True
        case (_, True) => True
        case (False, x) => x
        case (x, False) => x
        case (x, y) => Disjunction(x, y)

    def substitute(variable: Variable, substitution: Expression): Expression =
      Disjunction(left.substitute(variable, substitution), right.substitute(variable, substitution))
    override def toString: String = s"$left ∨ $right"

  case class Implication(left: Expression, right: Expression) extends Expression:
    def evaluate: Expression = Disjunction(Negation(left), right)

    def substitute(variable: Variable, substitution: Expression): Expression =
      Implication(left.substitute(variable, substitution), right.substitute(variable, substitution))
    override def toString: String = s"$left → $right"

  case class Equivalence(left: Expression, right: Expression) extends Expression:
    def evaluate: Expression = Conjunction(Implication(left, right), Implication(right, left))

    def substitute(variable: Variable, substitution: Expression): Expression =
      Equivalence(left.substitute(variable, substitution), right.substitute(variable, substitution))

    override def toString: String = s"$left ↔ $right"

  given Conversion[String, Variable] with
    def apply(str: String): Variable = Variable(str)

  extension (expr: Expression)

    @targetName("negation")
    infix def unary_! : Negation = Negation(expr)

    @targetName("conjunction")
    infix def ∧(that: Expression): Conjunction = Conjunction(expr, that)

    @targetName("disjunction")
    infix def ∨(that: Expression): Disjunction = Disjunction(expr, that)

    @targetName("implication")
    infix def →(that: Expression): Implication = Implication(expr, that)

    @targetName("equivalence")
    infix def ↔(that: Expression): Equivalence = Equivalence(expr, that)
