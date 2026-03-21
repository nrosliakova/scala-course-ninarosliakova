package kse.unit3.challenge

import kse.unit3.challenge.expressions.*
import kse.unit3.challenge.generators.given
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean}

object BooleansSpecification extends Properties("Expressions"):

  include(BooleanEvaluationSpecification)
  include(VariableEvaluationSpecification)
  include(NegationEvaluationSpecification)
  include(ConjunctionEvaluationSpecification)
  include(DisjunctionEvaluationSpecification)
  include(ImplicationEvaluationSpecification)
  include(EquivalenceEvaluationSpecification)
  include(BooleanSubstitutionSpecification)
  include(VariableSubstitutionSpecification)
  include(ExpressionSubstitutionSpecification)

end BooleansSpecification

object BooleanEvaluationSpecification extends Properties("Boolean Evaluation"):

  property("boolean value should be evaluated to itself") = forAll: (boolean: Boolean) =>
    boolean.evaluate == boolean

end BooleanEvaluationSpecification

object VariableEvaluationSpecification extends Properties("Variable Evaluation"):

  property("variable should be evaluated to itself") = forAll: (variable: Variable) =>
    variable.evaluate == variable

end VariableEvaluationSpecification

object NegationEvaluationSpecification extends Properties("Negation Evaluation"):

  property("!True should be evaluated to False") = !True.evaluate == False

  property("!False should be evaluated to True") = !False.evaluate == True

  property("!variable should be evaluated to !variable") = forAll: (variable: Variable) =>
    (!variable).evaluate == !variable

  property("!expression should be correctly evaluated") = forAll: (expression: Expression) =>
    (!expression).evaluate == (!expression.evaluate).evaluate

end NegationEvaluationSpecification

object ConjunctionEvaluationSpecification extends Properties("Conjunction Evaluation"):

  property("True ∧ expression should be evaluated to expression evaluation") = forAll: (exp: Expression) =>
    (True ∧ exp).evaluate == exp.evaluate

  property("expression ∧ True should be evaluated to expression evaluation") = forAll: (exp: Expression) =>
    (exp ∧ True).evaluate == exp.evaluate

  property("False ∧ expression should be evaluated to False") = forAll: (exp: Expression) =>
    (False ∧ exp).evaluate == False

  property("expression ∧ False should be evaluated to False") = forAll: (exp: Expression) =>
    (exp ∧ False).evaluate == False

  property("left ∧ right should be correctly evaluated") = forAll: (left: Expression, right: Expression) =>
    (left ∧ right).evaluate == (left.evaluate ∧ right.evaluate).evaluate

end ConjunctionEvaluationSpecification

object DisjunctionEvaluationSpecification extends Properties("Disjunction Evaluation"):

  property("True ∨ expression should be evaluated to True") = forAll: (exp: Expression) =>
    (True ∨ exp).evaluate == True

  property("expression ∨ True should be evaluated to True") = forAll: (exp: Expression) =>
    (exp ∨ True).evaluate == True

  property("False ∨ expression should be evaluated to expression evaluation") = forAll: (exp: Expression) =>
    (False ∨ exp).evaluate == exp.evaluate

  property("expression ∨ False should be evaluated to expression evaluation") = forAll: (exp: Expression) =>
    (exp ∨ False).evaluate == exp.evaluate

  property("left ∨ right should be correctly evaluated") = forAll: (left: Expression, right: Expression) =>
    (left ∧ right).evaluate == (left.evaluate ∨ right.evaluate).evaluate

end DisjunctionEvaluationSpecification

object ImplicationEvaluationSpecification extends Properties("Implication Evaluation"):

  property("True → expression should be evaluated to expression evaluation") = forAll: (exp: Expression) =>
    (True → exp).evaluate == exp.evaluate

  property("False → expression should be evaluated to True") = forAll: (exp: Expression) =>
    (False → exp).evaluate == True

  property("left → right should be correctly evaluated") = forAll: (left: Expression, right: Expression) =>
    (left ∧ right).evaluate == (left.evaluate → right.evaluate).evaluate

end ImplicationEvaluationSpecification

object EquivalenceEvaluationSpecification extends Properties("Equivalence Evaluation"):

  property("Reflexivity") = forAll: (exp: Expression) =>
    (exp ↔ exp).evaluate == True

  property("Symmetry") = forAll: (x: Expression, y: Expression) =>
    (x ↔ y).evaluate == (y ↔ x).evaluate

  property("Transitivity") = forAll: (x: Expression, y: Expression, z: Expression) =>
    ((x ↔ z) ∧ (z ↔ y)).evaluate == True ==> ((x ↔ z).evaluate == True)

  property("left ↔ right should be correctly evaluated") = forAll: (left: Expression, right: Expression) =>
    (left ↔ right).evaluate == ((left → right) ∧ (right → left)).evaluate

end EquivalenceEvaluationSpecification

object BooleanSubstitutionSpecification extends Properties("Boolean Substitution"):

  property("substitution into boolean should make no changes") = forAll:
    (boolean: Boolean, variable: Variable, sub: Expression) => boolean.substitute(variable, sub) == boolean

end BooleanSubstitutionSpecification

object VariableSubstitutionSpecification extends Properties("Variable Substitution"):

  property("substitution into different variable should make no changes") = forAll:
    (v1: Variable, v2: Variable, substitution: Expression) =>
      v1 != v2 ==> {
        v1.substitute(v2, substitution) == v1
      }

  property("substitution into the same variable should return the given expression") = forAll:
    (variable: Variable, sub: Expression) => variable.substitute(variable, sub) == sub

end VariableSubstitutionSpecification

object ExpressionSubstitutionSpecification extends Properties("Expression Substitution"):

  property("substitution into !expression should be equal to !(substitution into expression)") = forAll:
    (exp: Expression, v: Variable, sub: Expression) => (!exp).substitute(v, sub) == !(exp.substitute(v, sub))

  property("substitution into left ∧ right should be equal to substitution into left ∧ substitution into right") =
    forAll: (l: Expression, r: Expression, v: Variable, sub: Expression) =>
      (l ∧ r).substitute(v, sub) == (l.substitute(v, sub) ∧ r.substitute(v, sub))

  property("substitution into left ∨ right should be equal to substitution into left ∨ substitution into right") =
    forAll: (l: Expression, r: Expression, v: Variable, sub: Expression) =>
      (l ∨ r).substitute(v, sub) == (l.substitute(v, sub) ∨ r.substitute(v, sub))

  property("substitution into left → right should be equal to substitution into left → substitution into right") =
    forAll: (l: Expression, r: Expression, v: Variable, sub: Expression) =>
      (l → r).substitute(v, sub) == (l.substitute(v, sub) → r.substitute(v, sub))

  property("substitution into left ↔ right should be equal to substitution into left ↔ substitution into right") =
    forAll: (l: Expression, r: Expression, v: Variable, sub: Expression) =>
      (l ↔ r).substitute(v, sub) == (l.substitute(v, sub) ↔ r.substitute(v, sub))

end ExpressionSubstitutionSpecification
