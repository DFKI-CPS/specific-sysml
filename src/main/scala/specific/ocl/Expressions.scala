package specific.ocl

import specific.sysml.Types.Classifier
import specific.sysml.{Name, Types}

object Expressions {

  sealed trait OclExpression


  /**
    * An IfExp results in one of two alternative expressions depending on the evaluated value of a condition. Note that both the
    * thenExpression and the elseExpression are mandatory. The reason behind this is that an if expression should always result
    * in a value, which cannot be guaranteed if the else part is left out.
    *
    * @param condition      The OclExpression that represents the boolean condition. If this condition evaluates to true, the
    *                       result of the if expression is identical to the result of the thenExpression. If this condition
    *                       evaluates to false, the result of the if expression is identical to the result of the elseExpression.
    * @param thenExpression The OclExpression that represents the then part of the if expression.
    * @param elseExpression The OclExpression that represents the else part of the if expression.
    */
  case class IfExp(condition: OclExpression, thenExpression: OclExpression, elseExpression: OclExpression) extends OclExpression

  case class LetExp(varDecl: Seq[VariableDeclaration], subExp: OclExpression) extends OclExpression

  case class TypeExp(referredType: Option[Classifier]) extends OclExpression

  sealed trait LiteralExp extends OclExpression

  case object NullLiteralExp extends LiteralExp

  case object InvalidLiteralExp extends LiteralExp

  sealed trait PrimitiveLiteralExp extends LiteralExp

  sealed trait NumericLiteralExp extends PrimitiveLiteralExp

  case class RealLiteralExp(realSymbol: Double) extends NumericLiteralExp

  case class UnlimitedNaturalLiteralExp(unlimitedNaturalSymbol: BigInt) extends NumericLiteralExp

  case class IntegerLiteralExp(integerSymbol: Integer) extends NumericLiteralExp

  // MISSING: EnumLiteralExp

  case class VariableDeclaration(name: String, tpe: Option[Name[Classifier]])

}