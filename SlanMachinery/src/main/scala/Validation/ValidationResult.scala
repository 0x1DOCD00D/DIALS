package Validation

import cats.Monoid
import cats.implicits._

sealed trait ValidationMessage
case class Warning(message: String) extends ValidationMessage
case class Error(message: String)   extends ValidationMessage

case class ValidationResult(
                             errors: List[Error] = Nil,
                             warnings: List[Warning] = Nil,
//                           ValidatedDataModel
                             visitedEntities: List[String] = Nil
                           ) extends VisitorState {
  def isValid: Boolean = errors.isEmpty

  override def toString: String =
    s"ValidationResult(\n" +
      s"  errors = $errors,\n" +
      s"  warnings = $warnings,\n" +
      s"  visitedEntities = $visitedEntities\n" +
      ")"
}

object ValidationResult {
  val valid: ValidationResult = ValidationResult()

  def fromError(message: String): ValidationResult = ValidationResult(errors = List(Error(message)))
  def fromWarning(message: String): ValidationResult = ValidationResult(warnings = List(Warning(message)))

  implicit val validationResultMonoid: Monoid[ValidationResult] = Monoid.instance(
    valid,
    (x, y) => ValidationResult(
      errors = x.errors ++ y.errors, // Ensures errors accumulate
      warnings = x.warnings ++ y.warnings, // Ensures warnings accumulate
      visitedEntities = x.visitedEntities ++ y.visitedEntities // Ensures visited entities accumulate
    )
  )
}
