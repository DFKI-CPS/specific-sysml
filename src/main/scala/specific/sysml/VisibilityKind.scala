package specific.sysml

sealed trait VisibilityKind

object VisibilityKind {
  case object Public extends VisibilityKind
  case object Private extends VisibilityKind
  case object Protected extends VisibilityKind
  case object Package extends VisibilityKind
}