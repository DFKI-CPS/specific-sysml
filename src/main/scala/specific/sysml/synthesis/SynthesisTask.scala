package specific.sysml.synthesis

import org.eclipse.emf.ecore.EObject

sealed trait SynthesisTask[+T <: EObject] {
  def run: SynthesisResult[T]

  def andLater[U <: EObject](f: T => SynthesisResult[U]) = SynthesisTask {
    val a = run
    a match {
      case Success(e,ms) => Partial(e,ms,Seq(SynthesisTask(f(e))))
      case Partial(e,ms,ts) => Partial(e,ms,ts :+ SynthesisTask(f(e)))
      case Failure(x) => Failure(x)
    }
  }
}

object SynthesisTask {
  def apply[T <: EObject](f: => SynthesisResult[T]) = new SynthesisTask[T] {
    def run = f
  }

  def once[T <: EObject](f: => SynthesisResult[T]) = {
    var x: Option[SynthesisResult[T]] = None
    new SynthesisTask[T] {
      def run = x.getOrElse {
        val res = f
        x = Some(res)
        res
      }
    }
  }
}
