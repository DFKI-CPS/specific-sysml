package specific.sysml.synthesis

import org.eclipse.emf.ecore.EObject

sealed trait SynthesisTask[+T <: EObject] {
  def run: SynthesisResult[T]
}

object SynthesisTask {
  def apply[T <: EObject](f: => SynthesisResult[T]) = new SynthesisTask[T] {
    def run = f
  }
}
