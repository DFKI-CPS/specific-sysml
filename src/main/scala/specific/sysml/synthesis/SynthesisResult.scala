package specific.sysml.synthesis

import org.eclipse.emf.ecore.EObject
import specific.util.every

sealed trait SynthesisResult[+T <: EObject] {
  def synthesizedElement: Option[T]
  def delayedTasks: Seq[SynthesisTask[_ <: EObject]]
  def messages: Seq[Message]
  def isSuccess: Boolean = false
  def isFailure: Boolean = false
  def map[U <: EObject](f: T => U): SynthesisResult[U]

  def dispatchTasks(tasks: Seq[SynthesisTask[_ <: EObject]]): SynthesisResult[T]
  def dispatchTask(task: SynthesisTask[_ <: EObject]): SynthesisResult[T] = dispatchTasks(Seq(task))
  def dispatch(task: => SynthesisResult[_ <: EObject]): SynthesisResult[T] = dispatchTask(SynthesisTask(task))
  def complete(): SynthesisResult[T]
}

case class Success[T <: EObject](element: T, messages: Seq[Message] = Seq.empty) extends SynthesisResult[T] {
  def synthesizedElement = Some(element)
  def delayedTasks = Seq.empty
  def dispatchTasks(tasks: Seq[SynthesisTask[_ <: EObject]]) = Partial(element, messages, tasks)
  def complete() = this
  def map[U <: EObject](f: T => U): SynthesisResult[U] = Success(f(element),messages)
  override def isSuccess = true
}

case class Failure[T <: EObject](messages: Seq[Message] = Seq.empty, retry: Option[SynthesisTask[T]] = None) extends SynthesisResult[T] {
  def synthesizedElement = None
  def delayedTasks = retry.toSeq
  def dispatchTasks(tasks: Seq[SynthesisTask[_ <: EObject]]) = this
  def complete() = this
  def map[U <: EObject](f: T => U): SynthesisResult[U] = Failure(messages,retry.map(x => SynthesisTask(x.run.map(f))))
  override def isFailure = true
}

case class Partial[T <: EObject](element: T, messages: Seq[Message] = Seq.empty, delayedTasks: Seq[SynthesisTask[_ <: EObject]]) extends SynthesisResult[T] {
  def synthesizedElement = Some(element)
  def dispatchTasks(tasks: Seq[SynthesisTask[_ <: EObject]]) = copy(delayedTasks = delayedTasks ++ tasks)
  def map[U <: EObject](f: T => U): SynthesisResult[U] = Partial(f(element), messages, delayedTasks)

  def complete(): SynthesisResult[T] = {
    val runs = delayedTasks.map(t => (t,t.run.complete()))
    val (compl,delayed) = runs.partition(_._2.isSuccess)
    if (delayed.isEmpty) Success(element, messages)
    else if (compl.nonEmpty) Partial(element, messages ++ compl.flatMap(_._2.messages), delayed.flatMap(_._2.delayedTasks)).complete()
    else Failure(messages ++ delayed.flatMap(_._2.messages), retry = Some(SynthesisTask(Partial(element, messages, delayed.flatMap(_._2.delayedTasks)).complete())))
  }
}