package specific.sysml.synthesis

import org.eclipse.emf.ecore.EObject
import specific.util.every

sealed trait SynthesisResult[+T <: EObject] {
  def synthesizedElement: Option[T]
  def delayedTasks: Seq[SynthesisTask[_ <: EObject]]
  def messages: Seq[Message]
  def isSuccess: Boolean = false
  def progressed: Boolean = false
  def isFailure: Boolean = false
  def map[U <: EObject](f: T => U): SynthesisResult[U]
  def foreach(f: T => Unit): SynthesisResult[EObject] = map(t => { f(t); null.asInstanceOf[EObject] })

  def and(task: SynthesisResult[_ <: EObject]): SynthesisResult[T]
  def alsoDo(tasks: Seq[SynthesisResult[_ <: EObject]]): SynthesisResult[T] = tasks.foldLeft(this) {
    case (x,y) => x and y
  }

  def dispatchTasks(tasks: Seq[SynthesisTask[_ <: EObject]]): SynthesisResult[T]
  def dispatchTask(task: SynthesisTask[_ <: EObject]): SynthesisResult[T] = dispatchTasks(Seq(task))
  def dispatch(task: => SynthesisResult[_ <: EObject]): SynthesisResult[T] = dispatchTask(SynthesisTask(task))
  def dispatchAll(task: => Seq[SynthesisResult[_ <: EObject]]): SynthesisResult[T] = dispatchTasks(task.map(SynthesisTask(_)))
  def complete(): SynthesisResult[T]
}

case class Success[T <: EObject](element: T, messages: Seq[Message] = Seq.empty) extends SynthesisResult[T] {
  def synthesizedElement = Some(element)
  def delayedTasks = Seq.empty
  def dispatchTasks(tasks: Seq[SynthesisTask[_ <: EObject]]) = Partial(element, messages, tasks)
  def complete() = {
    println(s"complete succ $element")
    this
  }
  def map[U <: EObject](f: T => U): SynthesisResult[U] = Success(f(element),messages)
  def and(task: SynthesisResult[_ <: EObject]): SynthesisResult[T] = task match {
    case Success(el,ms) =>
      println("succ succ")
      Success(element,messages ++ ms)
    case Failure(ms) =>
      println("succ fail")
      Partial(element, messages ++ ms, Seq.empty)
    case Partial(el,ms,ts) =>
      println("succ part")
      Partial(element,messages ++ ms, ts)
  }
  override def isSuccess = true
}

case class Failure(messages: Seq[Message] = Seq.empty) extends SynthesisResult[Nothing] {
  def synthesizedElement = None
  def delayedTasks = Seq.empty
  def dispatchTasks(tasks: Seq[SynthesisTask[_ <: EObject]]) = this
  def complete() = {
    println("complete fail")
    this
  }
  def map[U <: EObject](f: Nothing => U): SynthesisResult[U] = Failure(messages)
  def and(task: SynthesisResult[_ <: EObject]): SynthesisResult[Nothing] = this
  override def isFailure = true
}

case class Partial[T <: EObject](element: T, messages: Seq[Message] = Seq.empty, delayedTasks: Seq[SynthesisTask[_ <: EObject]]) extends SynthesisResult[T] {
  def synthesizedElement = Some(element)
  def dispatchTasks(tasks: Seq[SynthesisTask[_ <: EObject]]) = copy(delayedTasks = delayedTasks ++ tasks)
  def map[U <: EObject](f: T => U): SynthesisResult[U] = Partial(f(element), messages, delayedTasks)
  def and(task: SynthesisResult[_ <: EObject]): SynthesisResult[T] = task match {
    case Failure(ms) => Partial(element, messages, delayedTasks :+ SynthesisTask(Failure(ms)))
    case Partial(es,ms,ts) => Partial(element,messages ++ ms, delayedTasks ++ ts)
    case Success(_,ms) => Success(element,messages ++ ms)
  }

  def complete(): SynthesisResult[T] = {
    println("completing")
    val runs = delayedTasks.map(t => t.run.complete())
    val (compl,delayed) = runs.partition(_.isSuccess)
    if (delayed.isEmpty) {
      println("succ")
      Success(element, messages ++ compl.flatMap(_.messages))
    }
    else if (compl.nonEmpty) {
      println("part")
      Partial(element, messages ++ compl.flatMap(_.messages), delayed.flatMap(_.delayedTasks)).complete()
    }
    else {
      println("fail")
      Failure(messages ++ delayed.flatMap(_.messages))
    }
  }
}