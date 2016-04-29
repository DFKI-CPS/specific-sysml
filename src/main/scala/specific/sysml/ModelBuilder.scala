package specific.sysml

import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.{EObject, xmi}
import org.eclipse.uml2.uml
import org.eclipse.papyrus.sysml._
import org.eclipse.papyrus.sysml.blocks.BlocksFactory
import org.eclipse.papyrus.sysml.portandflows.PortandflowsFactory
import org.eclipse.papyrus.sysml.util.SysmlResource
import org.eclipse.uml2.uml.{Model, PseudostateKind, UMLFactory}
import specific.sysml.DiagramKind.BlockDefinitionDiagram

import scala.collection.JavaConversions._
import scala.util.parsing.input.Position

/**
  * Created by martin on 27.04.16.
  */
class ModelBuilder(name: String) {
  trait SynthesisTask {
    def description: String
    def run: SynthesisResult
  }

  object Noop extends SynthesisTask {
    def description = "<NOOP>"
    def run = Success(null)
  }

  object SynthesisTask {
    def apply[T](descr: String)(condition: => Option[T])(task: T => SynthesisResult) = new SynthesisTask {
      def description = descr
      override def run: SynthesisResult = condition.map(task).getOrElse(Failure(Seq(this), Nil))
    }

    def simple(task: => SynthesisResult) = new SynthesisTask {
      def description = "<SIMPLE>"
      override def run: SynthesisResult = task
    }
  }

  sealed trait Message
  case class Plain(msg: String) extends Message

  object SynthesisResult {
    def apply(element: Option[EObject], outstanding: Seq[SynthesisTask], messages: Seq[Message]): SynthesisResult = element match {
      case None => Failure(outstanding, messages)
      case Some(elem) =>
        if (outstanding.isEmpty && messages.isEmpty) Success(elem) else Partial(elem,outstanding,messages)
    }
  }

  sealed trait SynthesisResult {
    val element: Option[EObject]
    val outstanding: Seq[SynthesisTask]
    val messages: Seq[Message]

    def flatMap[T <: EObject](f: T => SynthesisResult): SynthesisResult = this.element match {
      case None => this
      case Some(elem: T) => this and f(elem)
      case Some(other) => Failure(outstanding, messages :+ Plain(s"synthesis error: maltyped element $other"))
    }

    def foreach[T <: EObject](f: T => Unit): SynthesisResult = this.flatMap[T] { t =>
      f(t)
      Success(null)
    }

    def task(t: SynthesisTask) = this.element match {
      case None => Failure(outstanding :+ t, messages)
      case Some(elem) => Partial(elem, outstanding :+ t, messages)
    }

    def tasks(t: Seq[SynthesisTask]) = this.element match {
      case None => Failure(outstanding ++ t, messages)
      case Some(elem) => Partial(elem, outstanding ++ t, messages)
    }

    def and(sr: SynthesisResult) = this.element match {
      case None => sr match {
        case Success(_) => this
        case other => Failure(outstanding ++ other.outstanding, messages ++ other.messages)
      }
      case Some(elem) => sr match {
        case Success(_) => this
        case Partial(_,outstanding2,messages2) => Partial(elem, outstanding ++ outstanding2, messages ++ messages2)
        case Failure(outstanding2, messages2) => Failure(outstanding ++ outstanding2, messages ++ messages2)
      }
    }

    def complete(): SynthesisResult = if (outstanding.isEmpty) this else {
      val (os,ms) = outstanding.foldLeft((Seq.empty[SynthesisTask],messages)) {
        case ((os,ms),o) => o.run match {
          case Success(_) => (os,ms)
          case Partial(_,os2,ms2) => (os ++ os2, ms2)
          case Failure(os2,ms2) => (os ++ os2, ms ++ ms2)
        }
      }
      if (os.length < outstanding.length)
        SynthesisResult(element,os,ms)
      else
        SynthesisResult(element,Nil,ms)
    }

    def andAll(srs: Seq[SynthesisResult]) = srs.foldLeft(this)(_ and _)
  }

  case class Partial(elem: EObject, outstanding: Seq[SynthesisTask], messages: Seq[Message]) extends SynthesisResult {
    val element = Some(elem)
  }

  case class Success(elem: EObject) extends SynthesisResult {
    val element = Some(elem)
    val outstanding: Seq[SynthesisTask] = Seq.empty
    val messages: Seq[Message] = Seq.empty
  }

  case class Failure(outstanding: Seq[SynthesisTask], messages: Seq[Message]) extends SynthesisResult {
    val element = None
  }

  private val factory = new xmi.impl.XMIResourceFactoryImpl
  private val resource = factory.createResource(URI.createFileURI(s"./$name.uml"))
  private val umlFactory = UMLFactory.eINSTANCE
  private val blocksFactory = BlocksFactory.eINSTANCE
  private val portsFactory = PortandflowsFactory.eINSTANCE
  private val model = umlFactory.createModel()
  //private val sysmlProfile = umlFactory.createProfile()
  //model.applyProfile()
  model.setName(name)
  resource.getContents.add(model)

  def resolve(namespace: uml.Namespace, name: Seq[String]): Option[uml.NamedElement] = {
    name.foldLeft[Option[uml.Namespace]](Some(namespace)) {
      case (ns,part) => for {
        ns <- ns
        member <- ns.getMembers.find(_.getName == part)
      } yield member.asInstanceOf[uml.Namespace]
    }.orElse(namespace.allOwningPackages().map(resolve(_,name)).find(_.isDefined).flatten)
  }

  def synthesize(owner: EObject, element: Element): SynthesisResult = (owner,element) match {
    case (model: uml.Model, Diagram(BlockDefinitionDiagram, "package", name, diagName, elements)) =>
      val pkg = name.parts.foldLeft[uml.Package](model)(getOrCreatePackage)
      Success(pkg) andAll (for {
        elem <- elements
      } yield synthesize(pkg,elem))
    case (pkg: uml.Package, Block(rawName,compartments,comments)) =>
      val c = umlFactory.createClass()
      val b = blocksFactory.createBlock()
      b.setBase_Class(c)
      c.setName(rawName)
      pkg.eResource().getContents.add(b)
      pkg.getPackagedElements.add(c)
      Success(b) andAll (for {
        compartment <- compartments
        member <- compartment.content
      } yield synthesize(b,member))
    case (block: blocks.Block, Operation(name,tpe,params,constraints)) =>
      val c = block.getBase_Class
      val op = umlFactory.createOperation()
      op.setName(name)
      c.getOwnedOperations.add(op)
      Success(op) tasks tpe.map( tpe =>
        SynthesisTask(s"resolve $tpe")(resolve(c,tpe.name.parts)) { x =>
          op.setType(x.asInstanceOf[uml.Type])
          Success(null)
        }
      ).toSeq
    case (block: blocks.Block, Reference(name,tpe,None,other)) =>
      val c = block.getBase_Class
      val ref = umlFactory.createProperty()
      ref.setName(name)
      c.getOwnedAttributes.add(ref)
      ref.setLower(tpe.multiplicity.lower.toInt)
      ref.setUpper(tpe.multiplicity.upper.value.toInt)
      Success(ref) task (
        SynthesisTask(s"resolve $tpe")(resolve(c,tpe.name.parts)) { x =>
          ref.setType(x.asInstanceOf[uml.Type])
          Success(null)
        }
      )
    case (block: blocks.Block, StateMachine(name,states)) =>
      val c = block.getBase_Class
      val stm = umlFactory.createStateMachine()
      val reg = umlFactory.createRegion()
      reg.setName("default")
      stm.getRegions.add(reg)
      name.foreach(stm.setName)
      c.getOwnedBehaviors.add(stm)
      Success(stm) andAll states.map(synthesize(reg,_))
    case (stm: uml.Region, ConcreteState(name,transitions)) =>
      val st = umlFactory.createState()
      name.foreach(st.setName)
      stm.getSubvertices.add(st)
      Success(st) andAll transitions.map(synthesize(st,_))
    case (stm: uml.Region, Choice(transitions)) =>
      val st = umlFactory.createPseudostate()
      st.setKind(PseudostateKind.CHOICE_LITERAL)
      stm.getSubvertices.add(st)
      Partial(st,transitions.map(ts => SynthesisTask.simple(synthesize(st,ts))),Nil)
    case (st: uml.Vertex, Transition(trigger,guart,action,target)) =>
      val reg = st.getContainer
      val ts = umlFactory.createTransition()
      ts.setSource(st)
      reg.getTransitions.add(ts)
      target match {
        case UnresolvedTargetStateName(name) =>
          Success(ts) task (
            SynthesisTask(s"resolve $name")(resolve(reg,name.parts)) { x =>
              ts.setTarget(x.asInstanceOf[uml.State])
              Success(null)
            }
          )
        case InlineTargetState(ist) =>
          synthesize(reg, ist).foreach[uml.State](ts.setTarget)
      }
    case _ =>
      Failure(Seq.empty, Seq(Plain(s"could not synthesize $element inside $owner")))
  }

  def getOrCreatePackage(namespace: uml.Package, name: String): uml.Package = {
    namespace.getNestedPackages.find(_.getName == name).getOrElse {
      val pkg = umlFactory.createPackage()
      pkg.setName(name)
      namespace.getNestedPackages.add(pkg)
      pkg
    }
  }

  def addDiagrams(diag: Seq[Diagram]): SynthesisResult = {
    val result = Success(model) andAll diag.map(synthesize(model,_))
    result.complete()
  }

  def save() = resource.save(Map.empty[Any,Any])
}
