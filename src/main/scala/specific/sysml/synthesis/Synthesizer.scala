package specific.sysml.synthesis

import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.{EObject, xmi}
import org.eclipse.papyrus.sysml.blocks.BlocksFactory
import org.eclipse.papyrus.sysml.portandflows.PortandflowsFactory
import specific.sysml._
import org.eclipse.uml2.uml
import org.eclipse.uml2.uml.{PseudostateKind, UMLFactory}
import org.eclipse.papyrus.sysml._
import scala.collection.JavaConversions._
import scala.util.parsing.input.NoPosition

class Synthesizer(name: String) {
  private val factory = new xmi.impl.XMIResourceFactoryImpl
  private val resource = factory.createResource(URI.createFileURI(s"./$name.uml"))
  private val umlFactory = UMLFactory.eINSTANCE
  private val blocksFactory = BlocksFactory.eINSTANCE
  private val portsFactory = PortandflowsFactory.eINSTANCE
  private val model = umlFactory.createModel()
  // TODO: Profile Applications
  model.setName(name)
  resource.getContents.add(model)

  def synthesize(diagram: Diagram): SynthesisResult[uml.Model] = diagram match {
    case Diagram(DiagramKind.BlockDefinitionDiagram,"package",meName,name,content) =>
      val pkg = meName.parts.foldLeft[uml.Package](model)(getOrCreatePackage)
      Success(model) dispatchTasks content.map(c => SynthesisTask(synthesize(pkg,c)))
    case other =>
      Failure(Seq(Error(NoPosition,s"could not synthesize $other")))
  }

  def synthesize(owner: uml.Package, member: Element): SynthesisResult[EObject] = member match {
    case Block(name,compartments,comments) =>
      val c = umlFactory.createClass ()
      val b = blocksFactory.createBlock ()
      b.setBase_Class (c)
      c.setName(name)
      owner.eResource ().getContents.add (b)
      owner.getPackagedElements.add (c)
      Success(b) dispatchTasks compartments.flatMap(_.content.map(x => SynthesisTask(synthesize(b,x))))
    case UnprocessedConstraint(_) =>
      Success(null) // TODO
    case other =>
      Failure(Seq(Error(NoPosition,s"could not synthesize $other")))
  }

  def synthesize(owner: blocks.Block, member: BlockMember): SynthesisResult[uml.NamedElement] = member match {
    case Operation(name,tpe,params,constraints) =>
      val c = owner.getBase_Class
      val op = umlFactory.createOperation()
      op.setName(name)
      c.getOwnedOperations.add(op)
      Success(op)
    case StateMachine(name,states) =>
      val c = owner.getBase_Class
      val stm = umlFactory.createStateMachine()
      val reg = umlFactory.createRegion()
      name.foreach(stm.setName)
      name.foreach(reg.setName)
      c.getOwnedBehaviors.add(stm)
      stm.getRegions.add(reg)
      Success(stm) dispatchTasks states.map(st => SynthesisTask(synthesize(reg,st)))
    case other =>
      Failure(Seq(Error(NoPosition,s"could not synthesize $other")))
  }

  def synthesize(stm: uml.Region, state: State): SynthesisResult[uml.Vertex] = state match {
    case ConcreteState(name,transitions) =>
      val st = umlFactory.createState()
      name.foreach(st.setName)
      stm.getSubvertices.add(st)
      Success(st) dispatchTasks transitions.map(ts => SynthesisTask(synthesize(st,ts)))
    case Choice(transitions) =>
      val st = umlFactory.createPseudostate()
      stm.getSubvertices.add(st)
      Success(st) dispatchTasks transitions.map(ts => SynthesisTask(synthesize(st,ts)))
    case other =>
      Failure(Seq(Error(NoPosition,s"could not synthesize $other")))
  }

  def synthesize(source: uml.Vertex, ts: Transition): SynthesisResult[uml.Transition] = ts match {
    case Transition(trigger,guard,action,InlineTargetState(st)) =>
      val reg = source.getContainer
      synthesize(reg, st).map { st =>
        val ts = umlFactory.createTransition()
        ts.setSource(source)
        ts.setTarget(st)
        reg.getTransitions.add(ts)
        null
      }
    case Transition(trigger,guard,action,UnresolvedTargetStateName(name)) =>
      val reg = source.getContainer
      resolve[uml.Vertex](reg,name.parts).fold[SynthesisResult[uml.Transition]] {
        Failure(Seq(Error(NoPosition,s"state $name not found")), retry = Some(SynthesisTask(synthesize(source,ts))))
      } { target =>
        val ts = umlFactory.createTransition()
        ts.setSource(source)
        ts.setTarget(target)
        reg.getTransitions.add(ts)
        Success(ts)
      }
  }

  private def resolve[T <: EObject](namespace: uml.Namespace, name: Seq[String]): Option[T] = {
    name.foldLeft[Option[uml.Namespace]](Some(namespace)) {
      case (ns,part) => for {
        ns <- ns
        member <- ns.getMembers.find(_.getName == part)
      } yield member.asInstanceOf[uml.Namespace]
    }.orElse(namespace.allOwningPackages().map(resolve(_,name)).find(_.isDefined).flatten).collect {
      case t: T => t
    }
  }

  private def getOrCreatePackage(namespace: uml.Package, name: String): uml.Package = {
    namespace.getNestedPackages.find(_.getName == name).getOrElse {
      val pkg = umlFactory.createPackage()
      pkg.setName(name)
      namespace.getNestedPackages.add(pkg)
      pkg
    }
  }

  def save() = resource.save(Map.empty[Any,Any])
}
