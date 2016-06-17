package specific.sysml.synthesis

import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.{EObject, xmi}
import org.eclipse.papyrus.sysml.blocks.BlocksFactory
import org.eclipse.papyrus.sysml.portandflows.PortandflowsFactory
import specific.sysml._
import org.eclipse.uml2.uml
import org.eclipse.uml2.uml.{PseudostateKind, UMLFactory, UMLPackage}
import org.eclipse.papyrus.sysml._
import org.eclipse.papyrus.sysml.util.{SysmlResource, SysmlResourceImpl}

import scala.collection.JavaConversions._
import scala.reflect.ClassTag
import scala.util.parsing.input.NoPosition

class Synthesizer(name: String) {
  private val umlPackage = UMLPackage.eINSTANCE
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
      Partial(model,Nil,Nil) alsoDo content.map(synthesize(pkg,_))
    case other =>
      Partial(null,Seq(Error(NoPosition,s"could not synthesize $other")),null)
  }

  def synthesize(owner: uml.Package, member: Element): SynthesisResult[EObject] = member match {
    case Block(name,compartments,comments) =>
      val c = umlFactory.createClass ()
      val b = blocksFactory.createBlock ()
      b.setBase_Class (c)
      c.setName(name)
      owner.eResource ().getContents.add (b)
      owner.getPackagedElements.add (c)
      Success(b) alsoDo compartments.flatMap(_.content.map(synthesize(b,_)))
    case UnprocessedConstraint(_,_) =>
      Success(null) // TODO
    case other =>
      Partial(null,Seq(Error(NoPosition,s"could not synthesize $other")),null)
  }

  def synthesize(owner: blocks.Block, member: BlockMember): SynthesisResult[uml.NamedElement] = member match {
    case Operation(name,tpe,params,constraints) =>
      val c = owner.getBase_Class
      val op = umlFactory.createOperation()
      name.foreach(op.setName)
      c.getOwnedOperations.add(op)
      Success(op)
    case Property(name,tpe,cs) =>
      val c = owner.getBase_Class
      val p = umlFactory.createProperty()
      name.foreach(p.setName)
      c.getOwnedAttributes.add(p)
      Success(p) dispatch {
        resolve[uml.Type](c,tpe.name.parts).foreach(p.setType)
      }
    case Value(name,tpe) =>
      val c = owner.getBase_Class
      val p = umlFactory.createProperty()
      name.foreach(p.setName)
      c.getOwnedAttributes.add(p)
      Success(p) dispatch {
        resolve[uml.Type](c,tpe.name.parts).foreach(p.setType)
      }
    case Reference(name,tpe,None,cs) =>
      val c = owner.getBase_Class
      val p = umlFactory.createProperty()
      name.foreach(p.setName)
      c.getOwnedAttributes.add(p)
      Success(p) dispatch {
        resolve[uml.Type](c,tpe.name.parts).foreach(p.setType)
      }
    case StateMachine(name,states) =>
      val c = owner.getBase_Class
      val stm = umlFactory.createStateMachine()
      val reg = umlFactory.createRegion()
      name.foreach(stm.setName)
      name.foreach(reg.setName)
      c.getOwnedBehaviors.add(stm)
      stm.getRegions.add(reg)
      Success(stm) alsoDo states.map(synthesize(reg,_))
    case other =>
      Success(null,Seq(Error(NoPosition,s"could not synthesize $other")))
  }

  def synthesize(stm: uml.Region, state: State): SynthesisResult[uml.Vertex] = state match {
    case ConcreteState(name,transitions) =>
      val st = umlFactory.createState()
      name.foreach(st.setName)
      stm.getSubvertices.add(st)
      Success(st) dispatchAll transitions.map(synthesize(st,_))
    case Choice(transitions) =>
      val st = umlFactory.createPseudostate()
      st.setKind(PseudostateKind.CHOICE_LITERAL)
      stm.getSubvertices.add(st)
      Success(st) dispatchAll transitions.map(synthesize(st,_))
    case other =>
      Partial(null,Seq(Error(NoPosition,s"could not synthesize $other")),null)
  }

  def synthesize(source: uml.Vertex, ts: Transition): SynthesisResult[uml.Transition] = ts match {
    case Transition(trigger,guard,action,InlineTargetState(st)) =>
      val reg = source.getContainer
      synthesize(reg, st).map { st =>
        val ts = umlFactory.createTransition()
        ts.setSource(source)
        ts.setTarget(st)
        reg.getTransitions.add(ts)
        ts
      }
    case Transition(trigger,guard,action,UnresolvedTargetStateName(name)) =>
      val reg = source.getContainer
      resolve[uml.Vertex](reg,name.parts).map { target =>
        val ts = umlFactory.createTransition()
        ts.setSource(source)
        ts.setTarget(target)
        reg.getTransitions.add(ts)
        ts
      }
  }

  private def resolveNamespace(parent: uml.Namespace, name: Seq[String]): SynthesisResult[uml.Namespace] = {
    val r = name.foldLeft[Option[uml.Namespace]](Some(parent)) {
      case (ns,part) => for {
        ns <- ns
        member <- ns.getMembers.find(_.getName == part) if member.isInstanceOf[uml.Namespace]
      } yield member.asInstanceOf[uml.Namespace]
    }
    r.orElse(parent.allOwningPackages().map(resolveNamespace(_,name)).find(_.isSuccess).map(_.synthesizedElement.get)).fold[SynthesisResult[uml.Namespace]](
      Failure(Seq(Error(NoPosition,s"could not resolve $name")))
    ) { elem =>
      Success(elem)
    }
  }

  private def resolveType(namespace: uml.Namespace, name: Seq[String]): SynthesisResult[uml.Classifier] = {
    val ns = name.init
    val n = name.last
    resolveNamespace(namespace,ns).synthesizedElement.collect {
      case t: uml.Classifier => t
    }.fold[SynthesisResult[uml.Classifier]] (
      Failure(Seq(Error(NoPosition,s"could not resolve $name")))
    ) { elem =>
      Success(elem)
    }
  }

  private def resolve[T <: EObject](namespace: uml.Namespace, name: Seq[String])(implicit ev: ClassTag[T]): SynthesisResult[T] = {
    val ns = name.init
    val n = name.last
    /*if (ns.isEmpty && ) n match {
      case "Boolean"          => umlFactory.getUMLPackage.getLiteralBoolean
      case "String"           => umlFactory.getUMLPackage.getLiteralString
      case "Integer"          => umlFactory.getUMLPackage.getLiteralInteger
      case "Real"             => umlFactory.getUMLPackage.getLiteralReal
      case "UnlimitedNatural" => umlFactory.getUMLPackage.getLiteralUnlimitedNatural
    } else*/

    {
      resolveNamespace(namespace,ns).synthesizedElement.collect {
        case t: T => t
      }.fold[SynthesisResult[T]] (
        Failure(Seq(Error(NoPosition,s"could not resolve $name")))
      ) { elem =>
        Success(elem)
      }
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
