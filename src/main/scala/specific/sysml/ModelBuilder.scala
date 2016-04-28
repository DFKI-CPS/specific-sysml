package specific.sysml

import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.{EObject, xmi}
import org.eclipse.uml2.uml
import org.eclipse.papyrus.sysml._
import org.eclipse.papyrus.sysml.blocks.BlocksFactory
import org.eclipse.papyrus.sysml.portandflows.PortandflowsFactory
import org.eclipse.papyrus.sysml.util.SysmlResource
import org.eclipse.uml2.uml.{Model, UMLFactory}
import specific.sysml.DiagramKind.BlockDefinitionDiagram

import scala.collection.JavaConversions._
import scala.util.parsing.input.Position

/**
  * Created by martin on 27.04.16.
  */
class ModelBuilder(name: String) {
  case class SynthesisError(pos: Position, message: String)

  trait SynthesisTask {
    def description: String
    def synthesize(context: Model): Boolean
  }

  object Noop extends SynthesisTask {
    def description = "<NOOP>"
    def synthesize(context: Model) = true
  }

  object SynthesisTask {
    def apply[T](descr: String)(condition: Model => Option[T])(task: T => Unit) = new SynthesisTask {
      def description = descr
      override def synthesize(context: Model): Boolean = condition(context).map(task).isDefined
    }
  }

  private val factory = new xmi.impl.XMIResourceFactoryImpl
  private val resource = factory.createResource(URI.createFileURI(s"./$name.uml"))
  private val umlFactory = UMLFactory.eINSTANCE
  private val blocksFactory = BlocksFactory.eINSTANCE
  private val portsFactory = PortandflowsFactory.eINSTANCE
  private val model = umlFactory.createModel()
  //private val sysmlProfile = umlFactory.createProfile()
  //model.applyProfile()
  println(SysmlResource.PROFILES_PATHMAP)
  println(SysmlResource.SYSML_PROFILE_NS_URI)
  println(SysmlResource.SYSML_PRIMITIVE_TYPES_LIBRARY_URI)
  println(SysmlResource.SYSML_PROFILE_URI)
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

  def synthesize(owner: EObject, element: Element): Seq[SynthesisTask] = (owner,element) match {
    case (model: uml.Model, Diagram(BlockDefinitionDiagram, "package", name, diagName, elements)) =>
      val pkg = name.parts.foldLeft[uml.Package](model)(getOrCreatePackage)
      elements.flatMap(elem => synthesize(pkg,elem))
    case (pkg: uml.Package, Block(rawName,compartments,comments)) =>
      val c = umlFactory.createClass()
      val b = blocksFactory.createBlock()
      b.setBase_Class(c)
      c.setName(rawName)
      pkg.eResource().getContents.add(b)
      pkg.getPackagedElements.add(c)
      compartments.flatMap(_.content.flatMap(synthesize(b,_)))
    case (block: blocks.Block, Operation(name,tpe,params,constraints)) =>
      val c = block.getBase_Class
      val op = umlFactory.createOperation()
      op.setName(name)
      c.getOwnedOperations.add(op)
      Seq(
        SynthesisTask(s"resolve $tpe")(model => resolve(c,tpe.name.parts)) { x =>
          op.setType(x.asInstanceOf[uml.Type])
        }
      )
    case (block: blocks.Block, Reference(name,tpe,None,other)) =>
      val c = block.getBase_Class
      val ref = umlFactory.createProperty()
      ref.setName(name)
      c.getOwnedAttributes.add(ref)
      ref.setLower(tpe.multiplicity.lower.toInt)
      ref.setUpper(tpe.multiplicity.upper.value.toInt)
      Seq(
        SynthesisTask(s"resolve $tpe")(model => resolve(c,tpe.name.parts)) { x =>
          ref.setType(x.asInstanceOf[uml.Type])
        }
      )
    case _ =>
      println(s"could not synthesize $element inside $owner")
      Seq.empty
  }

  /*
  def create(stm: uml.Region, source: uml.State)(element: Element): Unit = element match {
    case Transition(trigger,guard,action,target) =>
      val ts = umlFactory.createTransition()
      ts.setSource(source)
      stm.getTransitions.add(ts)
  }

  def create(stm: uml.Region)(element: Element): Unit = element match {
    case ConcreteState(name,transitions) =>
      val st = umlFactory.createState()
      name.foreach(st.setName)
      stm.getSubvertices.add(st)
      transitions.foreach(create(stm,st))
    case other =>
      println("skipping " + other)
  }

  def create(o: uml.Operation)(element: Element): Unit = element match {
    case Parameter(name,typeAnnotation) =>
      val p = umlFactory.createParameter()
      p.setName(name)
      o.getOwnedParameters.add(p)
    case other => println("skipping " + other)
  }

  def create(c: uml.Class)(element: Element): Unit = element match {
    case Comment(text) =>
      val comment = umlFactory.createComment()
      comment.setBody(text)
      c.getOwnedComments.add(comment)
    case Operation(name,typeAnnotation,parameters,constraints) =>
      val op = umlFactory.createOperation()
      op.setName(name)
      c.getOwnedOperations.add(op)
      parameters.foreach(create(op))
    case Reference(name,typeAnnotation,oppositeName,constraint) =>
      val ref = umlFactory.createAssociation()
      c.getPackage.getPackagedElements.add(ref)
    case Property(name,typeAnnotation,constraint) =>
      val prop = umlFactory.createProperty()
      prop.setName(name)
      c.getOwnedAttributes.add(prop)
    case Port(name,direction,typeAnnotation) =>
      val b = umlFactory.createPort()
      b.setName(name)
      c.getOwnedPorts.add(b)
      val port = portsFactory.createFlowPort()
      port.setDirection(direction)
      port.setBase_Port(b)
      resource.getContents.add(port)
    case Value(name,typeAnnotation) =>
      val prop = umlFactory.createProperty()
      prop.setName(name)
      c.getOwnedAttributes.add(prop)
    case StateMachine(name,states) =>
      val stm = umlFactory.createStateMachine()
      val region = umlFactory.createRegion()
      name.foreach(stm.setName)
      stm.getRegions.add(region)
      c.getOwnedBehaviors.add(stm)
      states.foreach(create(region))
    case other => println("skipping " + other)
  }

  def create(pkg: uml.Package)(element: Element): Unit = element match {
    case block: Block =>
      val b = blocksFactory.createBlock()
      val c = umlFactory.createClass()
      b.setBase_Class(c)
      c.setName(block.rawName)
      block.comments.foreach(create(c))
      pkg.getPackagedElements.add(c)
      resource.getContents.add(b)
      block.members.foreach(create(c))
    case other => println("skipping " + other)
  }

  case class Namespace(
    elements: Set[Element],
    nested: Map[String,Namespace])

  private var namespaces: Map[String,Namespace] = Map.empty
  */

  def getOrCreatePackage(namespace: uml.Package, name: String): uml.Package = {
    namespace.getNestedPackages.find(_.getName == name).getOrElse {
      val pkg = umlFactory.createPackage()
      pkg.setName(name)
      namespace.getNestedPackages.add(pkg)
      pkg
    }
  }

  def completeTasks(tasks: Seq[SynthesisTask]): Unit = if (tasks.nonEmpty) {
    val after = tasks.filter(!_.synthesize(model))
    if (after.length < tasks.length)
      completeTasks(after)
    else tasks.foreach { task =>
      println("could not " + task.description)
    }
  }

  def addDiagrams(diag: Seq[Diagram]) = {
    completeTasks(diag.flatMap(synthesize(model,_)))
  }

  def save() = resource.save(Map.empty[Any,Any])
}
