package specific.sysml

import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.xmi
import org.eclipse.uml2.uml
import org.eclipse.papyrus.sysml._
import org.eclipse.papyrus.sysml.blocks.BlocksFactory
import org.eclipse.papyrus.sysml.portandflows.PortandflowsFactory
import org.eclipse.papyrus.sysml.util.{SysmlResource}
import org.eclipse.uml2.uml.{UMLFactory}

import scala.collection.JavaConversions._

/**
  * Created by martin on 27.04.16.
  */
class ModelBuilder(name: String) {
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

  def getOrCreatePackage(namespace: uml.Package, name: String): uml.Package = {
    namespace.getNestedPackages.find(_.getName == name).getOrElse {
      val pkg = umlFactory.createPackage()
      pkg.setName(name)
      namespace.getNestedPackages.add(pkg)
      pkg
    }
  }

  def addDiagrams(diag: Seq[Diagram]): Unit = diag.foreach { diag =>
    diag.modelElementType match {
      case "package" =>
        val pkg = diag.modelElementName.parts.foldLeft[uml.Package](model) {
          case (m,p) => getOrCreatePackage(m,p)
        }
        diag.content.foreach(create(pkg))
    }
  }

  def save() = resource.save(Map.empty[Any,Any])
}
