package specific.sysml

import java.util

import org.eclipse.emf.common.util.{Diagnostic, DiagnosticChain, URI}
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl
import org.eclipse.ocl.pivot
import org.eclipse.ocl.pivot.utilities.ParserException
import org.eclipse.papyrus.sysml
import org.eclipse.papyrus.sysml.blocks.{BlocksFactory, BlocksPackage}
import org.eclipse.papyrus.sysml.portandflows.{PortandflowsFactory, PortandflowsPackage}
import org.eclipse.uml2.uml
import org.eclipse.uml2.uml.{Profile, PseudostateKind, UMLFactory}
import org.eclipse.papyrus.sysml._
import org.eclipse.papyrus.sysml.util.SysmlResource
import org.eclipse.uml2.uml.util.UMLValidator
import specific.sysml.Types.PrimitiveType

import scala.util.parsing.input.Position
import scala.collection.JavaConverters._

object Synthesis {
  var initialized = false
  def init() = if (!initialized) {
    pivot.uml.UMLStandaloneSetup.init()
    org.eclipse.ocl.xtext.essentialocl.EssentialOCLStandaloneSetup.doSetup()
    org.eclipse.ocl.pivot.model.OCLstdlib.install()
    initialized = true
  }
}

class Synthesis(name: String) {
  Synthesis.init()
  /**
    * SysML
    * ModelElements
    * Blocks
    * PortAndFlows
    * Constraints
    * Activities
    * Allocations
    * Requirements
    * Interactions
    * StateMachines
    * UseCases
    */
  private val appliedProfiles = Set("SysML","Blocks","PortAndFlows")

  private val sysml_profile_uri = URI.createURI(getClass.getClassLoader.getResource("model/SysML.profile.uml").toString)
  private val library = new ResourceSetImpl

  library.getURIConverter.getURIMap.put(URI.createURI(SysmlResource.SYSML_PROFILE_URI), sysml_profile_uri)
  org.eclipse.uml2.uml.resources.util.UMLResourcesUtil.init(library)
  uml.resources.util.UMLResourcesUtil.initPackageRegistry(library.getPackageRegistry)
  library.getPackageRegistry put (uml.UMLPackage.eNS_URI, uml.UMLPackage.eINSTANCE)
  library.getPackageRegistry put (sysml.SysmlPackage.eNS_URI, sysml.SysmlPackage.eINSTANCE)
  library.getPackageRegistry put (BlocksPackage.eNS_URI, BlocksPackage.eINSTANCE)
  library.getPackageRegistry put (PortandflowsPackage.eNS_URI, PortandflowsPackage.eINSTANCE)
  private val primitives = library.getResource(URI.createURI(uml.resource.UMLResource.UML_PRIMITIVE_TYPES_LIBRARY_URI),true)
  private val sysmlProfiles = (URI.createURI(sysml.util.SysmlResource.SYSML_PROFILE_URI),true)

  val profs = library.getResource(URI.createURI("pathmap://SysML_PROFILES/SysML.profile.uml"),true)

  private val resource = library.createResource(URI.createFileURI(s"./$name.uml"))
  private val umlFactory = UMLFactory.eINSTANCE
  private val ecoreFactory = org.eclipse.emf.ecore.EcoreFactory.eINSTANCE
  private val blocksFactory = BlocksFactory.eINSTANCE
  private val portsFactory = PortandflowsFactory.eINSTANCE
  private val model = umlFactory.createModel()

  model.setName(name)
  resource.getContents.add(model)

  // PROFILE APPLICATIONS
  profs.getAllContents.forEachRemaining {
    case x: Profile if appliedProfiles contains(x.getName) =>
      val pappl = model.createProfileApplication()
      pappl.createEAnnotation("http://www.eclipse.org/uml2/2.0.0/UML").getReferences.add(x.getDefinition)
      pappl.setAppliedProfile(x)
    case _ =>
  }

  val chain = new DiagnosticChain {
    def merge(diagnostic: Diagnostic) =
      println(s"merge: $diagnostic")
    def addAll(diagnostic: Diagnostic) =
      println(s"addAll: $diagnostic")
    def add(diagnostic: Diagnostic) =
      println(s"add: $diagnostic")
  }

  val context = new util.HashMap[AnyRef,AnyRef]()

  val validate = new UMLValidator

  //////////////////////////////////////////////////////////////////////////////
  /// STRUCTURE  ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  def structure(diagram: Diagram): Unit = diagram match {
    case Diagram(DiagramKind.BlockDefinitionDiagram, "package", meName, name, content) =>
      val pkg = meName.foldLeft[uml.Package](model)(getOrCreatePackage)
      content.map(c => structure(pkg, c))
      diagram.uml = Some(pkg)
    case other =>
      error(diagram.pos, s"could not synthesize $other")
  }

  def structure(owner: uml.Package, member: Element): Unit = member match {
    case Block(name, compartments, comments) =>
      val c = umlFactory.createClass()
      val b = blocksFactory.createBlock()
      b.setBase_Class(c)
      c.setName(name)
      owner.eResource().getContents.add(b)
      owner.getPackagedElements.add(c)
      compartments.flatMap(_.content.map(x => structure(b, x)))
      member.uml = Some(c)
    case other =>
      error(other.pos, s"could not synthesize $other")
  }

  def structure(owner: blocks.Block, member: BlockMember): Unit = member match {
    case Operation(name, tpe, params, constraints) =>
      val c = owner.getBase_Class
      val op = umlFactory.createOperation()
      op.setName(name)
      c.getOwnedOperations.add(op)
      member.uml = Some(op)
      params.foreach(structure(op,_))
    case Reference(name,tpe,oppositeName,constraint) =>
      val c = owner.getBase_Class
      val p = umlFactory.createProperty()
      p.setIsUnique(tpe.multiplicity.isUnique)
      p.setIsOrdered(tpe.multiplicity.isOrdered)
      p.setLower(tpe.multiplicity.lower.toInt)
      p.setUpper(tpe.multiplicity.upper.value.toInt)
      p.setName(name)
      c.getOwnedAttributes.add(p)
      member.uml = Some(p)
    case Value(name,tpe) =>
      val c = owner.getBase_Class
      val p = umlFactory.createProperty()
      p.setLower(tpe.multiplicity.lower.toInt)
      p.setUpper(tpe.multiplicity.upper.value.toInt)
      p.setName(name)
      c.getOwnedAttributes.add(p)
      member.uml = Some(p)
    case Property(name,tpe,constraint) =>
      val c = owner.getBase_Class
      val p = umlFactory.createProperty()
      p.setLower(tpe.multiplicity.lower.toInt)
      p.setUpper(tpe.multiplicity.upper.value.toInt)
      p.setName(name)
      c.getOwnedAttributes.add(p)
      member.uml = Some(p)
    case Port(name,direction: Option[FlowDirection],tpe) =>
      val c = owner.getBase_Class
      val b = umlFactory.createPort()
      val p = portsFactory.createFlowPort()
      b.setLower(tpe.multiplicity.lower.toInt)
      b.setUpper(tpe.multiplicity.upper.value.toInt)
      direction.foreach(direction =>
        p.setDirection(direction match {
          case FlowDirection.In => org.eclipse.papyrus.sysml.portandflows.FlowDirection.IN
          case FlowDirection.InOut => org.eclipse.papyrus.sysml.portandflows.FlowDirection.INOUT
          case FlowDirection.Out => org.eclipse.papyrus.sysml.portandflows.FlowDirection.OUT
        })
      )
      b.setName(name)
      p.setBase_Port(b)
      c.getOwnedPorts.add(b)
      resource.getContents.add(p)
      member.uml = Some(b)
    case StateMachine(name, states) =>
      val c = owner.getBase_Class
      val stm = umlFactory.createStateMachine()
      val reg = umlFactory.createRegion()
      stm.setName(name)
      reg.setName(name)
      c.getOwnedBehaviors.add(stm)
      stm.getRegions.add(reg)
      states.map(st => structure(reg, st))
      member.uml = Some(stm)
    case UnprocessedConstraint(_) =>
      // TODO
    case other =>
      error(other.pos, s"could not synthesize $other")
  }

  def structure(op: uml.Operation, param: Parameter): Unit = param match {
    case Parameter(name, tpe) =>
      val p = umlFactory.createParameter()
      p.setName(name)
      param.uml = Some(p)
      op.getOwnedParameters.add(p)
  }

  def structure(stm: uml.Region, state: State): Option[uml.Vertex] = state match {
    case ConcreteState(name, transitions, isInitial) =>
      val st = umlFactory.createState()
      st.setName(name)
      stm.getSubvertices.add(st)
      transitions.map(ts => structure(st, ts))
      state.uml = Some(st)
      if (isInitial) {
        val init = umlFactory.createPseudostate()
        init.setKind(PseudostateKind.INITIAL_LITERAL)
        val tans = umlFactory.createTransition()
        tans.setSource(init)
        tans.setTarget(st)
        stm.getSubvertices.add(init)
        stm.getTransitions.add(tans)
      }
      Some(st)
    case Choice(transitions) =>
      val st = umlFactory.createPseudostate()
      st.setKind(PseudostateKind.CHOICE_LITERAL)
      stm.getSubvertices.add(st)
      transitions.map(ts => structure(st, ts))
      state.uml = Some(st)
      Some(st)
    case other =>
      error(other.pos, s"could not synthesize $other")
      None
  }

  def structure(source: uml.Vertex, trans: Transition): Unit = trans match {
    case Transition(trigger, guard, action, InlineTargetState(st)) =>
      val reg = source.getContainer
      structure(reg, st).foreach { st =>
        val ts = umlFactory.createTransition()
        trans.uml = Some(ts)
        ts.setSource(source)
        ts.setTarget(st)
        reg.getTransitions.add(ts)
        trigger.foreach(structure(ts,_))
      }
    case Transition(trigger, guard, action, UnresolvedTargetStateName(name)) =>
      val reg = source.getContainer
      val ts = umlFactory.createTransition()
      trans.uml = Some(ts)
      ts.setSource(source)
      reg.getTransitions.add(ts)
      trigger.foreach(structure(ts,_))
  }

  def structure(ts: uml.Transition, trigger: Trigger): Unit = trigger match {
    case trig@Trigger.Receive(port,v) =>
      val t = umlFactory.createTrigger()
      trig.uml = Some(t)
      //ts.getTriggers.add(t)
    case trig@Trigger.Timeout(duration) =>
      val t = umlFactory.createTrigger()
      val e = umlFactory.createTimeEvent()
      e.setIsRelative(true)
      val time = umlFactory.createTimeExpression()
      val o = umlFactory.createOpaqueExpression()
      o.getLanguages.add("SCALA")
      o.getBodies.add(duration.toString)
      time.setExpr(o)
      e.setWhen(time)
      model.getPackagedElements.add(e)
      t.setEvent(e)
      trig.uml = Some(t)
      ts.getTriggers.add(t)
  }

  private def getOrCreatePackage(namespace: uml.Package, name: String): uml.Package = {
    val nested = collectionAsScalaIterable(namespace.getNestedPackages)
    nested.find(_.getName == name).getOrElse {
      val pkg = umlFactory.createPackage()
      pkg.setName(name)
      namespace.getNestedPackages.add(pkg)
      pkg
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  /// NAME RESOLUTION  /////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  def resolveTypeName(scope: uml.Namespace, name: Name): Option[uml.Type] = {
    def resolveTypeNameInternal(scope: uml.Namespace, name: Name): Option[uml.Type] = name match {
      case ResolvedName(Types.Unit | Types.Null) => Some(null)
      case ResolvedName(t: PrimitiveType[_]) =>
        val primTypes = collectionAsScalaIterable(primitives.getContents.get(0).eContents())
        primTypes.collectFirst {
          case tpe: uml.Type if tpe.getName == t.name => tpe
        }
      case ResolvedName(other) =>
        resolveTypeNameInternal(scope,SimpleName(other.name) at name)
      case PathName(name) =>
        if (name.isEmpty) None
        else if (name.length == 1) resolveTypeNameInternal(scope,SimpleName(name.head)) else None
      case SimpleName(n) =>
        val members = collectionAsScalaIterable(scope.getMembers)
        val res = members.find(_.getName == n) collect {
          case tp: uml.Type => tp
        }
        val above = Option(scope.getNamespace)
        res orElse above.flatMap(resolveTypeNameInternal(_,SimpleName(n) at name))
    }

    resolveTypeNameInternal(scope,name) orElse {
      error(name.pos, s"not found: type ${name.parts.mkString("::")}")
      None
    }
  }

  def naming(elem: Element): Unit = elem match {
    case Diagram(DiagramKind.BlockDefinitionDiagram, "package", meName, name, content) =>
      content.foreach(naming)
      elem.uml.collect {
        case pkg: uml.Package => validate.validatePackage(pkg,chain,context)
      }
    case b: Block =>
      b.members.foreach(naming)
      elem.uml.collect {
        case blk: sysml.blocks.Block =>
          validate.validateClass(blk.getBase_Class,chain,context)
      }
    case Operation(name,tpe,params,constraints) =>
      elem.uml.collect {
        case op: uml.Operation =>
          resolveTypeName(op.getClass_,tpe.name).fold {
            error(tpe.name.pos, s"unknown type $tpe")
          } { tpe =>
            op.setType(tpe)
          }
      }
      params.map(naming)
      elem.uml.collect {
        case op: uml.Operation =>
          validate.validateOperation(op,chain,context)
      }
    case Parameter(name,tpe) =>
      elem.uml.collect {
        case op: uml.Parameter =>
          resolveTypeName(op.getOperation.getClass_,tpe.name)
            .foreach(op.setType)
      }
    case Reference(name,tpe,opposite,constraint) =>
      elem.uml.collect {
        case op: uml.Property =>
          resolveTypeName(op.getClass_,tpe.name).foreach{ tpe =>
            op.setType(tpe)
            val opp = (opposite, tpe) match {
              case (Some(o),tpe: uml.Classifier) =>
                collectionAsScalaIterable(tpe.getAttributes).find { p =>
                  p.getName == o
                }
              case _ => None
            }
            opp.foreach(op.setOpposite)
          }
      }
    case Property(name,tpe,constraint) =>
      elem.uml.collect {
        case op: uml.Property =>
          resolveTypeName(op.getClass_,tpe.name).foreach { tpe =>
            op.setType(tpe)
          }
      }
    case Port(name,dir,tpe) =>
    case Value(name,tpe) =>
      elem.uml.collect {
        case op: uml.Property =>
          resolveTypeName(op.getClass_,tpe.name).foreach { tpe =>
            op.setType(tpe)
          }
      }
    case UnprocessedConstraint(_) =>
    case StateMachine(name,states) =>
      states.map(naming)
    case ConcreteState(name,tss,initial) =>
      tss.map(naming)
    case Choice(tss) =>
      tss.map(naming)
    case Transition(trigger,guard,action,UnresolvedTargetStateName(name)) =>
      trigger.foreach(naming)
      elem.uml.collect {
        case t: uml.Transition =>
          val vertices = collectionAsScalaIterable(t.getContainer.getSubvertices)
          val target = if (name.parts.length == 1)
            vertices.find(_.getName == name.parts.head)
          else
            None
          target.fold {
            error(name.pos, s"could not resolve vertex $name")
          } { t.setTarget }
      }
    case Transition(trigger,guard,action,InlineTargetState(st)) =>
      trigger.foreach(naming)
      naming(st)
    case Trigger.Receive(portName, variable) =>

    case Trigger.Timeout(_) => // nothing to do here
    case other =>
      error(elem.pos, s"could not synthesize $other")
  }

  //////////////////////////////////////////////////////////////////////////////
  /// CONSTRAINT PARSING  //////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  lazy val ocl = {

    val ocl = pivot.utilities.OCL.newInstance(pivot.utilities.OCL.NO_PROJECTS,library)
    ocl
  }

  def parseConstraints(elem: Element): Unit = elem match {
    case Diagram(DiagramKind.BlockDefinitionDiagram, "package", meName, name, content) =>
      content.foreach(parseConstraints)
    case b: Block =>
      b.members.foreach {
        case UnprocessedConstraint(str) =>
          val name = b.uml.collect {
            case c: uml.Class =>
              println(c)
              try {
                val cls = ocl.getMetamodelManager.getASOf(classOf[org.eclipse.ocl.pivot.Class],c)
                val help = ocl.createOCLHelper(cls)
                val constr = help.createInvariant(str)
                println(constr)
              } catch {
                case e: ParserException =>
                  println(e.getDiagnostic.toString)
                  //e.printStackTrace()
              }
          }
        case other => // parseConstraints(other)
      }
    case Operation(name,tpe,params,constraints) =>
      params.foreach(parseConstraints)
      constraints.foreach(parseConstraints)
    case Reference(name,tpe,opposite,constraint) =>
      constraint.foreach(parseConstraints)
    case Property(name,tpe,constraint) =>
    case Port(name,dir,tpe) =>
    case Value(name,tpe) =>
    case UnprocessedConstraint(tokens) =>
      //ocl.parse(new OCLInput(tokens))
      println(tokens)
      println(elem.pos.longString)
    case StateMachine(name,states) =>
      states.foreach(parseConstraints)
    case ConcreteState(name,tss,initial) =>
      tss.foreach(parseConstraints)
    case Choice(tss) =>
      tss.foreach(parseConstraints)
    case Transition(trigger,guard,action,UnresolvedTargetStateName(name)) =>
      trigger.foreach(parseConstraints)
    case Transition(trigger,guard,action,InlineTargetState(st)) =>
      trigger.foreach(parseConstraints)
      parseConstraints(st)
    case Trigger.Receive(portName, variable) =>
    case Trigger.Timeout(_) =>
    case other =>
      error(elem.pos, s"could not synthesize $other")
  }

  private def warn(pos: Position, message: String) =
    println(s"WARN: $pos: $message\n${pos.longString}")
  private def error(pos: Position, message: String) =
    println(s"ERROR: $pos: $message\n${pos.longString}")
  private def abort(pos: Position, message: String) =
    println(s"ERROR: $pos: $message\n${pos.longString}")

  def save() = {
    validate.validateModel(model,chain,new util.HashMap)
    resource.save(mapAsJavaMap(Map.empty[Any,Any]))
  }
}
