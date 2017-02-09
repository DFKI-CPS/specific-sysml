package de.dfki.cps.specific

import java.io.File

import de.dfki.cps.specific.sysml.{Diagram, Mapping, Project, Synthesis}
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.Resource.Diagnostic
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.ocl.{Environment, OCL, SemanticException, SyntaxException}
import org.eclipse.ocl.expressions
import org.eclipse.ocl.uml.{ExpressionInOCL, UMLEnvironmentFactory, Variable}
import org.eclipse.papyrus.sysml.requirements.RequirementsFactory
import org.eclipse.uml2.uml
import org.eclipse.uml2.uml.OpaqueExpression
import specific.sysml.parser.{IndentScanner, SysMLLexer, SysMLParsers}

import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.io.Source
import scala.util.control.NonFatal
import scala.util.parsing.input.{Position, Reader}

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
object SysML {
  def load(source: File, target: Resource, includeOCL: Boolean = false, includeProfileApplcations: Boolean = true): Map[EObject, Position] = {
    val textSource = Source.fromFile(source)
    val tokens = new IndentScanner(new SysMLLexer.Scanner(textSource.mkString))

    SysMLParsers.phrase(SysMLParsers.diagram)(tokens) match {
      case SysMLParsers.Success(b: Diagram,_) =>
        val synth = new Synthesis(target)
        synth.includeOCL = includeOCL
        synth.includeProfileApplications = includeProfileApplcations
        synth.structure(b)
        synth.naming(b)
        synth.parseConstraints(b)
        synth.messages.foreach(println)
        synth.positions.toMap
      case SysMLParsers.NoSuccess(msg,i) =>
        sys.error(s"$msg [${i.pos}]:\n${i.pos.longString}")
      case other =>
        sys.error("expected project definition")
    }
  }

  def loadProject(source: URI, target: Resource, load: URI => Resource): Map[EObject,Position] = {
    val textSource = Source.fromFile(source.toFileString)
    val tokens = new IndentScanner(new SysMLLexer.Scanner(textSource.mkString))

    val positions = mutable.Map.empty[EObject,Position]

    val project = SysMLParsers.phrase(SysMLParsers.project)(tokens) match {
      case SysMLParsers.Success(b: Project,_) => b
      case SysMLParsers.NoSuccess(msg,i) =>
        sys.error(s"$msg [${i.pos}]:\n${i.pos.longString}")
    }

    val resources = project.includes
      .map(source.trimSegments(1).appendSegment)
      .map(load)

    var satisfies = 0

    project.satisfy.foreach { trace =>
      val Seq(modelName,path@_*) = trace.requirement.parts
      val model = resources.flatMap(_.getContents.asScala).collectFirst {
        case m: uml.Model if m.getName == modelName => m
      }
      if (model.isEmpty)
        println(s"error: could not find model $modelName")
      val requirement = model.flatMap { model =>
        Option(EcoreUtil.getEObject(model,path.mkString("/")))
      }
      if (requirement.isEmpty)
        println(s"error: could not find requirement ${path.mkString("::")}")
      val targets = trace.elements.map { elemName =>
        val Seq(targetModelName,path@_*) = elemName.parts
        val targetModel = resources.flatMap(_.getContents.asScala).collectFirst {
          case m: uml.Model if m.getName == targetModelName => m
        }
        if (targetModel.isEmpty)
          println(s"error: could not find model $targetModelName")
        val target = targetModel.flatMap { model =>
          Option(EcoreUtil.getEObject(model,path.mkString("/")))
        }
        if (target.isEmpty)
          println(s"error: could not find element ${path.mkString("::")}")
        target
      }
      requirement.foreach {
        case x: uml.NamedElement =>
          val abstr = uml.UMLFactory.eINSTANCE.createAbstraction()
          val satisfy = RequirementsFactory.eINSTANCE.createSatisfy()
          positions += abstr -> trace.pos
          positions += satisfy -> trace.pos
          satisfy.setBase_Abstraction(abstr)
          abstr.getSuppliers.add(x)
          abstr.getClients.addAll(
            targets.collect {
              case Some(x: uml.NamedElement) =>
                satisfies += 1
                x
            }.asJava
          )
          target.getContents.add(abstr)
          target.getContents.add(satisfy)
      }
    }

    val ocl = OCL.newInstance(new UMLEnvironmentFactory(target.getResourceSet))

    val mapped = mutable.HashMap.empty[uml.NamedElement,uml.NamedElement]
    val unmapped = mutable.HashSet.empty[uml.NamedElement]
    var autoCount = 0

    def automap(supplyingContext: uml.Namespace, clientContext: uml.Namespace): Unit = {
      supplyingContext.getOwnedMembers.asScala.foreach { supplier =>
        if (!mapped.contains(supplier) && !supplier.isInstanceOf[uml.Constraint] && !supplier.isInstanceOf[uml.Event] && !supplier.isInstanceOf[uml.Realization]) {
          val client = clientContext.getOwnedMembers.asScala.find(_.getName == supplier.getName)
          client.fold[Unit] {
            unmapped += supplier
          } { client =>
            autoCount += 1
            mapped += supplier -> client
            val realization = uml.UMLFactory.eINSTANCE.createRealization()
            realization.getSuppliers.add(supplier)
            realization.getClients.add(client)
            target.getContents.add(realization)
            (supplier,client) match {
              case (supplier: uml.Namespace,client: uml.Namespace) =>
                automap(supplier,client)
              case other => // nothing
            }
          }
        }
      }
    }

    type UMLEnvrionment = Environment[
      uml.Package,
      uml.Classifier,
      uml.Operation,
      uml.Property,
      uml.EnumerationLiteral,
      uml.Parameter,
      uml.State,
      uml.CallOperationAction,
      uml.SendSignalAction,
      uml.Constraint,
      uml.Class,
      uml.Element
    ]

    val opCallMappings = mutable.Map.empty[uml.Operation,Seq[expressions.OCLExpression[uml.Classifier]] => expressions.OCLExpression[uml.Classifier]]
    val parameterMappings = mutable.Map.empty[uml.Parameter,expressions.OCLExpression[uml.Classifier]]
    val selfMappings = mutable.Map.empty[uml.Operation,expressions.OCLExpression[uml.Classifier]]

    def submapping(supplyingContext: uml.Namespace, clientContext: uml.Namespace, otherMappings: Seq[Mapping])(mapping: Mapping): () => Unit = {
      if (supplyingContext.isInstanceOf[uml.Operation]) {
        val op = mapped(supplyingContext).asInstanceOf[uml.Operation]
        val helper = ocl.createOCLHelper()
        helper.setContext(op.getClass_)
        val variables = op.getOwnedParameters.asScala.map { param =>
          val v = expressions.ExpressionsFactory.eINSTANCE.createVariable[uml.Classifier, uml.Parameter]()
          v.setRepresentedParameter(param)
          v.setName(param.getName)
          v
        }
        variables.foreach { v =>
          helper.getEnvironment.asInstanceOf[UMLEnvrionment].addElement(v.getName, v, true)
        }
        helper.setValidating(false)
        val q = helper.createQuery(mapping.client.content)
        if (mapping.supplier == "self") {
          selfMappings(supplyingContext.asInstanceOf[uml.Operation]) = q
        } else {
          val supplier = supplyingContext.getMembers.asScala.find(_.getName == mapping.supplier)
          if (supplier.isEmpty) {
            println(s"error: ${mapping.pos} element '${mapping.supplier}' is not a member of ${supplyingContext.getName}")
            println(mapping.pos.longString)
          }
          supplier.foreach { case supplier: uml.Parameter =>
            parameterMappings(supplier) = q
          }
        }
        () => ()
      } else {
        val supplier = supplyingContext.getMembers.asScala.find(_.getName == mapping.supplier)
        if (supplier.isEmpty) {
          println(s"error: ${mapping.pos} element '${mapping.supplier}' is not a member of ${supplyingContext.getName}")
          println(mapping.pos.longString)
        }
        if (clientContext.isInstanceOf[uml.Package]) {
          val client = clientContext.getMembers.asScala.find(_.getName == mapping.client.content)
          if (client.isEmpty) {
            println(s"error: ${mapping.client.pos} element '${mapping.client.content}' is not a member of ${clientContext.getName}")
            println(mapping.client.pos.longString)
          }
          val res = for {
            supplier <- supplier
            client <- client
          } yield {
            mapped += supplier -> client
            val realization = uml.UMLFactory.eINSTANCE.createRealization()
            positions += realization -> mapping.pos
            realization.getSuppliers.add(supplier)
            realization.getClients.add(client)
            target.getContents.add(realization)
            () => {
              val defer = mapping.subMappings.map(submapping(supplier.asInstanceOf[uml.Namespace], client.asInstanceOf[uml.Namespace], mapping.subMappings))
              automap(supplier.asInstanceOf[uml.Namespace], client.asInstanceOf[uml.Namespace])
              defer.foreach(_ ())
            }
          }
          res.getOrElse(() => ())
        } else if (clientContext.isInstanceOf[uml.Classifier]) {
          val res = for {
            supplier <- supplier
          } yield {
            val helper = ocl.createOCLHelper()
            val mappedCls = mapped(supplyingContext).asInstanceOf[uml.Classifier]
            helper.setContext(mappedCls)
            val variables = supplier match {
              case op: uml.Operation =>
                op.getOwnedParameters.asScala.map { param =>
                  val v = expressions.ExpressionsFactory.eINSTANCE.createVariable[uml.Classifier, uml.Parameter]()
                  v.setName(param.getName)
                  v.setType(mapped(param.getType).asInstanceOf[uml.Classifier])
                  v
                }
              case prop: uml.Property =>
                val v = expressions.ExpressionsFactory.eINSTANCE.createVariable[uml.Classifier, uml.Parameter]()
                v.setName(prop.getName)
                v.setType(mapped(prop.getType).asInstanceOf[uml.Classifier])
                Seq(v)
            }
            variables.foreach { v =>
              helper.getEnvironment.asInstanceOf[UMLEnvrionment].addElement(v.getName, v, true)
            }
            helper.setValidating(false)
            val client = try {
              val clientExpr = helper.createQuery(mapping.client.content)

              def extractClient(expr: org.eclipse.ocl.expressions.OCLExpression[uml.Classifier]): Option[uml.NamedElement] = expr match {
                case prop: org.eclipse.ocl.uml.PropertyCallExp =>
                  Some(prop.getReferredProperty)
                case op: org.eclipse.ocl.uml.OperationCallExp =>
                  Some(op.getReferredOperation)
                case iter: org.eclipse.ocl.uml.IteratorExp =>
                  extractClient(iter.getBody)
                case other =>
                  println("error: " + other)
                  None
              }

              extractClient(clientExpr)
            } catch {
              case s: SyntaxException =>
                println(s"error: ${mapping.client.pos} ${s.getMessage}")
                println(mapping.client.pos.longString)
                None
              case s: SemanticException =>
                println(s"error: ${mapping.client.pos} ${s.getMessage}")
                println(mapping.client.pos.longString)
                None
            }
            val res = for {
              client <- client
            } yield {
              mapped += supplier -> client
              val realization = uml.UMLFactory.eINSTANCE.createRealization()
              positions += realization -> mapping.pos
              realization.getSuppliers.add(supplier)
              realization.getClients.add(client)
              val umapping = uml.UMLFactory.eINSTANCE.createOpaqueExpression()
              positions += umapping -> mapping.client.pos
              umapping.getLanguages.add("OCL")
              umapping.getBodies.add(mapping.client.content)
              realization.setMapping(umapping)
              target.getContents.add(realization)
              () =>
                (supplier, client) match {
                  case (supplier: uml.Namespace, client: uml.Namespace) =>
                    val defer = mapping.subMappings.map(submapping(supplier, client, mapping.subMappings))
                    automap(supplyingContext, clientContext)
                    defer.foreach(_ ())
                  case other =>
                    assert(mapping.subMappings.isEmpty)
                    automap(supplyingContext, clientContext)
                }
            }
            res.getOrElse(() => ())
          }
          res.getOrElse(() => ())
        } else {
          target.getErrors.add(
            new Diagnostic {
              def getMessage = "error"
              def getLine = mapping.pos.line
              def getColumn = mapping.pos.column
              def getLocation = mapping.file
            }
          )
          () => ()
        }
      }
    }

    project.mappings.foreach { mapping =>
      val modelName = mapping.supplier
      val model = resources.flatMap(_.getContents.asScala).collectFirst {
        case m: uml.Model if m.getName == modelName => m
      }
      if (model.isEmpty)
        println(s"error: could not find model $modelName")
      val name = mapping.client.content.trim
      val clientModel = resources.flatMap(_.getContents.asScala).collectFirst {
        case m: uml.Model if m.getName == name => m
      }
      if (clientModel.isEmpty)
        println(s"error: could not find model $name")
      for {
        supplier <- model
        client <- clientModel
      } {
        mapped += supplier -> client
        val realization = uml.UMLFactory.eINSTANCE.createRealization()
        realization.getSuppliers.add(supplier)
        realization.getClients.add(client)
        target.getContents.add(realization)
        val defer = mapping.subMappings.map(submapping(supplier,client,mapping.subMappings))
        automap(supplier,client)
        defer.foreach(_())
      }
    }

    def translateExpr(expr: expressions.OCLExpression[uml.Classifier], selfExpr: Option[expressions.OCLExpression[uml.Classifier]]): expressions.OCLExpression[uml.Classifier] = {
      expr match {
        case c: expressions.CollectionLiteralExp[uml.Classifier] =>
          val r = EcoreUtil.copy(c)
          val ps = r.getPart.asScala.map {
            case i: expressions.CollectionItem[uml.Classifier] =>
              val r = EcoreUtil.copy(i)
              r.setItem(translateExpr(r.getItem,selfExpr))
              r
            case ra: expressions.CollectionRange[uml.Classifier] =>
              val r = EcoreUtil.copy(ra)
              r.setFirst(translateExpr(r.getFirst,selfExpr))
              r.setLast(translateExpr(r.getLast,selfExpr))
              r
          }
          r.getPart.clear()
          r.getPart.addAll(ps.asJava)
          r
        case x: expressions.LiteralExp[uml.Classifier] =>
          EcoreUtil.copy(x)
        case c: expressions.IfExp[uml.Classifier] =>
          val r = EcoreUtil.copy(c)
          r.setCondition(translateExpr(r.getCondition,selfExpr))
          r.setThenExpression(translateExpr(r.getThenExpression,selfExpr))
          r.setElseExpression(translateExpr(r.getElseExpression,selfExpr))
          r
        case v: expressions.VariableExp[uml.Classifier, uml.Parameter] =>
          val r = EcoreUtil.copy(v)
          if (v.getName == "self") {
            selfExpr.getOrElse(r)
          } else {
            parameterMappings.get(r.getReferredVariable.getRepresentedParameter)
              .getOrElse(r)
          }
        case t: expressions.TypeExp[uml.Classifier] =>
          val r = EcoreUtil.copy(t)
          r
        case fc: expressions.PropertyCallExp[uml.Classifier, uml.Property] =>
          val r = EcoreUtil.copy(fc)
          mapped.get(r.getNavigationSource).foreach {
            case p: uml.Property =>
              r.setName(p.getName)
              r.setNavigationSource(p)
          }
          mapped.get(r.getReferredProperty).foreach {
            case p: uml.Property =>
              r.setName(p.getName)
              r.setReferredProperty(p)
          }
          r.setSource(translateExpr(r.getSource,selfExpr))
          val qf = r.getQualifier.asScala.map(translateExpr(_,selfExpr))
          r.getQualifier.clear()
          r.getQualifier.addAll(qf.asJava)
          r
        case oc: expressions.OperationCallExp[uml.Classifier, uml.Operation] =>
          val r = EcoreUtil.copy(oc)
          r.setSource(translateExpr(r.getSource,selfExpr))
          val args = r.getArgument.asScala.map(translateExpr(_,selfExpr))
          r.getArgument.clear()
          r.getArgument.addAll(args.asJava)
          mapped.get(r.getReferredOperation).foreach {
            case op: uml.Operation =>
              r.setReferredOperation(op)
          }
          r
        case ie: expressions.IteratorExp[uml.Classifier, uml.Parameter] =>
          val r = EcoreUtil.copy(ie)
          r.setBody(translateExpr(r.getBody,selfExpr))
          r.setSource(translateExpr(r.getSource,selfExpr))
          r
        case ie: expressions.IterateExp[uml.Classifier, uml.Parameter] =>
          val r = EcoreUtil.copy(ie)
          r
      }
    }

    val constraints = resources.flatMap { res =>
      res.getAllContents.asScala.collect {
        case cls: uml.Classifier =>
          cls.getOwnedRules.asScala.map { c =>
            val text = c.getSpecification.asInstanceOf[OpaqueExpression].getBodies.get(0).trim
            val help = ocl.createOCLHelper()
            help.setContext(cls)
            val inv = try {
              Some(help.createInvariant(text).getSpecification)
            } catch {
              case NonFatal(e) => None
            }
            inv.foreach {
              case inv: ExpressionInOCL =>
                val expr = inv.getBodyExpression
                val texpr = translateExpr(expr,None)
                val constr = uml.UMLFactory.eINSTANCE.createConstraint()
                val cx = uml.UMLFactory.eINSTANCE.createOpaqueExpression()
                cx.getLanguages.add("OCL")
                cx.getBodies.add(texpr.toString)
                constr.setSpecification(cx)
                target.getContents.add(constr)
                constr.getConstrainedElements.add(c)
            }
          }
        case opn: uml.Operation =>
          val help = ocl.createOCLHelper()
          help.setOperationContext(opn.getClass_,opn)
          opn.getPreconditions.asScala.map { c =>
            val text = c.getSpecification.asInstanceOf[OpaqueExpression].getBodies.get(0).trim
            val inv = help.createPrecondition(text)
            val expr = inv.getSpecification.asInstanceOf[ExpressionInOCL].getBodyExpression
            val texpr = translateExpr(expr,selfMappings.get(opn))
            val constr = uml.UMLFactory.eINSTANCE.createConstraint()
            val cx = uml.UMLFactory.eINSTANCE.createOpaqueExpression()
            cx.getLanguages.add("OCL")
            cx.getBodies.add(texpr.toString)
            constr.setSpecification(cx)
            target.getContents.add(constr)
            constr.getConstrainedElements.add(c)
          }
          opn.getPostconditions.asScala.map { c =>
            val text = c.getSpecification.asInstanceOf[OpaqueExpression].getBodies.get(0).trim
            val inv = help.createPostcondition(text)
            val expr = inv.getSpecification.asInstanceOf[ExpressionInOCL].getBodyExpression
            val texpr = translateExpr(expr,selfMappings.get(opn))
            val constr = uml.UMLFactory.eINSTANCE.createConstraint()
            val cx = uml.UMLFactory.eINSTANCE.createOpaqueExpression()
            cx.getLanguages.add("OCL")
            cx.getBodies.add(texpr.toString)
            constr.setSpecification(cx)
            target.getContents.add(constr)
            constr.getConstrainedElements.add(c)
          }

      }
    }.foreach(_ => ())

    println(s"project contains ${mapped.size*2 + satisfies * 2} mapped elements")
    println(s"$satisfies traceable requirements")
    println(s"${autoCount*2} elements were implicitly mapped")

    if (unmapped.nonEmpty) println( unmapped.size + " unmapped entities ")
    unmapped.foreach { x =>
      println("unmapped: " + x.getQualifiedName)
    }

    positions.toMap
  }
}