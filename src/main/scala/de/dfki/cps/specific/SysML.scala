package de.dfki.cps.specific

import java.io.File
import java.util

import com.sun.jmx.mbeanserver.NamedObject
import de.dfki.cps.specific.sysml.{Diagram, Mapping, Project, Synthesis}
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.Resource.Diagnostic
import org.eclipse.emf.ecore.util.EcoreUtil
import org.eclipse.ocl
import org.eclipse.ocl.{Environment, OCL, SemanticException, SyntaxException}
import org.eclipse.ocl.expressions.ExpressionsFactory
import org.eclipse.ocl.uml.UMLFactory.{eINSTANCE => oclFactory}
import org.eclipse.ocl.uml.util.OCLUMLUtil
import org.eclipse.ocl.uml.{OCLExpression, UMLEnvironmentFactory, Variable}
import org.eclipse.ocl.util.OCLUtil
import org.eclipse.ocl.utilities.UtilitiesPackage
import org.eclipse.papyrus.sysml.requirements.RequirementsFactory
import org.eclipse.uml2.uml
import specific.sysml.parser.{IndentScanner, SysMLLexer, SysMLParsers}

import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.io.Source
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
              case Some(x: uml.NamedElement) => x
            }.asJava
          )
          target.getContents.add(abstr)
          target.getContents.add(satisfy)
      }
    }

    println("loading realizations")

    val ocl = OCL.newInstance(new UMLEnvironmentFactory(target.getResourceSet))

    val mapped = mutable.HashMap.empty[uml.NamedElement,uml.NamedElement]
    val unmapped = mutable.HashSet.empty[uml.NamedElement]

    def automap(supplyingContext: uml.Namespace, clientContext: uml.Namespace): Unit = {
      supplyingContext.getOwnedMembers.asScala.foreach { supplier =>
        if (!mapped.contains(supplier) && !supplier.isInstanceOf[uml.Constraint] && !supplier.isInstanceOf[uml.Parameter] && !supplier.isInstanceOf[uml.Event]) {
          val client = clientContext.getOwnedMembers.asScala.find(_.getName == supplier.getName)
          client.fold[Unit] {
            unmapped += supplier
          } { client =>
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

    def submapping(supplyingContext: uml.Namespace, clientContext: uml.Namespace, otherMappings: Seq[Mapping])(mapping: Mapping): () => Unit = {
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
            defer.foreach(_())
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
                val v = ExpressionsFactory.eINSTANCE.createVariable[uml.Classifier,uml.Parameter]()
                v.setName(param.getName)
                v.setType(mapped(param.getType).asInstanceOf[uml.Classifier])
                v
              }
            case prop: uml.Property =>
              val v = ExpressionsFactory.eINSTANCE.createVariable[uml.Classifier,uml.Parameter]()
              v.setName(prop.getName)
              v.setType(mapped(prop.getType).asInstanceOf[uml.Classifier])
              Seq(v)
          }
          variables.foreach { v =>
            helper.getEnvironment.asInstanceOf[UMLEnvrionment].addElement(v.getName,v,true)
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
            () => (supplier, client) match {
              case (supplier: uml.Namespace, client: uml.Namespace) =>
                val defer = mapping.subMappings.map(submapping(supplier, client, mapping.subMappings))
                automap(supplyingContext,clientContext)
                defer.foreach(_())
              case other =>
                assert(mapping.subMappings.isEmpty)
                automap(supplyingContext,clientContext)
            }
          }
          res.getOrElse(() => ())
        }
        res.getOrElse(() => ())
      } else {
        target.getErrors.add(new Diagnostic {
          def getMessage = "error"
          def getLine = mapping.pos.line
          def getColumn = mapping.pos.column
          def getLocation = mapping.file
        })
        () => ()
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

    if (unmapped.nonEmpty) println( unmapped.size + " unmapped entities ")
    unmapped.foreach { x =>
      println("unmapped: " + x.getQualifiedName)
    }

    positions.toMap
  }
}
