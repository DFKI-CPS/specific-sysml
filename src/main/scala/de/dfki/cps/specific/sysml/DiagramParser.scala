package de.dfki.cps.specific.sysml

import java.io.File

import specific.sysml.parser.{IndentScanner, SysMLLexer, SysMLParsers}

import scala.io.Source
import spray.json._
import spray.json.DefaultJsonProtocol._

import scala.collection.mutable

/**
  * This case class is an immediate Representationform of a connection between Blocks. It recieves a syntactically
  * correct connection. The function semanticaly analyses the connections and remove reverse connections
  * canEqual is overwritten in such way, that a BlockConnection is equivalent if from from and l_start are swaped with to and l_end
  *
  * @param from    Represents from in a Connection
  * @param to      Represents to in a Connection
  * @param l_start Represents the Map Label Start in a Connection
  * @param l_end   Represents the Map Label End in a Connection
  */
case class BlockConnection(from: String, to: String, l_start: String, l_end: String, mul_start: Option[Multiplicity], mul_end: Option[Multiplicity]) { // contravariant type bounds
  def isOpposite(that: BlockConnection) = that match {
    case BlockConnection(from, to, l_start, l_end, _, _) =>
      this.from == to && this.to == from && this.l_end == l_start && this.l_start == l_end
  }
}

object BlockConnection {
  /**
    * Serializer for BlockConnection to JsValue
    */
  implicit val blockConnectionJsonFormat = new JsonFormat[BlockConnection] {
    def read(js: JsValue) = ???

    def write(connection: BlockConnection): JsValue = {
      JsObject(
        "from" -> JsString(connection.from),
        "to" -> JsString(connection.to),
        "labels" -> Map(
          "start" -> (connection.l_start + connection.mul_start.fold("")(_.toString)),
          "end" -> (connection.l_end + connection.mul_end.fold("")(_.toString))
        ).toJson
      )
    }
  }

}


object DiagramParser extends App {
  var useOCL = false
  val usedConnections: Map[String, String] = Map()

  /**
    * Converts a NamedElement from the SysML parser to json
    * Uses the following Syntax Modell:
    *
    * Positional = Filepositinal | TypeAnnotation | TimeEvent
    * FilePostional = Element
    * Element =  Comment | Mapping | Satifsfy | Trace | Project | BlockCompartment | TransistionTarget | Trigger |
    * BlockCompatement = UnsupportedCompartment | PropertiesCompartment |  ValuesCompartment | OperationsCompartment
    * BehaviorCompartment | ConstraintsCompartment | ReferencesCompartment | PortsCompartment |
    * DiagramKind = ActivityDiagram | BlockDefinitionDiagram | InternalBlockDiagram | InternalBlockDiagram | ParametricDiagram | RequirementDiagram |
    * SequenceDiagram | StateMachineDiagram |  UseCaseDiagram
    * NameSpace = Diagram | Package
    * NamedElement = Realization | BlockMember | State | Requirement | State
    * BlockMember = Property | Reference | Operation | Parameter | Port | StateMachine
    * Classifier = Block
    * ConstraintType = Inv | Pre | Post | Body | Init | Derive | Query
    * Blockmember = UnprocessedConstraint
    * FlowDirection = In | Out | InOut
    * State = PseudoState
    * PseudoState = Choice
    * TransistionTarget = InlineTargetState | UnresolvedTargetStateName
    * ShortConstraint = Subsets
    *
    * @param element Takes a Diagram from the Parser
    * @return the json representation
    */

  def convert(element: NamedElement): JsValue = element match {
    case Diagram(DiagramKind.BlockDefinitionDiagram, m, n, name, members) =>
      Map(
        "title" -> Array("Block Definition Diagram", s"[$m]", n.mkString("::"), name).toJson,
        "blocks" -> members.collect {
          case Block(name, compartments, comments) =>
            name -> Map(
              "stereotype" -> "block".toJson,
              "compartments" -> compartments.collect { // Hier die Ausnahmen, welche separat geparsed werdenn
                case comp if (!((comp.compartmentName == "references") || (comp.compartmentName == "operations") || (comp.compartmentName == "constraints")))
                => comp.compartmentName -> comp.content.map(convert).toJson
                case comp if (comp.compartmentName == "constraints" && useOCL) => comp.compartmentName -> comp.content.map(_.toString).toJson
                case comp if (comp.compartmentName == "operations") => comp.compartmentName -> comp.content.map(_.toString).toJson
              }.toMap.toJson
            ).toJson
        }.toMap.toJson,
        "connectors" -> {
          val refs = members.collect { case b: Block => b }.flatMap { from =>
            from.compartments.collect { case r: ReferencesCompartment => r }.flatMap(r => r.references.map((from, _)))
          }
          filterConnections(parseNodes(refs, members)).toJson
        }
      ).toJson
    case default => default.toString.toJson
  }


  /**
    * (refs: Seq[Reference], members: Seq[NamedElement], from: Block) -> Seq[BlockConnection] -> Option[Set[BlockConnection]
    *
    * Parse the Parameters to a Sequence of Block Connections. Syntax Correct, Semantic Not
    *
    * @param refs
    * @param members
    * @return
    */
  def parseNodes(refs: Seq[(Block,Reference)], members: Seq[NamedElement]): Seq[BlockConnection] =
    refs.map{ case (from,to) => {
      val c = {
        to.typeAnnotation.name.parts.mkString("::")
      }
      val opposite = to.oppositeName.flatMap(n => members.collect { case b: Block if b.rawName == c => b }
        .flatMap(_.members.collect { case r: Reference if r.name == n => r }).headOption) // her dopllet?

      BlockConnection(from.rawName, c, to.oppositeName.getOrElse(from.rawName.toLowerCase), to.name, opposite.flatMap(_.typeAnnotation.multiplicity), to.typeAnnotation.multiplicity)
  }}

  /**
    * Takes an Sequence of BlockConnections with correct Syntax and return it Semantically correct
    * The Sequence of connections is a  Bigraph, which should be returned as a Graph.
    * In case of an Error None is returned.
    *
    * Fixes the Semantic
    *
    * @param connection Seq[BlockConnection]
    * @return Option[Set[BlockConnection]]
    **/
  def filterConnections(connection: Seq[BlockConnection]): Seq[BlockConnection] = {
    val buf = mutable.Buffer.empty[BlockConnection]
    for (c <- connection) {
      buf.indexWhere(c.isOpposite) match {
        case -1 => buf.append(c)
        case n =>
          val o = buf.remove(n)
          val r = BlockConnection(c.from, c.to, c.l_start, c.l_end, c.mul_start orElse o.mul_end, c.mul_end orElse o.mul_start)
          buf.append(r)
      }
    }
    connection.toSet
    buf.toSeq
  }




  /**
    * Take a .sysml file and return its json representation
    * Use SysMLParsers, if successful convert to json
    *
    * @param source Specify where a .sysml file is saved
    * @return returns the prettyprinted json representation
    */
  def fileToJsonString(source: File) = {
    // Read from File and run SysML Lexer
    val textSource = Source.fromFile(source)
    val tokens = IndentScanner(new SysMLLexer.Scanner(textSource.mkString))


    SysMLParsers.phrase(SysMLParsers.diagram)(tokens) match {
      case SysMLParsers.Success(b: Diagram, _) =>
        convert(b).prettyPrint // IR
      case err@SysMLParsers.NoSuccess(msg, i) =>
        s"parse error [${source.getName}:${i.pos.line}:${i.pos.column}]: $msg" +
          i.pos.longString
    }
  }

  useOCL = true
  println("diagram = " + fileToJsonString(new File("example.sysml")))
}