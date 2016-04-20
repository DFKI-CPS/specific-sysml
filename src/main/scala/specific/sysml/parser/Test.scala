package specific.sysml.parser

import scala.util.parsing.input.{CharSequenceReader, Reader}

/**
  * Created by martin on 19.04.16.
  */
object Test extends App {
  val input2 =
    "block Test\n" +
    "\t\treferences\n"+
    "  operations"

  val input3 =
    """
      |block Blub
      |  references
      |    gate: Building[3..*] <- building
      |    building: Building[1] <- gate
      |  operations
      |    admitted(s: String): Boolean
      |       { pre: blub }
      |  owned behaviors
      |    state machine Blub
      |      state Bla
      |      state Blub
      |        after 3 seconds -> Bla
      |        receive blub -> Foo
      |      state Foo
      |  ports
      |    in blub: Int
      |  constraints
      |    { bla }
      |""".stripMargin


  val input =
    """package ACS
      |
      |block Building
      |  references
      |    gate: Building[*] <- building
      |    building: Building[*] <- gate
      |    org_dom: Door[*] <- org
      |  constraints
      |    { self.gate = self.org_dom.dest }
      |    { not (self.gate->includes(self)) }
      |
      |block Person
      |  operations
      |    admitted(q: Door): Boolean {
      |      post: q.org = self.sit and
      |            self.aut->includes(q.dest) and
      |            self.dap_dom->isEmpty()
      |    }
      |  references
      |    aut: Building[*]
      |    sit: Building[1] {subsets aut}
      |    dap_dom: Door <- dap
      |  constraints
      |    { self.aut->forAll(b|self.aut.building->includes(b) }
      |    { Person.allInstances()->forAll(p1, p2 | p1.dap_dom->notEmpty() and p1.dap_dom = p2.dap_dom implies p1 = p2) }
      |
      |block Door
      |  values
      |    green: Boolean
      |    red: Boolean
      |  operations
      |    accept(p: Person): Unit
      |      { pre: p.admitted(self) }
      |      { post: p.sit == p }
      |    refuse(p: Person): Unit
      |      { post: p.sit == p@pre.sit }
      |    pass_thru(): Unit
      |    off_grn(): Unit {post: not green}
      |    off_red(): Unit {post: not red}
      |  references
      |    org: Building[1] <- org_dom {subsets dap.sit}
      |    dest: Building[1] {subsets dap.aut}
      |    dap: Person <- dap_dom
      |  ports
      |    in incoming: Person
      |    in passing: Boolean
      |  owned behaviors
      |    state machine DoorBehavior
      |      state Waiting
      |        receive incoming(p) -> choose
      |          [{ p.admitted(this) }] / { accept(p) }     -> Accepting
      |          [{ not p.admitted(this) }] / { refuse(p) } -> Refusing
      |      state Accepting
      |        after 30 seconds              -> Waiting
      |        receive passing / { pass_thru() } -> Waiting
      |      state Refusing
      |        after 30 seconds -> Waiting
      |
      |{
      |context Door::accept(p: Person):
      |  pre accept_pre1: not (green or red)
      |  pre accept_pre2: p.admitted(self)
      |  post accept_post: self.dap = p and p.dap_dom= self
      |
      |
      |context Door::pass_thru():
      |  pre pass_pre:  self.green
      |  post pass_post:
      |     self.dap@pre.sit = self.dest and
      |     self.dap@pre.dap_dom->isEmpty() and
      |     self.dap->isEmpty() -- implies not (self.green)
      |
      |context Door::off_grn():
      |  pre off_grn_pre:  self.green
      |  post off_grn_post:
      |     self.dap@pre.dap_dom->isEmpty() and
      |     self.dap->isEmpty()
      |
      |-- Not mentioned explicitly:
      |-- If a door is not accepted for a certain person, it is refused.
      |context Door::refuse(p: Person):
      |  pre refuse_pre_1: not (green or red)
      |  pre refuse_pre_2: not (p.admitted(self))
      |  post refuse_post: self.red
      |
      |-- P15: The red light of a door whose access has just been refused stays on
      |-- for a period of 2 seconds, the door stays blocked of course.
      |context Door::off_red():
      |  post off_red_post: self.red = false
      |}""".stripMargin
  var tokens: Reader[Lexer.Token] = new IndentScanner(new Lexer.Scanner(input))

  Parser.phrase(Parser.pkg)(tokens) match {
    case Parser.Success(b,_) => println(b)
    case Parser.NoSuccess(msg,i) =>
      println(s"$msg [${i.pos}]:\n${i.pos.longString}")
  }

}
