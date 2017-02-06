bdd [package] fsl5::acs [ACS]
-----------------------------------------------------------

block Building
  references
    gate: Building[*] <- building
      derive: org_dom.dest
    building: Building[*] <- gate
    org_dom: Door[*] <- org
  constraints
    inv: not gate->includes(self)

block Person
  operations
    admitted(q: Door): Boolean { query }
      post: q.org = self.sit and
        self.aut->includes(q.dest) and
        self.dap_dom->isEmpty()
  references
    aut: Building[*]
    sit: Building[1] { subsets aut }
    dap_dom: Door[0..1] <- dap
  constraints
    inv: aut->forAll(b|self.aut.building->includes(b))
    inv: aut->includes(dap_dom.dest)
    inv: sit->includes(dap_dom.org)
    inv: Person.allInstances()->
             forAll(p1, p2 |p1.dap_dom->notEmpty()
                        and p1.dap_dom = p2.dap_dom
                    implies p1 = p2)

block Door
  values
    green: Boolean
      derive: dap->notEmpty()
    red: Boolean
  operations
    accept(p: Person)
      pre: not (green or red)
      pre: p.admitted(self)
      post: dap = p and p.dap_dom = self
    -- Not mentioned explicitly:
    -- If a door is not accepted for a certain person, it is refused.
    refuse(p: Person)
      pre: not (green or red)
      pre: not (p.admitted(self))
      post: red
    pass_thru()
      pre: green
      post: dap@pre.sit = dest
        and dap@pre.dap_dom->isEmpty()
        and dap->isEmpty()
    off_grn()
      pre: green
      post: dap@pre.dap_dom->isEmpty()
        and dap->isEmpty()
    -- P15: The red light of a door whose access has just been refused stays on
    -- for a period of 2 seconds, the door stays blocked of course.
    off_red()
      post: not red
  references
    org:  Building[1] <- org_dom
    dest: Building[1]
    dap:  Person[0..1] <- dap_dom
  owned behaviors
    state machine EnterBehavior
      initial state Waiting
        accept / -> Accepting
        refuse / -> Refusing
      state Accepting
        off_grn / -> Waiting
        pass_thru / -> Waiting
      state Refusing
        off_red / -> Waiting
