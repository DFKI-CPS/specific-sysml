bdd [package] acs [ACS]
-----------------------------------

block Building
  references
    gate: Building[*] <- building
    building: Building[1..0] <- gate
    org_dom: Door[*] <- org
  constraints
    inv: self.gate = self.org_dom.dest
    inv: not (self.gate->includes(self))

block Person
  operations
    admitted(q: Door): Boolean { query }
      post: q.org = self.sit and
            self.aut->includes(q.dest) and
            self.dap_dom->isEmpty()
  references
    aut: Building[*]
    sit: Building[1] <- blub
    dap_dom: Door <- dap
  constraints
    inv: self.aut->forAll(b|self.aut.building->includes(b))
    inv: Person.allInstances()->forAll(p1, p2 | p1.dap_dom->notEmpty() and p1.dap_dom = p2.dap_dom implies p1 = p2)

block Door
  values
    green: Boolean
    red: Boolean
  operations
    accept(p: Person)
      pre: p.admitted(self)
      post: p.sit = p
    refuse(p: Person)
      post: p.sit = p.sit@pre
    pass_thru()
    off_grn()
      post: not green
    off_red()
      post: not red
  references
    org: Building[1] { subsets dap.sit } <- org_dom
    dest: Building[1] { subsets dap.aut }
    dap: Person <- dap_dom
  ports
    in incoming: Person
    in passing: Boolean
  owned behaviors
    state machine DoorBehavior
      initial state Waiting
        receive incoming(p) -> choose
          [ p.admitted(this)     ] / accept(p) -> Accepting
          [ not p.admitted(this) ] / refuse(p) -> Refusing
      state Accepting
        after 30000 ms           -> Waiting
        receive passing / pass_thru() -> Waiting
      state Refusing
        after 30 seconds -> Waiting