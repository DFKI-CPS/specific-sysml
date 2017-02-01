bdd [package] fsl2::acs [ACS]
-----------------------------------------------------------

-- P6: The geometry of buildings serves to define which
-- buildings can communicate between each other, and in
-- which direction.
block Building
  operations
    enter(p: Person)
      pre:  p.aut->includes(self)
      pre:  p.sit.gate->includes(self)
      post: p.sit = self
  references
    building: Building[*] <- gate
    gate: Building[*] <- building
  ports
    incoming: Person
  owned behaviors
    state machine EnterBehavior
      initial state Waiting
        receive incoming(p) / enter(p) -> Waiting

block Person
  references
    aut: Building[*]
    sit: Building[1] { subsets aut }