bdd [package] fsl2::acs [ACS]
-----------------------------------------------------------

block Building
  operations
    enter(p: Person)
      pre:  p.aut->includes(self)
      pre P8:  p.sit.gate->includes(self)
      post: p.sit = self
  references
    building: Building[*] <- gate
    gate: Building[*] <- building

block Person
  references
    aut: Building[*]
    sit: Building[1] { subsets aut }