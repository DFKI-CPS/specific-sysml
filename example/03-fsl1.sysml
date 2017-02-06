bdd [package] fsl1::acs [ACS]
-----------------------------------------------------------

-- P6: The geometry of buildings serves to define which
-- buildings can communicate between each other, and in
-- which direction.
block Building
  references
    gate: Building[*] <- building
    building: Building[*] <- gate
  constraints
    -- P7: A building does not communicate with itself.
    inv P7: not (self.gate->includes(self))

block Person
  operations
    pass(b: Building)
      pre: aut->includes(b)
      -- P8: A person can only go from one building in
      -- which he is to another where he desires to go if
      -- these two buildings communicate with each other.
      pre P8: sit.gate->includes(b)
      post: sit = b
  references
    aut: Building[*]
    sit: Building[1] { subsets aut }
  constraints
    -- P10: Any person authorised to be in a building must
    -- also be authorised to go into another building which
    -- communicates with the first one.
    inv P10: aut->forAll(b|aut.building->includes(b))