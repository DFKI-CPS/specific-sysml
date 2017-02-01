bdd [package] fsl0::acs [ACS]
-----------------------------------------------------------

-- P1: The model is composed of people and buldings.
block Building

-- P1: The model is composed of people and buldings.
block Person
  operations
    pass(b: Building)
      pre:  aut->includes(b)
      pre:  sit <> b
      post: sit = b
  references
    -- P2: Each person is authorized to enter certain buldings.
    aut: Building[*]
    -- P3: A person can only be in one building.
    -- P4: A person must be in at least one building.
    -- P5: Any person in a given building is authorised to be there.
    sit: Building[1] { subsets aut }