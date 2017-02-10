bdd [package] fsl0::acs [ACS]
-----------------------------------------------------------

-- P1: The model is composed of people and buldings.
block Building

-- P1: The model is composed of people and buldings.
block Person
  operations
    pass(b: Building)
      pre:  authorized->includes(c)
      pre:  current <> b
      post: current = b
  references
    -- P2: Each person is authorized to enter certain buldings.
    authorized: Building[*]
    -- P3: A person can only be in one building.
    -- P4: A person must be in at least one building.
    -- P5: Any person in a given building is authorised to be there.
    current: Building[1] { subsets authorized }