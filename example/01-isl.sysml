req [package] isl::acs [ACS]
-----------------------------------------------------------

requirement P1
  The model must comprise people and buildings.

requirement P2
  Each person has authorisation to enter certain buildings (and not others).
  Access to buildings not consigned in this authorization is strictly forbidden.
  This is a permanent assignment.

requirement P3
  At any one moment, a person can only be in one building.

requirement P4
  At any one moment, a person must be in at least one building.

requirement P5
  Any person in a given building is authorised to be there.

-- First refinement

requirement P6
  The geometry of buildings serves to define which buildings can communicate
  between each other, and in which direction.

requirement P7
  A building does not communicate with itself.

requirement P8
  A person can only go from one building in which he is to another where he
  desires to go if these two buildings communicate with each other.

requirement P9
  No persons must remain blocked in the building.

requirement P10
  Any person authorized to be in a building must also be authorized to go in
  another building which communicates with the first one.

-- Remark: P10 implies P9

-- Second refinement

requirement P11
  The buildings communicate with each other via doors, which are one-way.
  We there refer to buildings of origin and destination for each door.

requirement P12
  A person can only get through a door if it is accepted.
  A door can only be accepted for one person at a time.
  Conversely, any person implied in the acceptance of a door cannot
  do the same of another.

requirement P13
  For a door to be accepted for a certain person, this person should be
  inside the building of origin of that door. Moreover, this person should
  be authorized to enter the destination building of that same door.

requirement P14
  The green light of a door is lit as long as the latter is accepted.
  As soon as the person has got through, the door blocks itself again.
  After 30 seconds, if no-one goes through an accepted door,
  the latter will block automatically.
  In both cases, the green light goes off.

requirement P15
  The red light of a door whose access has just been refused stays on for a
  period of 2 seconds, the door stays blocked of course.

requirement P16
  The red and green lights of a door cannot be lit simultaneously.

requirement P17
  Any person is allowed to go through a door which communicates from the
  building he is to a building in which he is authorized to go.
  Morever, this person must not be already engaged with another door.
