bdd [package] selfie::acs [ACS]
-------------------------------

block Building
  references
    rooms: Room[*] <- building
    cards: Card[*] <- building
  values
    emergency: Boolean

block Door
  references
    from: Room[1] <- exits
    to: Room[1] <- entries
  values
    isOpen: Boolean
  operations
    pass(card: Card)
      pre: mayPass(card)
      post: card.location = to  
  constraints
    def: mayPass(card: Card): Boolean = 
           if from.building.emergency
           then isOpen 
           else card.location = from and card.authorizations->contains(to)
    inv: from.building = to.building
    inv: isOpen implies from.building.emergency

block Card
  references
    authorizations: Room[*] <- authorized
    location: Room[1] { subsets authorizations } <- checkedIn 
    building: Building[1] <- cards
  constraints
    inv: authorizations->forall(r| r.building = self.building)
    inv: building.rooms->forall(r | not r.authorized->contains(self) implies not r.hasAccess(self)))
    inv: building.rooms->exists(r | r.isSafe and r.hasAccess(self))

block Room
  references
    building: Building[1] <- rooms
    exits: Door[*] <- from
    entries: Door[*] <- to
    authorized: Card[*] <- authorizations
    checkedIn: Card[*] { subsets authorized } <- location 
  values
    isSafe: Boolean  
  constraints
    def: hasAccess(card: Card): Boolean = 
           card.location = self or entries->exists(e | e.mayPass(card) and e.from.hasAccess(card))    