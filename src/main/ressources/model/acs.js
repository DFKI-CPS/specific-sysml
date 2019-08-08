diagram = {
  "title": ["Block Definition Diagram", "[package]", "selfie::acs", "ACS"],
  "blocks": {
    "Building": {
      "stereotype": "block",
      "compartments": {
        "values": ["emergency: Boolean"]
      }
    },
    "Door": {
      "stereotype": "block",
      "compartments": {
        "values": ["isOpen: Boolean"],
        "operations": ["pass(card: Card): Unit"],
        "constraints": ["def: mayPass(card: Card): Boolean = \n           isOpen or card.location = from and card.authorizations->contains(to)", "inv: from.building = to.building", "inv: isOpen implies from.building.emergency"]
      }
    },
    "Card": {
      "stereotype": "block",
      "compartments": {
        "constraints": ["inv: authorizations->forall(r| r.building = self.building)", "inv: building.rooms->forall(r | not r.authorized->contains(self) implies not r.hasAccess(self)))", "inv: building.rooms->exists(r | r.isSafe and r.hasAccess(self))"]
      }
    },
    "Room": {
      "stereotype": "block",
      "compartments": {
        "values": ["isSafe: Boolean"],
        "constraints": ["def: hasAccess(card: Card): Boolean = \n           card.location = self or entries->exists(e | e.mayPass(card) and e.from.hasAccess(card))"]
      }
    }
  },
  "connectors": [{
    "from": "Building",
    "to": "Room",
    "label s": {
      "start": "building[1]",
      "end": "rooms[*]"
    }
  }, {
    "from": "Building",
    "to": "Card",
    "labels": {
      "start": "building[1]",
      "end": "cards[*]"
    }
  }, {
    "from": "Door",
    "to": "Room",
    "labels": {
      "start": "exits[*]",
      "end": "from[1]"
    }
  }, {
    "from": "Door",
    "to": "Room",
    "labels": {
      "start": "entries[*]",
      "end": "to[1]"
    }
  }, {
    "from": "Card",
    "to": "Room",
    "labels": {
      "start": "authorized[*]",
      "end": "authorizations[*]"
    }
  }, {
    "from": "Card",
    "to": "Room",
    "labels": {
      "start": "checkedIn[*]",
      "end": "location[1]"
    }
  }]
}




options = {
  spacing: 8,
  smallFont: 13,
  normalFont: 15,
  layout: {    
    "Building": { y: 200 },
    "Door": { x: 500 },
    "Room": { x: 500, y: 200 },
    "Card": { x: 500, y: 400 },
    "authorizations": [["top",0.1],["bottom",0.1]],
    "location": [["top",0.6],["bottom",0.6]],
    "entries": [["bottom",0.1],["top",0.1]],
    "exits": [["bottom",0.6],["top",0.6]],
    "Building->Room": ["right", "left"],
    "Building->Card": ["right", "left"],
    "Person->Building": ["right", "left"],
    defaults: {
      block: {
        x: 0,        
        minWidth: 300
      }
    }
  }
}
