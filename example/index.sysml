project [ACS]
-----------------------------------------------------------

include
  01-isl.sysml
  02-fsl0.sysml
  03-fsl1.sysml
  04-fsl2.sysml
  05-fsl3.sysml
  06-fsl4.sysml
  07-fsl5.sysml
  08-fsl6.sysml

fsl0 <|- fsl1
  acs <|- acs
    Person <|- Person
      authorized <|- aut
      current <|- sit

fsl1 <|- fsl2
  acs <|- acs
    Person <|- Person
      pass <|- b.enter(self)
        self <|- p
        b <|- self

fsl2 <|- fsl3
  acs <|- acs
    Building <|- Building
      enter <|- org_dom->select(d|d.org = p.sit).enter(p)
        self <|- org

fsl3 <|- fsl4

fsl4 <|- fsl5

fsl5 <|- fsl6
  acs <|- acs
    Door <|- Door
      accept <|- accept()
        p <|- self.dap
      refuse <|- refuse()
        p <|- self.dap

satisfy isl::acs::P1
  <- fsl0::acs::Building
  <- fsl0::acs::Person
satisfy isl::acs::P2
  <- fsl0::acs::Person::authorized
satisfy isl::acs::P3 <- fsl0::acs::Person::current
satisfy isl::acs::P4 <- fsl0::acs::Person::current
satisfy isl::acs::P5 <- fsl0::acs::Person::current

satisfy isl::acs::P6
  <- fsl1::acs::Building::gate
  <- fsl1::acs::Building::building
satisfy isl::acs::P7
  <- fsl1::acs::Building::P7
satisfy isl::acs::P8
  <- fsl1::acs::Person::pass::P8
satisfy isl::acs::P9
  <- isl::acs::P10
satisfy isl::acs::P10
  <- fsl1::acs::Person::P10

satisfy isl::acs::P11
  <- fsl4::acs::Door
satisfy isl::acs::P12
  <- fsl4::acs::Door::dap
  <- fsl4::acs::Person::dap_dom
  <- fsl4::acs::Person::P12
satisfy isl::acs::P13
  <- fsl4::acs::Door::accept
  <- fsl4::acs::Person::P13a
  <- fsl4::acs::Person::P13b
satisfy isl::acs::P14
  <- fsl4::acs::Door::green
  <- fsl4::acs::Door::pass_thru
  <- fsl4::acs::Door::off_grn
satisfy isl::acs::P15
  <- fsl5::acs::Door::off_red
satisfy isl::acs::P16
  <- fsl4::acs::Door::P16
satisfy isl::acs::P17
  <- fsl4::acs::Person::admitted