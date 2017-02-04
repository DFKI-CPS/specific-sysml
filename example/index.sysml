project [acs]

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
fsl1 <|- fsl2
  acs <|- acs
    Person <|- Person
      pass <|- b.enter(self)
fsl2 <|- fsl3
  acs <|- acs
    Building <|- Building
      enter <|- org_dom->select(d|d.org = p.sit).enter(p)
fsl3 <|- fsl4
fsl4 <|- fsl5
fsl5 <|- fsl6

satisfy isl::acs::P1
  <- fsl0::acs::Building
  <- fsl0::acs::Person