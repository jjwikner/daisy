devMap := resistor rm1
addProp := model rm1
propMatch := subtype R1
termMap := PLUS PLUS MINUS MINUS

devMap := resistor rm2
addProp := model rm2
propMatch := subtype R2
termMap := PLUS PLUS MINUS MINUS

devMap := resistor rppoly
addProp := model rppoly
propMatch := subtype RP
termMap := PLUS PLUS MINUS MINUS

devMap := resistor rppolywo
addProp := model rppolywo
propMatch := subtype RW
termMap := PLUS PLUS MINUS MINUS

devMap := resistor rppolys
addProp := model rppolys
propMatch := subtype RQ
termMap := PLUS PLUS MINUS MINUS

devMap := nfet nch_hvt
propMatch := subtype NB
addProp := model nch_hvt
termMap := D D S S G G B B

devMap := pfet pch_hvt
propMatch := subtype PB
addProp := model pch_hvt
termMap := D D S S G G B B

devMap := nfet nch
propMatch := subtype NA
addProp := model nch
termMap := D D S S G G B B

devMap := pfet pch
propMatch := subtype PB
addProp := model pch
termMap := D D S S G G B B

devMap := nfet nch_33
propMatch := subtype NH
addProp := model nch_33
termMap := D D S S G G B B

devMap := pfet pch_33
propMatch := subtype PH
addProp := model pch_33
termMap := D D S S G G B B

devMap := diode ndio_33
propMatch := subtype ND
addProp := model ndio_33
termMap := PLUS PLUS MINUS MINUS

devMap := diode pdio_33
propMatch := subtype PD
addProp := model pdio_33
termMap := PLUS PLUS MINUS MINUS

devMap := diode ndio
propMatch := subtype NX
addProp := model ndio_33
termMap := PLUS PLUS MINUS MINUS

devMap := diode pdio
propMatch := subtype PX
addProp := model pdio_33
termMap := PLUS PLUS MINUS MINUS
