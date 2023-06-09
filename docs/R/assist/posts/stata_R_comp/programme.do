* Fragment programme M.Caceres
capture program drop bench
program bench
    gettoken timer call: 0,    p(:)
    gettoken colon call: call, p(:)
    cap timer clear `timer'
    timer on `timer'
    `call'
    timer off `timer'
    qui timer list
    c_local r`timer' `=r(t`timer')'
end


* changer valeur N
local N = 10000

clear 
set obs `N'
tempvar x
gen `x' = runiform()
gen g = `x'>.5

forv i=1/10 {
gen y`i' = rnormal()	
	
}
gen id=_n

* base pour fonctions R
* export delimited "D:\D\stata_temp\gtools.csv", replace


**** GQUANILES ****


qui forv i=1/10 {
tempvar yg`i'
bench 1:   xtile `yg`i'' = y`i' ,  nq(10) 
local rt1 = `rt1' + `r1' 
  }
di "XTILE runtime =" `rt1'

qui forv i=1/10 {
capt drop  `yg`i''	
tempvar yg`i'
bench 1: gquantiles `yg`i'' = y`i' , xtile  nq(10) 
local rt2 = `rt2' + `r1'     
   }
di  "GQUANTILES runtime =" `rt2'

}


* GRESHAPE

**RESHAPE
qui bench 1: reshape long y, i(id) j(j)
di "RESHAPE LONG runtime =" `r1'
qui bench 1: reshape wide y, i(id) j(j)
di "RESHAPE WIDE runtime =" `r1'

**GRESHAPE
qui bench 1: greshape long y, by(id) keys(j)
di "GRESHAPE LONG runtime =" `r1'
qui bench 1: greshape wide y, by(id) keys(j)
di "GRESHAPE WIDE runtime =" `r1'
*/


* GCOLLAPSE

preserve
qui bench 1: collapse  y1-y10,  by(g)
local col `r1'
restore

preserve
qui bench 1: gcollapse  y1-y10,  by(g)
local gcol `r1' 
restore

di "COLLAPSEruntime =" `col'
di "GCOLLAPSEruntime =" `gcol'
*/

* GEGEN

forv  i=1/10 {
qui bench 1: egen my`i' = mean(y`i'), by(g)
local egen = `egen' + `r1' 
}

drop my*

forv  i=1/10 {
qui bench 1: gegen my`i' = mean(y`i'), by(g)
local gegen = `gegen' + `r1' 
}

di "N=`N"
di "EGEN  runtime =" `egen'
di "GEGEN runtime =" `gegen'


* GLEVELSOF


clear
set obs `N'
local c2use ABCDEFGHIJKLMNPQRSTUVWXYZ
gen random_string = substr("`c2use'", runiformint(1,length("`c2use'")),1) + ///
    string(runiformint(0,9))        + ///
    char(runiformint(65,90))        + ///
    char(runiformint(65,90))        + ///
    string(runiformint(0,9))        + ///
    char(runiformint(65,90))

gen xchar = substr(random_string,1,1)
encode xchar, gen(xnum)
drop random_string
	
levelsof xchar
levelsof xnum

glevelsof xchar
glevelsof xnum



