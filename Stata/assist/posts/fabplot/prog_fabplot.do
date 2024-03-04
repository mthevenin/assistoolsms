
* ssc install fabplot
* net install fabplot2, from("https://mthevenin.github.io/stata_graphiques/ressources/fabplot/") replace


webuse set https://github.com/mthevenin/stata_graphiques/tree/main/ressources/fabplot
use babynames, replace
webuse set

* courbes empilées

grstyle init 
grstyle set mesh
grstyle set color tableau, n(9)

levelsof name, local(name)
local i = 1
foreach nom of local name  {
local j = `i++'	
local line `line' line n year if name=="`nom'" ||		
local leg `leg'  `j' "`nom'"
}

tw `line' , legend(order(`leg') row(2) size(*.8)  region(color(%0)) pos(11)) ytitle("") ylabel(0(20000)100000, angle(0)) ///
title("Popularité des prénoms")


* fabplot

#delimit ;

fabplot2 line n year, 
by(name, title("Popularité des prénoms", pos(11))) 

frontopts(lw(*2) lc("45 178 125")) 
backopts(lw(*.5) lc(gs9)) 

ytitle("")  ylabel(0(20000)100000, labsize(*.8) glw(*.5))
; 

#delimit cr

