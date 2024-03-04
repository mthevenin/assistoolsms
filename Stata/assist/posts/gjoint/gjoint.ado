*** Version alpha
*** Marc thévenin - Ined Sms
*** Commande hexplot: Ben Jann


capture program drop gjoint
program define gjoint
syntax varlist(min=2 max=2) [if],             ///
                            [hopts(string)]   ///
							[PALette(string)] ///
							[title(string)]


*** installation dépendance si nécessaire	
local packg colorpalette grstyle gtools heatplot
foreach p of local packg {
capture which `p'
if _rc==111 ssc install `p'
if _rc==111 di "Installation de  `p'"
} 
							
							
							
tokenize `varlist'


*** Legende
hexplot  `varlist', color(`palette') `hopts'                ///
ylab(,nogrid) xlab(,nogrid)                                 ///
xtitle("") ytitle("") yscale(off noline) xscale(off noline) ///
nodraw                                                      ///
legend(pos(0) ring(0) subtitle("%"))                        ///
keylabels(,format(%7.3g)) 

gr_edit .plotregion1.draw_view.setstyle, style(no) 
graph save leg , replace



hexplot  `varlist',              ///
`hopts'                          ///
name(hex, replace) nodraw        ///
color(`palette')                 ///
legend(off)                      ///
fxsize(100) fysize(100)          ///
ylabel(, angle(90))              ///
p(lcolor(black) lalign(center))  



* Histogrammes
local c1= substr(`r(colors)',1,.)


histogram `1' ,  percent                                      ///    
fc("`c1'") lc(%0)                                             ///       
xtitle("%",color())                                           ///  
ytitle(,color(%0))                                            ///
ylabel(,  angle(90) labs(0) glw(0))                           ///
xlabel(, labc() glw(0))                                       ///
name(h1, replace) horizontal fxsize(50) fysize(100) nodraw


histogram `2' ,   percent                         ///
fc("`c1'") lc(%0)                                 ///
xtitle(,color(%0))                                ///
ytitle("%",color())                               ///
ylabel(,  angle(90) glw(0))                       ///
xlabel(, labc(%0) glw(0))                         ///
name(h2, replace) fxsize(100) fysize(50) nodraw


* combine
graph combine  h2 leg.gph hex h1, ///
imargin(-4 -4 -4 -4) graphr(margin(4 4 4 4)) xsize(20) ysize(20) ///
title("`title'") 

end






