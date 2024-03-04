
*frame reset 

frame dir 

webuse nhanes2

frame dir 
des, s

frame rename default nhanes
frame dir 

frame put sex bpsystol, into(tension)
frame put sex bmi,      into(imc)

frame dir

* frame active (nhanes2)
tab agegrp


* frame tension
frame tension: rename bpsystol tension
frame tension: mean tension, over(sex)

* frame imc
frame imc: rename bmi imc
frame imc: mean imc, over(sex)


frame tension {
collapse tension, by(sex)
list
}

frame imc {
collapse imc, by(sex)
list
}

frame dir

frlink m:1 sex, frame(tension) gen(l1)
frlink m:1 sex, frame(imc) gen(l2)


frget mtension=tension, from(l1)
frget mimc=imc,     from(l2)



drop mtension mimc
frame tension: rename tension mean_tension
frame imc:     rename imc     mean_imc


gen diff_tens = bpsystol - frval(l1, mean_tension)
gen diff_imc =  bmi      - frval(l2, mean_imc)

mean diff_tens diff_imc, over(sex)

drop diff_tens diff_imc
gen sup_tens_imc = bpsystol> frval(l1, mean_tension) & bmi > frval(l2, mean_imc)


frame change tension
des
frame nhanes:  des, s
frame change nhanes

frame drop tension
frame dir

frame reset
frame dir

frame create new
frame dir

frame create new2 x
frame dir

frame new2 {
set obs 1000
replace x = rnormal()
label variable x "x = random normal (0,1)"
des
}

frame dir




