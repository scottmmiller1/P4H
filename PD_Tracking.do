
******************
**	 PD Meta	** 
******************

cd "/Users/scottmiller/Desktop/P4H Global/Evaluation/PD/PD_Tacking/All"

import delimited "PD_Y1 Exam Data Full.csv", clear 

drop if complete=="i"
drop complete


forvalues i=6/21 {
	replace v`i'="1" if v`i'=="v" | v`i'=="a" | v`i'=="bon"
	replace v`i'="2" if v`i'=="f" | v`i'=="b" | v`i'=="pa bon"
	replace v`i'="3" if v`i'=="c"
	replace v`i'="4" if v`i'=="d"
	replace v`i'="5" if v`i'=="e"
	replace v`i'="" if v`i'=="." | v`i'==" " | v`i'=="vf" | v`i'=="aq" | v`i'=="cd" | v`i'=="dc"
	destring v`i', replace
}

rename ïno i
replace pre_post = "Post" if pre_post == "post"
replace pre_post = "Post" if pre_post == "Poat"
replace first = "Against" if first == "Agains"

reshape wide v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21, i(i) j(pre_post) string

drop first last

gen nschool = 1 if school == "Harvy"
replace nschool = 2 if school == "Mission of Hope"
replace nschool = 3 if school == "Ecole St Marc"
replace nschool = 4 if school == "CEFCAP"
replace nschool = 5 if school == "HOH"
replace nschool = 6 if school == "RTS"
replace nschool = 7 if school == "JS"
replace nschool = 8 if school == "TRESOR"
replace nschool = 9 if school == "JOUISSANT"
replace nschool = 10 if school == "PD Dondon"
replace nschool = 11 if school == "OEDP"
replace nschool = 12 if school == "CFC - K"
replace nschool = 13 if school == "Sonje"
replace nschool = 14 if school == "LABY"
replace nschool = 15 if school == "CFC - G"


* correct answers

foreach v of varlist v10* v12* v14* {
	replace `v' = 0 if `v' != 1
}
foreach v of varlist v6* v7* v8* v9* v11* v13* {
	replace `v' = cond(`v' == 2, 1, 0) 
}
foreach v of varlist v18* v19* {
	replace `v' = cond(`v' == 3, 1, 0) 
}
foreach v of varlist v15* v16* v17* v20* {
	replace `v' = cond(`v' == 4, 1, 0) 
}
foreach v of varlist v21* {
	replace `v' = cond(`v' == 3, 1, 0) if nschool < 6 
	replace `v' = cond(`v' == 5, 1, 0) if nschool >= 6
}

egen pre_avg = rmean(v6Pre v7Pre v8Pre v9Pre v10Pre v11Pre v12Pre v13Pre v14Pre v15Pre v16Pre v17Pre v18Pre v19Pre v20Pre v21Pre)
egen post_avg = rmean(v6Post v7Post v8Post v9Post v10Post v11Post v12Post v13Post v14Post v15Post v16Post v17Post v18Post v19Post v20Post v21Post)
gen diff = post_avg - pre_avg

collapse (mean) v* (firstnm) nschool, by(school)

export delimited using "PD_All_Collapse.csv", replace



** Evals
import delimited "PD_Y1 Evals Full.csv", clear 

drop ïobs


*destring v23, replace

sum v23, d
replace v23 = `r(p95)' if v23 > `r(p95)'

rename a v3
rename b v4

collapse (mean) v*, by(school)

gen nschool = 1 if school == "Harvey"
replace nschool = 2 if school == "MOH"
replace nschool = 3 if school == "ESM"
replace nschool = 4 if school == "CEFCAP"
replace nschool = 5 if school == "HOH"
replace nschool = 6 if school == "RTS"
replace nschool = 7 if school == "Joseph"
replace nschool = 8 if school == "TRESOR"
replace nschool = 9 if school == "Jouissant"
replace nschool = 10 if school == "PD Dondon"
replace nschool = 11 if school == "OEDP"
replace nschool = 12 if school == "CFC-K"
replace nschool = 13 if school == "Sonje"
replace nschool = 14 if school == "Laby"
replace nschool = 15 if school == "CFC - G"

replace v18 =. if school == "CFC-K"

export delimited using "Eval_All_Collapse.csv", replace








