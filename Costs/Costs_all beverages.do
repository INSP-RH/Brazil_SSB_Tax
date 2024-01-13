
import delimited  "/Users/insp/Dropbox/INSP 2020/Brazil/Brazil Bloomberg/Model simulations/Results/finalsimbodyweight_10years.csv", encoding(ISO-8859-1)clear
svyset [pweight =peso_final], strata(estrato_pof) psu(cod_upa) singleunit(centered)


drop if idade<20
svy: mean idade



gen sexo="1" if sex=="male" 
replace sexo="2" if sex=="female" 
destring sexo, replace

label define sexo 1 "Male" 2 "Female" 
label values sexo sexo


gen bmi_ajustado=peso_ajustado/(altura_ajustada/100)^2
***hacer las variables de reducción de obesidad
gen obesidad=0
replace obesidad=1 if bmi_ajustado>=30

****Para cada año de hall****
***Para el escenario de 30 % de impuesto***
forv i=1(1)9{
gen obes_allbev_30_`i'years=0
replace obes_allbev_30_`i'years=1 if bmi_allbev_kcal_red_nse_30_`i'year>=30
}


gen obes_allbev_30_10years=0
replace obes_allbev_30_10years=1 if bmi_allbev_kcal_red_nse_30_10yea>=30


*reducciones de obesidad 
forv i=1(1)10{
gen reduc_obseallbev_30_`i'y=(obes_allbev_30_`i'years-obesidad)*100
}

*reducciones de obesidad 
forv i=1(1)10{
svy: mean reduc_obseallbev_30_`i'y
}


****Para cada año de hall****
***Para el escenario de 20 % de impuesto***
forv i=1(1)9{
gen obes_allbev_20_`i'years=0
replace obes_allbev_20_`i'years=1 if bmi_allbev_kcal_red_nse_20_`i'year>=30
}


gen obes_allbev_20_10years=0
replace obes_allbev_20_10years=1 if bmi_allbev_kcal_red_nse_20_10yea>=30


*reducciones de obesidad 
forv i=1(1)10{
gen reduc_obseallbev_20_`i'y=(obes_allbev_20_`i'years-obesidad)*100
}

*reducciones de obesidad 
forv i=1(1)10{
svy: mean reduc_obseallbev_20_`i'y
}
