
**Ver resultados con la base que subió Ross 
import delimited "/Users/mac/Library/CloudStorage/Dropbox/INSP 2020/Brazil/Brazil Bloomberg/Model simulations/Results/finalsimbodyweight_10years_adjusted.csv", clear

rename *, lower
svyset [pweight =peso_final], strata(estrato_pof) psu(cod_upa) singleunit(centered)


*drop if idade_anos<20
svy: mean idade


egen agecat=cut(idade), at (20 (20) 60, 120)

cd "/Users/mac/Library/CloudStorage/Dropbox/INSP 2020/Brazil/Paper/Tabout
 
 
gen ses="1" if nse=="BAJO" 
replace ses="2" if nse=="MEDIO" 
replace ses="3" if nse=="ALTO" 
destring ses, replace
label define ses 1 "Low" 2 "Middle" 3 "High" 
label values ses ses


***Expancion 
tabout sex agecat ses using frec_expand.txt, svy c(freq) pop replac


****Reduction in calories
forva x=20(10)30{
tabout sex agecat ses using ssb2_kcal_red_nse_`x'.xls, c(mean ssb2_kcal_red_nse_`x'  ci) f(1 1) clab(Mean_caloric reduction CI) sum svy npos(lab) oneway replace
tabout sex agecat ses using allbev_kcal_red_nse_`x'.xls, c(mean allbev_kcal_red_nse_`x'  ci) f(1 1) clab(Mean_caloric reduction CI) sum svy npos(lab) oneway replace
}

***Weight at baseline 
tabout sex agecat ses using peso_baseline.xls, c(mean peso_ajustado  ci) f(2 2) clab(Mean_peso CI) sum svy npos(lab) oneway replace


***Change in weight for the 2 scenarios (only SSBs or all beverages)
forva x=20(10)30{
gen reduc_pesoSSB`x'=peso_ssb2_kcal_red_nse_`x'_10year-peso_ajustado
}


forva x=20(10)30{
gen reduc_pesoall`x'=peso_allbev_kcal_red_nse_`x'_10ye-peso_ajustado
}



****Reduction in weight
forva x=20(10)30{
tabout sex agecat ses using reduc_pesoSSB_`x'.xls, c(mean reduc_pesoSSB`x' ci) f(2 2) clab(Mean_reduction CI) sum svy npos(lab) oneway replace
tabout sex agecat ses using reduc_pesoall_`x'.xls, c(mean reduc_pesoall`x' ci) f(2 2) clab(Mean_reduction CI) sum svy npos(lab) oneway replace
}



****Tables exported about weight reduction
forva x=20(10)30{
tabout sex agecat using reduc_pesoSSB`x'.xls, c(mean reduc_pesoSSB`x'  ci) f(2 2) clab(Mean_reduction CI) sum svy npos(lab) oneway replace
tabout sex agecat using reduc_pesoall`x'.xls, c(mean reduc_pesoall`x'  ci) f(2 2) clab(Mean_reduction CI) sum svy npos(lab) oneway replace
}



****Obesity after the interventionn****
forva x=20(10)30{
gen obes_ssb2_`x'=0
replace obes_ssb2_`x'=1 if bmi_ssb2_kcal_red_nse_`x'_10yea>=30
gen obes_all_`x'=0
replace obes_all_`x'=1 if bmi_allbev_kcal_red_nse_`x'_10yea>=30
}


***BMI at baseline
gen bmi_ajustado=peso_ajustado/(altura_ajustada/100)^2
***Obesity at baseline
gen obesidad=0
replace obesidad=1 if bmi_ajustado>=30


tabout sex agecat using obesidad.xls, c(mean obesidad  ci) f(3 3) clab(Mean_reduction CI) sum svy npos(lab) oneway replace


*Reduction in obesity
forva x=20(10)30{
gen reduc_obseSSB_`x'=obes_ssb2_`x'-obesidad
gen reduc_obesall_`x'=obes_all_`x'-obesidad
}


forva x=20(10)30{
tabout sex agecat using reduc_obseSSB_`x'.xls, c(mean reduc_obseSSB_`x'  ci) f(3 3) clab(Mean_reduction CI) sum svy npos(lab) oneway replace
tabout sex agecat using reduc_obesall_`x'.xls, c(mean reduc_obesall_`x'  ci) f(3 3) clab(Mean_reduction CI) sum svy npos(lab) oneway replace
}

