rm(list=ls())
library(haven)
library(survey)
library(readr)
library(tidyverse)
options(scipen = 999999999)

#Add directory 
dir <- "~/Dropbox/Brazil/Brazil Github/"


                        #Get anthropometric data from external POF
#=====================================================================
#Read 24 hours recall from Brazilian Household Budget Survey (Pesquisa de Orçamentos Familiares - POF)
#This database contains self-reported anthropometric data, but consumption of beverages is not
#constructed the same as our estimates
POFA <- read.csv(paste0(dir, "Data/POF/Original/Individual_Sandra.csv"))
glimpse(POFA)
sum(POFA$PESO_FINAL)

#Create id
POFA$id <- paste0(POFA$UF, POFA$ESTRATO_POF, POFA$TIPO_SITUACAO_REG, POFA$COD_UPA, POFA$NUM_DOM, 
                  POFA$NUM_UC, POFA$COD_INFORMANTE)

#Select self-reported weight and height
POFA <- select(POFA, "id", "PESO_KG", "ALTURA_CM" )



#Load our individual beverage consumption database 
load(paste0(dir, "Data/POF/Clean/POF_SANDRA_indiv_sin_cavalinha.rda"))
length(unique(TaxedDay1S$id))
class(TaxedDay1S$id)



length(which(POFA$id %in% TaxedDay1S$id))

length(which(POFA$id == TaxedDay1S$id))


class(POFA$id)
#class(POFS$id)

#Merge POFES
MasterPOF <- merge(TaxedDay1S, POFA, by = "id")


#Save final database (already in folder)
save(MasterPOF, file = paste0(dir, "Data/POF/Clean/MasterPOF_antrosandra.rda"))


#Set design to summarise results

Design <- svydesign(id= ~id, strata= ~ESTRATO_POF, weights=~PESO_FINAL, PSU=~COD_UPA, data= MasterPOF)

options(survey.lonely.psu = "adjust")


svymean(~ssb_kcal, Design, na.rm = TRUE)
mean(TaxedDay1$ssb_kcal, na.rm = TRUE)
#svyquantile(~taxed_kcal, Design, quantiles = c(0.25, 0.5, 0.75), na.rm =TRUE)
svymean(~ssb_qtd_gramas, Design, na.rm =TRUE)
mean(TaxedDay1$ssb_qtd_gramas, na.rm =TRUE)
#svyquantile(~taxed_qtd_gramas, Design, quantiles = c(0.25, 0.5, 0.75), na.rm =TRUE)

svymean(~ssb2_kcal, Design, na.rm = TRUE)
mean(TaxedDay1S$ssb2_kcal, na.rm = TRUE)
#svyquantile(~taxed_kcal, Design, quantiles = c(0.25, 0.5, 0.75), na.rm =TRUE)
svymean(~ssb2_qtd_gramas, Design, na.rm =TRUE)
mean(TaxedDay1S$ssb2_qtd_gramas, na.rm =TRUE)


svymean(~alcohol_kcal, Design, na.rm = TRUE)
mean(TaxedDay1S$alcohol_kcal, na.rm = TRUE)
#svyquantile(~taxed_kcal, Design, quantiles = c(0.25, 0.5, 0.75), na.rm =TRUE)
svymean(~alcohol_qtd_gramas, Design, na.rm =TRUE)
mean(TaxedDay1S$alcohol_qtd_gramas, na.rm =TRUE)


svymean(~otherbev_kcal, Design, na.rm = TRUE)
mean(TaxedDay1S$otherbev_kcal, na.rm = TRUE)
#svyquantile(~taxed_kcal, Design, quantiles = c(0.25, 0.5, 0.75), na.rm =TRUE)
svymean(~otherbev_qtd_gramas, Design, na.rm =TRUE)
mean(TaxedDay1S$otherbev_qtd_gramas, na.rm =TRUE)


svymean(~diet_kcal, Design, na.rm = TRUE)
mean(TaxedDay1S$diet_kcal, na.rm = TRUE)
#svyquantile(~taxed_kcal, Design, quantiles = c(0.25, 0.5, 0.75), na.rm =TRUE)
svymean(~diet_qtd_gramas, Design, na.rm =TRUE)
mean(TaxedDay1S$diet_qtd_gramas, na.rm =TRUE)

svymean(~potable_qtd_gramas, Design, na.rm = TRUE)

length(unique(TaxedDay1$id))



