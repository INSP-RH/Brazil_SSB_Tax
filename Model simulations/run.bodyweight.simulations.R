rm(list=ls())
library(bw)
library(survey)
library(tidyverse)
library(janitor)


#Add directory 
dir <- "~/Dropbox/Brazil/Brazil Github/"

#           Read final POF database with individual beverage consumption and
#                  adjusted anthropometric data (constructed by us)
#==============================================================================================
load(paste0(dir, "Data/Final database/POF_SANDRA_indiv_antrosandra_adjusted.rda"))


#         *****************************************************************************
#         * Since the price elasticities for beverages are estimated by income level *
#         * we need to construct an income level variable and add it to our database *
#         *****************************************************************************


#Read 24 hours recall from Brazilian Household Budget Survey (Pesquisa de Orçamentos Familiares - POF)
#This database contains socioeconomic data, but consumption of beverages is not
#constructed the same as our estimates

#==============================================================================================
POFA <- read.csv(paste0(dir, "Data/POF/Original/Individual_Sandra.csv"))

glimpse(POFA)
sum(POFA$PESO_FINAL)

POFA$id <- paste0(POFA$UF, POFA$ESTRATO_POF, POFA$TIPO_SITUACAO_REG, POFA$COD_UPA, POFA$NUM_DOM, 
                  POFA$NUM_UC, POFA$COD_INFORMANTE)
#Educational level
POFA$nivel_edu <- rep(NA, nrow(POFA))
POFA$nivel_edu[POFA$ANOS_ESTUDO < 5] <- "BAJO"
POFA$nivel_edu[POFA$ANOS_ESTUDO >= 5 & POFA$ANOS_ESTUDO <= 12] <- "MEDIO"
POFA$nivel_edu[POFA$ANOS_ESTUDO > 12] <- "ALTO"

#Get income tertiles
DesignPOFA <- svydesign(id= ~id, strata= ~ESTRATO_POF, weights=~PESO_FINAL, PSU=~COD_UPA, data= POFA)
options(survey.lonely.psu = "adjust")

#Same 
svyquantile(~RENDA_TOTAL, DesignPOFA, c(1/3, 2/3, 1))

#Use income tertiles to create discrete income level variable
POFA$nse <- rep(NA, nrow(POFA))
POFA$nse[POFA$RENDA_TOTAL < 2570.50] <- "BAJO"
POFA$nse[POFA$RENDA_TOTAL >= 2570.50 & POFA$RENDA_TOTAL < 5125.52] <- "MEDIO"
POFA$nse[POFA$RENDA_TOTAL >= 5125.52] <- "ALTO"

#Select new socioeconomic variables
POFA <- dplyr::select(POFA, id, nivel_edu, nse)


#Merge our constructed POF with POF containing socioeconomic variables
POF <- merge(POF_adjusted, POFA, by = "id", all.x = TRUE)

POF$sexo <- POF$SEXO
POF$idade <- POF$IDADE_ANOS

glimpse(POF)
table(POF$sexo)

#                                   Overall consumption change 
#==============================================================================================

#Elasticities
ssb_elast <- -1.19177
otherbev_elast <-  0.04628
alcohol_elast <-  -0.10373
diet_elast <-   0.08879


#                         ***********************************************
#                         *               ASSUMPTIONS:                  *
#                         * 1. Change in purchases = change in calories *
#                         * 2. Tax effect is linear                     *
#                         ***********************************************


#We'll use the ssb group wich does not include diet beverages

POF$ssb2_kcal_red_20 <- POF$ssb2_kcal*(2*ssb_elast/10)
POF$ssb2_kcal_red_30 <- POF$ssb2_kcal*(3*ssb_elast/10)



POF$ssb2_ml_red_20 <- POF$ssb2_qtd_gramas*(2*ssb_elast/10)
POF$ssb2_ml_red_30 <- POF$ssb2_qtd_gramas*(3*ssb_elast/10)



POF$otherbev_kcal_red_20 <- POF$otherbev_kcal*(2*otherbev_elast/10)
POF$otherbev_kcal_red_30 <- POF$otherbev_kcal*(3*otherbev_elast/10)



POF$otherbev_ml_red_20 <- POF$otherbev_qtd_gramas*(2*otherbev_elast/10)
POF$otherbev_ml_red_30 <- POF$otherbev_qtd_gramas*(3*otherbev_elast/10)



POF$alcohol_kcal_red_20 <- POF$alcohol_kcal*(2*alcohol_elast/10)
POF$alcohol_kcal_red_30 <- POF$alcohol_kcal*(3*alcohol_elast/10)


POF$alcohol_ml_red_20 <- POF$alcohol_qtd_gramas*(2*alcohol_elast/10)
POF$alcohol_ml_red_30 <- POF$alcohol_qtd_gramas*(3*alcohol_elast/10)



POF$diet_kcal_red_20 <- POF$diet_kcal*(2*diet_elast/10)
POF$diet_kcal_red_30 <- POF$diet_kcal*(3*diet_elast/10)



POF$diet_ml_red_20 <- POF$diet_qtd_gramas*(2*diet_elast/10)
POF$diet_ml_red_30 <- POF$diet_qtd_gramas*(3*diet_elast/10)



#                          Consumption change by educational level
#===============================================================================

# ssb_elast_nivel_edu_low <- -1.26844
# ssb_elast_nivel_edu_medium <- -1.17775
# ssb_elast_nivel_edu_high <- -1.16865
# 
# otherbev_elast_nivel_edu_low <- 0.02048
# otherbev_elast_nivel_edu_medium <-0.03831
# otherbev_elast_nivel_edu_high <- 0.10456
# 
# alcohol_elast_nivel_edu_low <- -0.13446
# alcohol_elast_nivel_edu_medium <- -0.09653
# alcohol_elast_nivel_edu_high <- -0.10537
# 
# diet_elast_nivel_edu_low <- 0.04181
# diet_elast_nivel_edu_medium <- 0.07145
# diet_elast_nivel_edu_high <- 0.11999
# 
# 
# 
# 
# POF$ssb2_kcal_red_nivel_edu_20 <- rep(NA, nrow(POF))
# POF$ssb2_kcal_red_nivel_edu_20[POF$nivel_edu == "BAJO"] <- POF$ssb2_kcal[POF$nivel_edu == "BAJO"]*(2*ssb_elast_nivel_edu_low/10)
# POF$ssb2_kcal_red_nivel_edu_20[POF$nivel_edu == "MEDIO"] <- POF$ssb2_kcal[POF$nivel_edu == "MEDIO"]*(2*ssb_elast_nivel_edu_medium/10)
# POF$ssb2_kcal_red_nivel_edu_20[POF$nivel_edu == "ALTO"] <- POF$ssb2_kcal[POF$nivel_edu == "ALTO"]*(2*ssb_elast_nivel_edu_high/10)
# 
# POF$ssb2_ml_red_nivel_edu_20 <- rep(NA, nrow(POF))
# POF$ssb2_ml_red_nivel_edu_20[POF$nivel_edu == "BAJO"] <- POF$ssb2_qtd_gramas[POF$nivel_edu == "BAJO"]*(2*ssb_elast_nivel_edu_low/10)
# POF$ssb2_ml_red_nivel_edu_20[POF$nivel_edu == "MEDIO"] <- POF$ssb2_qtd_gramas[POF$nivel_edu == "MEDIO"]*(2*ssb_elast_nivel_edu_medium/10)
# POF$ssb2_ml_red_nivel_edu_20[POF$nivel_edu == "ALTO"] <- POF$ssb2_qtd_gramas[POF$nivel_edu == "ALTO"]*(2*ssb_elast_nivel_edu_high/10)
# 
# 
# 
# POF$ssb2_kcal_red_nivel_edu_30 <- rep(NA, nrow(POF))
# POF$ssb2_kcal_red_nivel_edu_30[POF$nivel_edu == "BAJO"] <- POF$ssb2_kcal[POF$nivel_edu == "BAJO"]*(3*ssb_elast_nivel_edu_low/10)
# POF$ssb2_kcal_red_nivel_edu_30[POF$nivel_edu == "MEDIO"] <- POF$ssb2_kcal[POF$nivel_edu == "MEDIO"]*(3*ssb_elast_nivel_edu_medium/10)
# POF$ssb2_kcal_red_nivel_edu_30[POF$nivel_edu == "ALTO"] <- POF$ssb2_kcal[POF$nivel_edu == "ALTO"]*(3*ssb_elast_nivel_edu_high/10)
# 
# 
# POF$ssb2_ml_red_nivel_edu_30 <- rep(NA, nrow(POF))
# POF$ssb2_ml_red_nivel_edu_30[POF$nivel_edu == "BAJO"] <- POF$ssb2_qtd_gramas[POF$nivel_edu == "BAJO"]*(3*ssb_elast_nivel_edu_low/10)
# POF$ssb2_ml_red_nivel_edu_30[POF$nivel_edu == "MEDIO"] <- POF$ssb2_qtd_gramas[POF$nivel_edu == "MEDIO"]*(3*ssb_elast_nivel_edu_medium/10)
# POF$ssb2_ml_red_nivel_edu_30[POF$nivel_edu == "ALTO"] <- POF$ssb2_qtd_gramas[POF$nivel_edu == "ALTO"]*(3*ssb_elast_nivel_edu_high/10)
# 
# 
# 
# POF$otherbev_kcal_red_nivel_edu_20 <- rep(NA, nrow(POF))
# POF$otherbev_kcal_red_nivel_edu_20[POF$nivel_edu == "BAJO"] <- POF$otherbev_kcal[POF$nivel_edu == "BAJO"]*(2*otherbev_elast_nivel_edu_low/10)
# POF$otherbev_kcal_red_nivel_edu_20[POF$nivel_edu == "MEDIO"] <- POF$otherbev_kcal[POF$nivel_edu == "MEDIO"]*(2*otherbev_elast_nivel_edu_medium/10)
# POF$otherbev_kcal_red_nivel_edu_20[POF$nivel_edu == "ALTO"] <- POF$otherbev_kcal[POF$nivel_edu == "ALTO"]*(2*otherbev_elast_nivel_edu_high/10)
# 
# 
# POF$otherbev_ml_red_nivel_edu_20 <- rep(NA, nrow(POF))
# POF$otherbev_ml_red_nivel_edu_20[POF$nivel_edu == "BAJO"] <- POF$otherbev_qtd_gramas[POF$nivel_edu == "BAJO"]*(2*otherbev_elast_nivel_edu_low/10)
# POF$otherbev_ml_red_nivel_edu_20[POF$nivel_edu == "MEDIO"] <- POF$otherbev_qtd_gramas[POF$nivel_edu == "MEDIO"]*(2*otherbev_elast_nivel_edu_medium/10)
# POF$otherbev_ml_red_nivel_edu_20[POF$nivel_edu == "ALTO"] <- POF$otherbev_qtd_gramas[POF$nivel_edu == "ALTO"]*(2*otherbev_elast_nivel_edu_high/10)
# 
# 
# 
# POF$otherbev_kcal_red_nivel_edu_30 <- rep(NA, nrow(POF))
# POF$otherbev_kcal_red_nivel_edu_30[POF$nivel_edu == "BAJO"] <- POF$otherbev_kcal[POF$nivel_edu == "BAJO"]*(3*otherbev_elast_nivel_edu_low/10)
# POF$otherbev_kcal_red_nivel_edu_30[POF$nivel_edu == "MEDIO"] <- POF$otherbev_kcal[POF$nivel_edu == "MEDIO"]*(3*otherbev_elast_nivel_edu_medium/10)
# POF$otherbev_kcal_red_nivel_edu_30[POF$nivel_edu == "ALTO"] <- POF$otherbev_kcal[POF$nivel_edu == "ALTO"]*(3*otherbev_elast_nivel_edu_high/10)
# 
# 
# POF$otherbev_ml_red_nivel_edu_30 <- rep(NA, nrow(POF))
# POF$otherbev_ml_red_nivel_edu_30[POF$nivel_edu == "BAJO"] <- POF$otherbev_qtd_gramas[POF$nivel_edu == "BAJO"]*(3*otherbev_elast_nivel_edu_low/10)
# POF$otherbev_ml_red_nivel_edu_30[POF$nivel_edu == "MEDIO"] <- POF$otherbev_qtd_gramas[POF$nivel_edu == "MEDIO"]*(3*otherbev_elast_nivel_edu_medium/10)
# POF$otherbev_ml_red_nivel_edu_30[POF$nivel_edu == "ALTO"] <- POF$otherbev_qtd_gramas[POF$nivel_edu == "ALTO"]*(3*otherbev_elast_nivel_edu_high/10)
# 
# 
# 
# POF$alcohol_kcal_red_nivel_edu_20 <- rep(NA, nrow(POF))
# POF$alcohol_kcal_red_nivel_edu_20[POF$nivel_edu == "BAJO"] <- POF$alcohol_kcal[POF$nivel_edu == "BAJO"]*(2*alcohol_elast_nivel_edu_low/10)
# POF$alcohol_kcal_red_nivel_edu_20[POF$nivel_edu == "MEDIO"] <- POF$alcohol_kcal[POF$nivel_edu == "MEDIO"]*(2*alcohol_elast_nivel_edu_medium/10)
# POF$alcohol_kcal_red_nivel_edu_20[POF$nivel_edu == "ALTO"] <- POF$alcohol_kcal[POF$nivel_edu == "ALTO"]*(2*alcohol_elast_nivel_edu_high/10)
# 
# 
# POF$alcohol_ml_red_nivel_edu_20 <- rep(NA, nrow(POF))
# POF$alcohol_ml_red_nivel_edu_20[POF$nivel_edu == "BAJO"] <- POF$alcohol_qtd_gramas[POF$nivel_edu == "BAJO"]*(2*alcohol_elast_nivel_edu_low/10)
# POF$alcohol_ml_red_nivel_edu_20[POF$nivel_edu == "MEDIO"] <- POF$alcohol_qtd_gramas[POF$nivel_edu == "MEDIO"]*(2*alcohol_elast_nivel_edu_medium/10)
# POF$alcohol_ml_red_nivel_edu_20[POF$nivel_edu == "ALTO"] <- POF$alcohol_qtd_gramas[POF$nivel_edu == "ALTO"]*(2*alcohol_elast_nivel_edu_high/10)
# 
# 
# 
# POF$alcohol_kcal_red_nivel_edu_30 <- rep(NA, nrow(POF))
# POF$alcohol_kcal_red_nivel_edu_30[POF$nivel_edu == "BAJO"] <- POF$alcohol_kcal[POF$nivel_edu == "BAJO"]*(3*alcohol_elast_nivel_edu_low/10)
# POF$alcohol_kcal_red_nivel_edu_30[POF$nivel_edu == "MEDIO"] <- POF$alcohol_kcal[POF$nivel_edu == "MEDIO"]*(3*alcohol_elast_nivel_edu_medium/10)
# POF$alcohol_kcal_red_nivel_edu_30[POF$nivel_edu == "ALTO"] <- POF$alcohol_kcal[POF$nivel_edu == "ALTO"]*(3*alcohol_elast_nivel_edu_high/10)
# 
# 
# POF$alcohol_ml_red_nivel_edu_30 <- rep(NA, nrow(POF))
# POF$alcohol_ml_red_nivel_edu_30[POF$nivel_edu == "BAJO"] <- POF$alcohol_qtd_gramas[POF$nivel_edu == "BAJO"]*(3*alcohol_elast_nivel_edu_low/10)
# POF$alcohol_ml_red_nivel_edu_30[POF$nivel_edu == "MEDIO"] <- POF$alcohol_qtd_gramas[POF$nivel_edu == "MEDIO"]*(3*alcohol_elast_nivel_edu_medium/10)
# POF$alcohol_ml_red_nivel_edu_30[POF$nivel_edu == "ALTO"] <- POF$alcohol_qtd_gramas[POF$nivel_edu == "ALTO"]*(3*alcohol_elast_nivel_edu_high/10)
# 
# 
# 
# POF$diet_kcal_red_nivel_edu_20 <-rep(NA, nrow(POF))
# POF$diet_kcal_red_nivel_edu_20[POF$nivel_edu == "BAJO"] <- POF$diet_kcal[POF$nivel_edu == "BAJO"]*(2*diet_elast_nivel_edu_low/10)
# POF$diet_kcal_red_nivel_edu_20[POF$nivel_edu == "MEDIO"] <- POF$diet_kcal[POF$nivel_edu == "MEDIO"]*(2*diet_elast_nivel_edu_medium/10)
# POF$diet_kcal_red_nivel_edu_20[POF$nivel_edu == "ALTO"] <- POF$diet_kcal[POF$nivel_edu == "ALTO"]*(2*diet_elast_nivel_edu_high/10)
# 
# 
# POF$diet_ml_red_nivel_edu_20 <-rep(NA, nrow(POF))
# POF$diet_ml_red_nivel_edu_20[POF$nivel_edu == "BAJO"] <- POF$diet_qtd_gramas[POF$nivel_edu == "BAJO"]*(2*diet_elast_nivel_edu_low/10)
# POF$diet_ml_red_nivel_edu_20[POF$nivel_edu == "MEDIO"] <- POF$diet_qtd_gramas[POF$nivel_edu == "MEDIO"]*(2*diet_elast_nivel_edu_medium/10)
# POF$diet_ml_red_nivel_edu_20[POF$nivel_edu == "ALTO"] <- POF$diet_qtd_gramas[POF$nivel_edu == "ALTO"]*(2*diet_elast_nivel_edu_high/10)
# 
# 
# 
# POF$diet_kcal_red_nivel_edu_30 <-rep(NA, nrow(POF))
# POF$diet_kcal_red_nivel_edu_30[POF$nivel_edu == "BAJO"] <- POF$diet_kcal[POF$nivel_edu == "BAJO"]*(3*diet_elast_nivel_edu_low/10)
# POF$diet_kcal_red_nivel_edu_30[POF$nivel_edu == "MEDIO"] <- POF$diet_kcal[POF$nivel_edu == "MEDIO"]*(3*diet_elast_nivel_edu_medium/10)
# POF$diet_kcal_red_nivel_edu_30[POF$nivel_edu == "ALTO"] <- POF$diet_kcal[POF$nivel_edu == "ALTO"]*(3*diet_elast_nivel_edu_high/10)
# 
# 
# POF$diet_ml_red_nivel_edu_30 <-rep(NA, nrow(POF))
# POF$diet_ml_red_nivel_edu_30[POF$nivel_edu == "BAJO"] <- POF$diet_qtd_gramas[POF$nivel_edu == "BAJO"]*(3*diet_elast_nivel_edu_low/10)
# POF$diet_ml_red_nivel_edu_30[POF$nivel_edu == "MEDIO"] <- POF$diet_qtd_gramas[POF$nivel_edu == "MEDIO"]*(3*diet_elast_nivel_edu_medium/10)
# POF$diet_ml_red_nivel_edu_30[POF$nivel_edu == "ALTO"] <- POF$diet_qtd_gramas[POF$nivel_edu == "ALTO"]*(3*diet_elast_nivel_edu_high/10)



#                           Consumption change by income level
#===============================================================================

ssb_elast_nse_low <- -1.24112
ssb_elast_nse_medium <- -1.18595
ssb_elast_nse_high <- -1.12575

otherbev_elast_nse_low <- 0.04571
otherbev_elast_nse_medium <- 0.02961
otherbev_elast_nse_high <- 0.06639

alcohol_elast_nse_low <- -0.12249
alcohol_elast_nse_medium <- -0.07616
alcohol_elast_nse_high <- -0.14054

diet_elast_nse_low <- -0.17438
diet_elast_nse_medium <- 0.09368
diet_elast_nse_high <- 0.20113




POF$ssb2_kcal_red_nse_20 <- rep(NA, nrow(POF))
POF$ssb2_kcal_red_nse_20[POF$nse == "BAJO"] <- POF$ssb2_kcal[POF$nse == "BAJO"]*(2*ssb_elast_nse_low/10)
POF$ssb2_kcal_red_nse_20[POF$nse == "MEDIO"] <- POF$ssb2_kcal[POF$nse == "MEDIO"]*(2*ssb_elast_nse_medium/10)
POF$ssb2_kcal_red_nse_20[POF$nse == "ALTO"] <- POF$ssb2_kcal[POF$nse == "ALTO"]*(2*ssb_elast_nse_high/10)

POF$ssb2_ml_red_nse_20 <- rep(NA, nrow(POF))
POF$ssb2_ml_red_nse_20[POF$nse == "BAJO"] <- POF$ssb2_qtd_gramas[POF$nse == "BAJO"]*(2*ssb_elast_nse_low/10)
POF$ssb2_ml_red_nse_20[POF$nse == "MEDIO"] <- POF$ssb2_qtd_gramas[POF$nse == "MEDIO"]*(2*ssb_elast_nse_medium/10)
POF$ssb2_ml_red_nse_20[POF$nse == "ALTO"] <- POF$ssb2_qtd_gramas[POF$nse == "ALTO"]*(2*ssb_elast_nse_high/10)


POF$ssb2_kcal_red_nse_30 <- rep(NA, nrow(POF))
POF$ssb2_kcal_red_nse_30[POF$nse == "BAJO"] <- POF$ssb2_kcal[POF$nse == "BAJO"]*(3*ssb_elast_nse_low/10)
POF$ssb2_kcal_red_nse_30[POF$nse == "MEDIO"] <- POF$ssb2_kcal[POF$nse == "MEDIO"]*(3*ssb_elast_nse_medium/10)
POF$ssb2_kcal_red_nse_30[POF$nse == "ALTO"] <- POF$ssb2_kcal[POF$nse == "ALTO"]*(3*ssb_elast_nse_high/10)

POF$ssb2_ml_red_nse_30 <- rep(NA, nrow(POF))
POF$ssb2_ml_red_nse_30[POF$nse == "BAJO"] <- POF$ssb2_qtd_gramas[POF$nse == "BAJO"]*(3*ssb_elast_nse_low/10)
POF$ssb2_ml_red_nse_30[POF$nse == "MEDIO"] <- POF$ssb2_qtd_gramas[POF$nse == "MEDIO"]*(3*ssb_elast_nse_medium/10)
POF$ssb2_ml_red_nse_30[POF$nse == "ALTO"] <- POF$ssb2_qtd_gramas[POF$nse == "ALTO"]*(3*ssb_elast_nse_high/10)



POF$otherbev_kcal_red_nse_20 <- rep(NA, nrow(POF))
POF$otherbev_kcal_red_nse_20[POF$nse == "BAJO"] <- POF$otherbev_kcal[POF$nse == "BAJO"]*(2*otherbev_elast_nse_low/10)
POF$otherbev_kcal_red_nse_20[POF$nse == "MEDIO"] <- POF$otherbev_kcal[POF$nse == "MEDIO"]*(2*otherbev_elast_nse_medium/10)
POF$otherbev_kcal_red_nse_20[POF$nse == "ALTO"] <- POF$otherbev_kcal[POF$nse == "ALTO"]*(2*otherbev_elast_nse_high/10)


POF$otherbev_ml_red_nse_20 <- rep(NA, nrow(POF))
POF$otherbev_ml_red_nse_20[POF$nse == "BAJO"] <- POF$otherbev_qtd_gramas[POF$nse == "BAJO"]*(2*otherbev_elast_nse_low/10)
POF$otherbev_ml_red_nse_20[POF$nse == "MEDIO"] <- POF$otherbev_qtd_gramas[POF$nse == "MEDIO"]*(2*otherbev_elast_nse_medium/10)
POF$otherbev_ml_red_nse_20[POF$nse == "ALTO"] <- POF$otherbev_qtd_gramas[POF$nse == "ALTO"]*(2*otherbev_elast_nse_high/10)


POF$otherbev_kcal_red_nse_30 <- rep(NA, nrow(POF))
POF$otherbev_kcal_red_nse_30[POF$nse == "BAJO"] <- POF$otherbev_kcal[POF$nse == "BAJO"]*(3*otherbev_elast_nse_low/10)
POF$otherbev_kcal_red_nse_30[POF$nse == "MEDIO"] <- POF$otherbev_kcal[POF$nse == "MEDIO"]*(3*otherbev_elast_nse_medium/10)
POF$otherbev_kcal_red_nse_30[POF$nse == "ALTO"] <- POF$otherbev_kcal[POF$nse == "ALTO"]*(3*otherbev_elast_nse_high/10)

POF$otherbev_ml_red_nse_30 <- rep(NA, nrow(POF))
POF$otherbev_ml_red_nse_30[POF$nse == "BAJO"] <- POF$otherbev_qtd_gramas[POF$nse == "BAJO"]*(3*otherbev_elast_nse_low/10)
POF$otherbev_ml_red_nse_30[POF$nse == "MEDIO"] <- POF$otherbev_qtd_gramas[POF$nse == "MEDIO"]*(3*otherbev_elast_nse_medium/10)
POF$otherbev_ml_red_nse_30[POF$nse == "ALTO"] <- POF$otherbev_qtd_gramas[POF$nse == "ALTO"]*(3*otherbev_elast_nse_high/10)


POF$alcohol_kcal_red_nse_20 <- rep(NA, nrow(POF))
POF$alcohol_kcal_red_nse_20[POF$nse == "BAJO"] <- POF$alcohol_kcal[POF$nse == "BAJO"]*(2*alcohol_elast_nse_low/10)
POF$alcohol_kcal_red_nse_20[POF$nse == "MEDIO"] <- POF$alcohol_kcal[POF$nse == "MEDIO"]*(2*alcohol_elast_nse_medium/10)
POF$alcohol_kcal_red_nse_20[POF$nse == "ALTO"] <- POF$alcohol_kcal[POF$nse == "ALTO"]*(2*alcohol_elast_nse_high/10)

POF$alcohol_ml_red_nse_20 <- rep(NA, nrow(POF))
POF$alcohol_ml_red_nse_20[POF$nse == "BAJO"] <- POF$alcohol_qtd_gramas[POF$nse == "BAJO"]*(2*alcohol_elast_nse_low/10)
POF$alcohol_ml_red_nse_20[POF$nse == "MEDIO"] <- POF$alcohol_qtd_gramas[POF$nse == "MEDIO"]*(2*alcohol_elast_nse_medium/10)
POF$alcohol_ml_red_nse_20[POF$nse == "ALTO"] <- POF$alcohol_qtd_gramas[POF$nse == "ALTO"]*(2*alcohol_elast_nse_high/10)

POF$alcohol_kcal_red_nse_30 <- rep(NA, nrow(POF))
POF$alcohol_kcal_red_nse_30[POF$nse == "BAJO"] <- POF$alcohol_kcal[POF$nse == "BAJO"]*(3*alcohol_elast_nse_low/10)
POF$alcohol_kcal_red_nse_30[POF$nse == "MEDIO"] <- POF$alcohol_kcal[POF$nse == "MEDIO"]*(3*alcohol_elast_nse_medium/10)
POF$alcohol_kcal_red_nse_30[POF$nse == "ALTO"] <- POF$alcohol_kcal[POF$nse == "ALTO"]*(3*alcohol_elast_nse_high/10)

POF$alcohol_ml_red_nse_30 <- rep(NA, nrow(POF))
POF$alcohol_ml_red_nse_30[POF$nse == "BAJO"] <- POF$alcohol_qtd_gramas[POF$nse == "BAJO"]*(3*alcohol_elast_nse_low/10)
POF$alcohol_ml_red_nse_30[POF$nse == "MEDIO"] <- POF$alcohol_qtd_gramas[POF$nse == "MEDIO"]*(3*alcohol_elast_nse_medium/10)
POF$alcohol_ml_red_nse_30[POF$nse == "ALTO"] <- POF$alcohol_qtd_gramas[POF$nse == "ALTO"]*(3*alcohol_elast_nse_high/10)


POF$diet_kcal_red_nse_20 <-rep(NA, nrow(POF))
POF$diet_kcal_red_nse_20[POF$nse == "BAJO"] <- POF$diet_kcal[POF$nse == "BAJO"]*(2*diet_elast_nse_low/10)
POF$diet_kcal_red_nse_20[POF$nse == "MEDIO"] <- POF$diet_kcal[POF$nse == "MEDIO"]*(2*diet_elast_nse_medium/10)
POF$diet_kcal_red_nse_20[POF$nse == "ALTO"] <- POF$diet_kcal[POF$nse == "ALTO"]*(2*diet_elast_nse_high/10)


POF$diet_ml_red_nse_20 <-rep(NA, nrow(POF))
POF$diet_ml_red_nse_20[POF$nse == "BAJO"] <- POF$diet_qtd_gramas[POF$nse == "BAJO"]*(2*diet_elast_nse_low/10)
POF$diet_ml_red_nse_20[POF$nse == "MEDIO"] <- POF$diet_qtd_gramas[POF$nse == "MEDIO"]*(2*diet_elast_nse_medium/10)
POF$diet_ml_red_nse_20[POF$nse == "ALTO"] <- POF$diet_qtd_gramas[POF$nse == "ALTO"]*(2*diet_elast_nse_high/10)


POF$diet_kcal_red_nse_30 <-rep(NA, nrow(POF))
POF$diet_kcal_red_nse_30[POF$nse == "BAJO"] <- POF$diet_kcal[POF$nse == "BAJO"]*(3*diet_elast_nse_low/10)
POF$diet_kcal_red_nse_30[POF$nse == "MEDIO"] <- POF$diet_kcal[POF$nse == "MEDIO"]*(3*diet_elast_nse_medium/10)
POF$diet_kcal_red_nse_30[POF$nse == "ALTO"] <- POF$diet_kcal[POF$nse == "ALTO"]*(3*diet_elast_nse_high/10)


POF$diet_ml_red_nse_30 <-rep(NA, nrow(POF))
POF$diet_ml_red_nse_30[POF$nse == "BAJO"] <- POF$diet_qtd_gramas[POF$nse == "BAJO"]*(3*diet_elast_nse_low/10)
POF$diet_ml_red_nse_30[POF$nse == "MEDIO"] <- POF$diet_qtd_gramas[POF$nse == "MEDIO"]*(3*diet_elast_nse_medium/10)
POF$diet_ml_red_nse_30[POF$nse == "ALTO"] <- POF$diet_qtd_gramas[POF$nse == "ALTO"]*(3*diet_elast_nse_high/10)


#Sum across beverage types to obtain cross price elasticity effect

POF <- POF %>% mutate(allbev_kcal_red_20 = reduce(select(., ends_with("kcal_red_20")), `+`),
                      allbev_kcal_red_30 = reduce(select(., ends_with("kcal_red_30")), `+`),
                      allbev_kcal_red_nse_20 = reduce(select(., ends_with("kcal_red_nse_20")), `+`),
                      allbev_kcal_red_nse_30 = reduce(select(., ends_with("kcal_red_nse_30")), `+`),
                      allbev_ml_red_20 = reduce(select(., ends_with("ml_red_20")), `+`),
                      allbev_ml_red_30 = reduce(select(., ends_with("ml_red_30")), `+`),
                      allbev_ml_red_nse_20 = reduce(select(., ends_with("ml_red_nse_20")), `+`),
                      allbev_ml_red_nse_30 = reduce(select(., ends_with("ml_red_nse_30")), `+`),
                      # allbev_kcal_red_nivel_edu_20 = reduce(select(., ends_with("kcal_red_nivel_edu_20")), `+`),
                      # allbev_kcal_red_nivel_edu_30 = reduce(select(., ends_with("kcal_red_nivel_edu_30")), `+`),
                      # allbev_ml_red_nivel_edu_20 = reduce(select(., ends_with("ml_red_nivel_edu_20")), `+`),
                      # allbev_ml_red_nivel_edu_30 = reduce(select(., ends_with("ml_red_nivel_edu_30")), `+`)
                      )

#Check that our variables are correct
#ver2 <- POF %>% dplyr::select(ends_with("ml_red_nivel_edu_20"))




#                               Body weight simulations
#==============================================================================================
#Recode sex for Hall's model
POF$sex <- rep(NA, nrow(POF))
POF$sex[POF$sexo == 1] <- "male"
POF$sex[POF$sexo == 2] <- "female"

#Variables of consumption change we want to simulate
vars <- c("ssb2_kcal_red_20",             
          "ssb2_kcal_red_30", 
          "ssb2_kcal_red_nse_20",
          "ssb2_kcal_red_nse_30",
          "allbev_kcal_red_20",                 
          "allbev_kcal_red_30",           
          "allbev_kcal_red_nse_20", "allbev_kcal_red_nse_30")

#Select variables of interest from our final database to minimize computational time
POF <- POF %>%
  dplyr::select(id, ESTRATO_POF, PESO_FINAL, COD_UPA, sex, nse,
         peso_ajustado, altura_ajustada, idade, ssb2_kcal, vars )



sum(POF$PESO_FINAL)
#Years to simulate
anos <- 10
seqanos        <- seq(1, 10, 1) 

#Uncomment to obtain mini database to check loop
# POF <- POF %>%
#    group_by(sex) %>%
#    sample_n(size = 10, replace = F) %>%
#    ungroup()



#save(POF, file = paste0(dir, "Model simulations/Results/PREsimbodyweight_", anos,"years_adjusted.rda"))

#Dummy variable to measure time at the end
start <- Sys.time()


#                              Body weight simulation loop
#==============================================================================================


for (i in 1:length(vars)) {

dias        <- (0:anos)*365

mat_reduc <- matrix(rep(unlist(POF[, vars[i]]), anos+1), ncol = anos +1)

energia <- energy_build(mat_reduc, dias, "Stepwise_R")


wtraje   <- adult_weight(bw = POF$peso_ajustado, ht = POF$altura_ajustada/100, age = POF$idade, 
                         sex = POF$sex, EIchange = energia, days = max(dias))


print(vars[i])

#for (j in 1:length(seqanos)) {  # Uncomment if you want all simulated years

  j = 10 # Comment if you want all simulated years

POF[1:nrow(POF), ncol(POF) + 1] <- wtraje[["Body_Weight"]][, seqanos[j]*365] #Body weight at day max(days) +`1 (last day)`
names(POF)[ncol(POF)] <- paste0("peso_", vars[i], "_", seqanos[j], "years")

POF[1:nrow(POF), ncol(POF) + 1] <- wtraje[["Body_Mass_Index"]][, seqanos[j]*365] #Body weight at day max(days) +`1 (last day)`
names(POF)[ncol(POF)] <- paste0("bmi_", vars[i], "_", seqanos[j], "years")

print(seqanos[j])

#} #Uncomment if you want all simulated years

}

end <- Sys.time() 
beepr::beep(3)

end - start #1 to 10 years all vars: Time difference of 1.252504 hours

glimpse(POF)

#Save results 
write.csv(POF, paste0(dir, "Model simulations/Results/finalsimbodyweight_", anos,"years_adjusted.csv"))
save(POF, file = paste0(dir, "Model simulations/Results/finalsimbodyweight_", anos,"years_adjusted.rda"))


#load(paste0(dir, "Model simulations/Results/finalsimbodyweight_", anos,"years_adjusted.rda"))
#Set design to summarise results

DesignPOF <- svydesign(id= ~id, strata= ~ESTRATO_POF, weights=~PESO_FINAL, PSU=~COD_UPA, data= POF)
options(survey.lonely.psu = "adjust")

names(POF)


round(svymean(~peso_ajustado, DesignPOF, na.rm = TRUE), 1)
round(confint(svymean(~peso_ajustado, DesignPOF, na.rm = TRUE)),1)

round(svymean(~altura_ajustada, DesignPOF),1)
round(confint(svymean(~altura_ajustada, DesignPOF)),1)

round(svymean(~sex, DesignPOF)*100,1)
round(confint(svymean(~sex, DesignPOF))*100,1)

round(svymean(~idade, DesignPOF), 1)
round(confint(svymean(~idade, DesignPOF)),1)

round(svymean(~nse, DesignPOF)*100,1)
round(confint(svymean(~nse, DesignPOF))*100,1)

round(svymean(~ssb2_kcal_red_nse_30, DesignPOF, na.rm = TRUE), 1)
round(confint(svymean(~ssb2_kcal_red_nse_30, DesignPOF, na.rm = TRUE)),1)


svymean(~peso_ssb2_kcal_red_nse_30_10years, DesignPOF)
svymean(~peso_ssb2_kcal_red_nse_30_10years, DesignPOF)

svymean(~peso_allbev_kcal_red_30_10years, DesignPOF)
svymean(~peso_allbev_kcal_red_nse_30_10years, DesignPOF)


