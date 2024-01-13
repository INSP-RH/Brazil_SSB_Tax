rm(list = ls())
library(tidyverse)
library(janitor)
library(survey)
library(srvyr)
library(haven)
library(reshape)

#Set directory
dir <- "~/Dropbox/Brazil/Brazil Github/"

#           Read POF database with self-reported anthropometric data (constructed by us)
#=================================================================================================
load(paste0(dir, "Data/POF/Clean/MasterPOF_antrosandra.rda"))
length(unique(MasterPOF$id))

#Change names to avoid confusion
MasterPOF$peso_reportado <- MasterPOF$PESO_KG
MasterPOF$altura_reportada <- MasterPOF$ALTURA_CM

#Adults only
MasterPOF <- filter(MasterPOF, IDADE_ANOS >= 20)

#Partition by sex for better adjustment
POF_F <- filter(MasterPOF, SEXO == 2)
POF_M <- filter(MasterPOF, SEXO == 1)


#Set design for females
DesignPOF_F <- svydesign(id= ~id, strata= ~ESTRATO_POF, weights=~PESO_FINAL, PSU=~COD_UPA, data= POF_F)
options(survey.lonely.psu = "adjust")

#Set design for males
DesignPOF_M <- svydesign(id= ~id, strata= ~ESTRATO_POF, weights=~PESO_FINAL, PSU=~COD_UPA, data= POF_M)
options(survey.lonely.psu = "adjust")


#Get weight quantiles by sex
WeightPOF_F <- oldsvyquantile(x = POF_F$peso_reportado, design = DesignPOF_F, 
                           quantiles = seq(0.1, 0.99, 0.001))

WeightPOF_M <- oldsvyquantile(POF_M$peso_reportado, DesignPOF_M, 
                           seq(0.1, 0.99, 0.001))

#Join weight quantiles into single dataframe
WeightPOF <- data.frame(Quantile = seq(0.1, 0.99, 0.001), KG_POF_F = WeightPOF_F,
                        KG_POF_M = WeightPOF_M)


mWeightPOF <- dplyr::rename(WeightPOF, Females = KG_POF_F, Males = KG_POF_M) 
mWeightPOF <- melt(mWeightPOF, id.vars = "Quantile")
mWeightPOF$data <- "Self-reported"
mWeightPOF$measure <- "Weight (kg)"


#Same for height
HeightPOF_F <- oldsvyquantile(POF_F$altura_reportada, DesignPOF_F, seq(0.1, 0.99, 0.001))
HeightPOF_M <- oldsvyquantile(POF_M$altura_reportada, DesignPOF_M, seq(0.1, 0.99, 0.001))

HeightPOF <- data.frame(Quantile = seq(0.1, 0.99, 0.001), CM_POF_F = HeightPOF_F, CM_POF_M = HeightPOF_M)


mHeightPOF <- dplyr::rename(HeightPOF, Females = CM_POF_F, Males = CM_POF_M) 
mHeightPOF <- melt(mHeightPOF, id.vars = "Quantile")
mHeightPOF$data <- "Self-reported"
mHeightPOF$measure <- "Height (cm)"


#Join anthropometric quantiles from POF into a single dataframe
QuantilesPOF <- cbind(WeightPOF, HeightPOF)


#                     Read PNS survey with measured anthropometric data
#=================================================================================================
PNS <- read.csv(paste0(dir, "/Data/PNS/PNS2019_6730obs.csv"))
glimpse(PNS)

#Create id for each individual
#A chave de pessoas é composta pelas variáveis: V0001 + V0024 + UPA_PNS + V0006_PNS + C00301.
PNS$id <- paste0(PNS$V0001, PNS$V0024, PNS$UPA_PNS, PNS$V0006_PNS, PNS$C00301)

length(unique(PNS$id)) 

#2) Para análises que consideram os moradores adultos selecionados (blocos M ao X), 
#você deverá considerar: 
#UPA (samplingunits): UPA_PNS 
#Estrato (strata): V0024 
#Peso do morador selecionado (samplingweightvariable): V00301

summary(PNS$C008)

#Expanded survey weights (15 a 104 años)
sum(PNS$V00301)

#Edad = C008   #Adults only
PNS <- filter(PNS, C008 >= 20)

#A cuanto expanden los adultos
#sum(PNS$V00291) #esta mal
sum(PNS$V00301)

#Partition by sex for better adjustment
PNS_F <- filter(PNS, C006 == 2)
PNS_M <- filter(PNS, C006 == 1)

#Set design for females
DesignPNS_F <- svydesign(id= ~id, strata= ~V0024, weights=~V00301, PSU=~UPA_PNS, data= PNS_F)
options(survey.lonely.psu = "adjust")

#Set design for males
DesignPNS_M <- svydesign(id= ~id, strata= ~V0024, weights=~V00301, PSU=~UPA_PNS, data= PNS_M)
options(survey.lonely.psu = "adjust")


#Weight = W00103
#Height = W00203
#Sex = C006

summary(PNS$W00103)
class(PNS$W00103)  

summary(PNS$W00203)
class(PNS$W00203)  


#Get weight quantiles by sex
WeightPNS_F <- oldsvyquantile(PNS_F$W00103, DesignPNS_F, seq(0.1, 0.99, 0.001))
WeightPNS_F <- data.frame(KG_PNS_F = WeightPNS_F)

WeightPNS_M <- oldsvyquantile(PNS_M$W00103, DesignPNS_M, seq(0.1, 0.99, 0.001))

WeightPNS <- data.frame(Quantile = seq(0.1, 0.99, 0.001), KG_PNS_F = WeightPNS_F,
                        KG_PNS_M = WeightPNS_M)

mWeightPNS <- dplyr::rename(WeightPNS, Females = KG_PNS_F, Males = KG_PNS_M) 
mWeightPNS <- melt(mWeightPNS, id.vars = "Quantile")
mWeightPNS$data <- "Measured"
mWeightPNS$measure <- "Weight (kg)"


#Get height quantiles by sex
HeightPNS_F <- oldsvyquantile(PNS_F$W00203, DesignPNS_F, seq(0.1, 0.99, 0.001))
HeightPNS_F <- data.frame(CM_PNS_F = HeightPNS_F)

HeightPNS_M <- oldsvyquantile(PNS_M$W00203, DesignPNS_M, seq(0.1, 0.99, 0.001))

HeightPNS<- data.frame(Quantile = seq(0.1, 0.99, 0.001),
                       CM_PNS_M = HeightPNS_M, CM_PNS_F = HeightPNS_F)

mHeightPNS <- dplyr::rename(HeightPNS, Females = CM_PNS_F, Males = CM_PNS_M) 
mHeightPNS <- melt(mHeightPNS, id.vars = "Quantile")
mHeightPNS$data <- "Measured"
mHeightPNS$measure <- "Height (cm)"


#Join anthropometric quantiles from PNS into a single dataframe
QuantilesPNS <- cbind(WeightPNS, HeightPNS)

#Join anthropometric quantiles a single dataframe
Quantiles <- cbind(QuantilesPOF, QuantilesPNS)
Quantiles <- Quantiles[, !duplicated(colnames(Quantiles))]

#Estimate anthropometric differences by quantile across PNS (measured) and POF (reported)
Quantiles <- Quantiles %>%
  mutate(DIF_KG_F = KG_PNS_F - KG_POF_F, DIF_CM_F = CM_PNS_F - CM_POF_F,
         DIF_KG_M = KG_PNS_M - KG_POF_M, DIF_CM_M = CM_PNS_M - CM_POF_M)

#Remove extra databases
rm("HeightPNS",   "HeightPOF",
   "QuantilesPNS",  "QuantilesPOF",  "WeightPNS",    "WeightPOF",
   "WeightPNS_F", "HeightPNS_F")



#                              Plot our results                        
#********************************************************************************
antrodif_df <- rbind.data.frame(mWeightPOF, mWeightPNS, mHeightPOF, mHeightPNS) 


antro.compare <- ggplot() + 
  theme_bw() +
  #ggtitle("Females") +
  geom_point(data = antrodif_df, aes(x = Quantile, y = value, group = data, color = data)) +
  xlab("Quantile") +
  ylab("") +
  facet_grid(cols = vars(variable), rows = vars(measure), scales = "free") +
  # scale_color_manual("", values = c("Measured" = "deepskyblue",
  #                                                 "Self-reported" = "deepskyblue4"),
  #                   labels = c("Self-reported" = "Self-reported data from \n Pesquisa de Orçamentos \n Familiares (POF 2017-2018) \n",
  #                              "Measured" = "Measured data from \n Pesquisa Nacional de Saúde \n (PNS 2019) \n")) +
  # 
  scale_color_manual("", values = c("Measured" = "deepskyblue",
                                    "Self-reported" = "deepskyblue4"),
                     labels = c("Self-reported" = "Self-reported data from Pesquisa de Orçamentos Familiares (POF 2017-2018)",
                                "Measured" = "Measured data from Pesquisa Nacional de Saúde (PNS 2019)")) +
  
  
  guides(colour=guide_legend(nrow=2)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size = 14),
        legend.position = "bottom",
        legend.margin=margin(),
        axis.text = element_text(size = 16 ), 
        axis.title = element_text(size = 17),
        plot.title = element_text(size = 19, hjust = 0.5),
        panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        #strip.background = element_blank(),
        #panel.border = element_rect(colour = "black", fill = NA),
        strip.text = element_text(size = 20)) 

antro.compare

ggsave(antro.compare, file = paste0(dir,
                                    "Adjustment of self-reporting bias/Results/anthropometryreportedvsmeasured.png"),
       width = 12, height = 9)








#                           Body weight adjustment for females
#=====================================================================================
#Fit cubic spline to height differences by quantile between POF and PNS surveys
fit.peso         <- smooth.spline(Quantiles$Quantile, Quantiles$DIF_KG_F, df=16) 

cdf.est.peso     <- svycdf(~peso_reportado, DesignPOF_F)
POF_F$cuantil.peso <- cdf.est.peso[[1]](POF_F$peso_reportado)

#Get expected difference for reported weight
POF_F$predict.peso <- predict(fit.peso, POF_F$cuantil.peso)$y

#Add difference to reported weight
POF_F$peso_ajustado <- POF_F$peso_reportado + POF_F$predict.peso


#Quick summary
#Self-reported bodyweight in POF
svymean(POF_F$peso_reportado, DesignPOF_F)
#Adjusted bodyweight in POF
svymean(POF_F$peso_ajustado, DesignPOF_F)
#Measured bodyweight
svymean(PNS_F$W00103, DesignPNS_F)

oldsvyquantile(POF_F$peso_reportado, DesignPOF_F, c(0.25, 0.5, 0.75))
oldsvyquantile(POF_F$peso_ajustado, DesignPOF_F, c(0.25, 0.5, 0.75))
oldsvyquantile(PNS_F$W00103, DesignPNS_F, c(0.25, 0.5, 0.75))



#         *****************************************************************************
#         * Repeat same process for female height and bodyweight and height for males *
#         *****************************************************************************



#                               Height adjustment for females
#=====================================================================================
fit.altura         <- smooth.spline(Quantiles$Quantile, Quantiles$DIF_CM_F, df=16) #REVISAR

cdf.est.altura     <- svycdf(~altura_reportada, DesignPOF_F)
POF_F$cuantil.altura <- cdf.est.altura[[1]](POF_F$altura_reportada)

POF_F$predict.altura <- predict(fit.altura, POF_F$cuantil.altura)$y

POF_F$altura_ajustada <- POF_F$altura_reportada + POF_F$predict.altura


svymean(POF_F$altura_reportada, DesignPOF_F)
svymean(POF_F$altura_ajustada, DesignPOF_F)
svymean(PNS_F$W00203, DesignPNS_F)

oldsvyquantile(POF_F$altura_reportada, DesignPOF_F, c(0.25, 0.5, 0.75))
oldsvyquantile(POF_F$altura_ajustada, DesignPOF_F, c(0.25, 0.5, 0.75))
oldsvyquantile(PNS_F$W00203, DesignPNS_F, c(0.25, 0.5, 0.75))



#                                Body weight adjustment for males
#=====================================================================================
fit.peso         <- smooth.spline(Quantiles$Quantile, Quantiles$DIF_KG_M, df=16) #REVISAR

cdf.est.peso     <- svycdf(~peso_reportado, DesignPOF_M)
POF_M$cuantil.peso <- cdf.est.peso[[1]](POF_M$peso_reportado)

POF_M$predict.peso <- predict(fit.peso, POF_M$cuantil.peso)$y

POF_M$peso_ajustado <- POF_M$peso_reportado + POF_M$predict.peso


svymean(POF_M$peso_reportado, DesignPOF_M)
svymean(POF_M$peso_ajustado, DesignPOF_M)
svymean(PNS_M$W00103, DesignPNS_M)

oldsvyquantile(POF_M$peso_reportado, DesignPOF_M, c(0.25, 0.5, 0.75))
oldsvyquantile(POF_M$peso_ajustado, DesignPOF_M, c(0.25, 0.5, 0.75))
oldsvyquantile(PNS_M$W00103, DesignPNS_M, c(0.25, 0.5, 0.75))



#                                Height adjustment for females
#=====================================================================================
fit.altura         <- smooth.spline(Quantiles$Quantile, Quantiles$DIF_CM_M, df=16) 

cdf.est.altura     <- svycdf(~altura_reportada, DesignPOF_M)
POF_M$cuantil.altura <- cdf.est.altura[[1]](POF_M$altura_reportada)

POF_M$predict.altura <- predict(fit.altura, POF_M$cuantil.altura)$y

POF_M$altura_ajustada <- POF_M$altura_reportada + POF_M$predict.altura



svymean(POF_M$altura_reportada, DesignPOF_M)
svymean(POF_M$altura_ajustada, DesignPOF_M)
svymean(PNS_M$W00203, DesignPNS_M)

oldsvyquantile(POF_M$altura_reportada, DesignPOF_M, c(0.25, 0.5, 0.75))
oldsvyquantile(POF_M$altura_ajustada, DesignPOF_M, c(0.25, 0.5, 0.75))
oldsvyquantile(PNS_M$W00203, DesignPNS_M, c(0.25, 0.5, 0.75))



#                              Plot our results                        
#********************************************************************************

Altura_ajustada <- rbind.data.frame(POF_F, POF_M) %>%
        select(SEXO, altura_ajustada, 
                PESO_FINAL)  %>%
        mutate(data = "Adjusted", measure = "Height (cm)") %>%
dplyr::rename(variable = SEXO,
              value = altura_ajustada) 
  


Peso_ajustado <- rbind.data.frame(POF_F, POF_M) %>%
  select(SEXO, peso_ajustado, 
         PESO_FINAL)  %>%
  mutate(data = "Adjusted", measure = "Weight (cm)") %>%
  dplyr::rename(variable = SEXO,
              value = peso_ajustado) 
  



Altura_reportada <- rbind.data.frame(POF_F, POF_M) %>%
  select(SEXO, altura_reportada, 
         PESO_FINAL)  %>%
  mutate(data = "Reported", measure = "Height (cm)")%>%
  dplyr::rename(variable = SEXO,
              value = altura_reportada) 
  


Peso_reportado <- rbind.data.frame(POF_F, POF_M) %>%
  select(SEXO, peso_reportado, 
         PESO_FINAL)  %>%
  mutate(data = "Reported", measure = "Weight (cm)")%>%
  dplyr::rename(variable = SEXO,
              value = peso_reportado) 
  


Peso_medido <- rbind.data.frame(PNS_F, PNS_M) %>%
  select(C006, W00103, V00301) %>%
  dplyr::rename(SEXO = C006,
                peso_medido = W00103, PESO_FINAL = V00301) %>%
  dplyr::rename(variable = SEXO,
                value = peso_medido) %>%
  
  mutate(data = "Measured", measure = "Weight (cm)")


Altura_medida <- rbind.data.frame(PNS_F, PNS_M) %>%
  select(C006, W00203, V00301) %>%
  dplyr::rename(SEXO = C006,
                altura_medida = W00203, PESO_FINAL = V00301) %>%
  dplyr::rename(variable = SEXO,
                value = altura_medida) %>%
  mutate(data = "Measured", measure = "Height (cm)")


antro_adj_df <- rbind.data.frame(Altura_ajustada, Altura_reportada,
                                 Altura_medida, Peso_ajustado, Peso_reportado, Peso_medido)

antro_adj_df$variable[antro_adj_df$variable == 1] <- "Males"
antro_adj_df$variable[antro_adj_df$variable == 2] <- "Females"

rm(Altura_ajustada, Altura_reportada,
   Altura_medida, Peso_ajustado, Peso_reportado, Peso_medido)


antro.adj <- ggplot() + 
  
  theme_bw() +
  
  geom_density(data = antro_adj_df, aes(value, color = data, weight = PESO_FINAL), size = 0.8) +
  
  ylab("") +
  
  facet_grid(cols = vars(measure), rows = vars(variable), scales = "free") +
 
   scale_colour_discrete("", 
                     labels = c("Reported" = "Self-reported data from Pesquisa de Orçamentos Familiares (POF 2017-2018)",
                                "Measured" = "Measured data from Pesquisa Nacional de Saúde (PNS 2019)",
                                "Adjusted" = "Adjusted self-reported data from Pesquisa de Orçamentos Familiares (POF 2017-2018)")) +
  
  
  guides(colour=guide_legend(nrow=3)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size = 14),
        legend.position = "bottom",
        legend.margin=margin(),
        axis.text = element_text(size = 16 ), 
        axis.title = element_text(size = 17),
        plot.title = element_text(size = 19, hjust = 0.5),
        panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        #strip.background = element_blank(),
        #panel.border = element_rect(colour = "black", fill = NA),
        strip.text = element_text(size = 20)) 

antro.adj

ggsave(antro.adj, file = paste0(dir,
                                    "Adjustment of self-reporting bias/Results/anthropometryadjusted.png"),
       width = 12, height = 9)





#                                Obesity prevalences
#==========================================================================================

POF_adjusted <- rbind(POF_F, POF_M)
PNS_measured <- rbind(PNS_F, PNS_M)


POF_adjusted <- mutate(POF_adjusted, bmi_reportado = peso_reportado/((altura_reportada)/100)^2,
                       bmi_ajustado = peso_ajustado/((altura_ajustada)/100)^2)

POF_adjusted$bmi_cat_reportado <- rep(NA, nrow(POF_adjusted))
POF_adjusted$bmi_cat_reportado <- 0
POF_adjusted$bmi_cat_reportado[POF_adjusted$bmi_reportado >= 30] <- 1

POF_adjusted$bmi_cat_ajustado <- rep(NA, nrow(POF_adjusted))
POF_adjusted$bmi_cat_ajustado <- 0
POF_adjusted$bmi_cat_ajustado[POF_adjusted$bmi_ajustado >= 30] <- 1




PNS_measured <- mutate(PNS_measured, bmi_medido = W00103/((W00203)/100)^2)

PNS_measured$bmi_cat_medido <- rep(NA, nrow(PNS_measured))
PNS_measured$bmi_cat_medido <- 0
PNS_measured$bmi_cat_medido[PNS_measured$bmi_medido >= 30] <- 1


#Save final database (already in folder)
save(POF_adjusted, file = paste0(dir, "Data/Final database/POF_SANDRA_indiv_antrosandra_adjusted.rda"))


DesignPNS <- svydesign(id= ~id, strata= ~V0024, weights=~V00301, PSU=~UPA_PNS, data= PNS_measured)
options(survey.lonely.psu = "adjust")

DesignPOFad <- svydesign(id= ~id, strata= ~ESTRATO_POF, weights=~PESO_FINAL, PSU=~COD_UPA, data= POF_adjusted)
options(survey.lonely.psu = "adjust")

svymean(POF_adjusted$bmi_cat_reportado, DesignPOFad)
svymean(POF_adjusted$bmi_cat_ajustado, DesignPOFad)
svymean(PNS_measured$bmi_cat_medido, DesignPNS)

