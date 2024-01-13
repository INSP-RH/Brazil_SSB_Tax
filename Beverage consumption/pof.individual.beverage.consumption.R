rm(list=ls())
library(haven)
library(survey)
library(readr)
library(readxl)

#Add directory 
dir <- "~/Dropbox/Brazil/Brazil Github/"

#Read 24 hours recall from Brazilian Household Budget Survey (Pesquisa de Orçamentos Familiares - POF)
R24H <- read_dta(paste0(dir, "Data/POF/Original/POF2017_2018.dta"))

glimpse(R24H)


#Read list of food and beverages classified into subgroups (Paula's list)
FB <- read_excel(paste0(dir, "Data/POF/Original/POF_classification_2017-2018.xlsx"))


#                        R24H beverages classification
#====================================================================================

#Number of foods and beverages in R24H
length(unique(R24H$COD_ITEM )) #1591

#Number of foods and beverages in Paula's list
length(unique(FB$V9001)) #8708

#We do not expect the same length, Paula's list comes from household purchases survey


#This is a little bit extra but we can visualize better our selected beverage subgroups
#desc_group includes diet beverages
#desc_group2 does not include diet beverages

SSB <- filter(FB, desc_group == "SSBs")
SSB2 <- filter(FB, desc_group2 == "SSBs")
Alcohol <- filter(FB, desc_group == "Alc_bev")
OtherBev <- filter(FB, desc_group == "Other_bev", V9001 != 8216301) #remove cha de cavalinha, consumption is not correct
Diet <- filter(FB, desc_group2 == "Diet_bev")
Aguas <- filter(OtherBev,  str_detect(V9000_desc, "^AGUA"))


knitr::kable(unique(SSB2$V9000_desc))

length(unique(SSB2$V9000_desc))



#Initialize variables for beverage definitions
R24H$ssb <- rep(0, nrow(R24H))
R24H$ssb2 <- rep(0, nrow(R24H))
R24H$alcohol <- rep(0, nrow(R24H))
R24H$otherbev <- rep(0, nrow(R24H))
R24H$diet <- rep(0, nrow(R24H))
R24H$agua_potable <- rep(0, nrow(R24H))

#Search for codes already selected in subgroups
R24H$ssb[which(R24H$COD_ITEM %in% SSB$V9001)] <- 1
R24H$ssb2[which(R24H$COD_ITEM %in% SSB2$V9001)] <- 1
R24H$alcohol[which(R24H$COD_ITEM %in% Alcohol$V9001)] <- 1
R24H$otherbev[which(R24H$COD_ITEM %in% OtherBev$V9001)] <- 1
R24H$diet[which(R24H$COD_ITEM %in% Diet$V9001)] <- 1
R24H$agua_potable[which(R24H$COD_ITEM %in% Aguas$V9001)] <- 1

#Check selected groups
aguas <- filter(R24H, agua_potable == 1)
unique(aguas$COD_ITEM)

aguas2 <- Aguas[which(Aguas$V9001 %in% unique(aguas$COD_ITEM)), ]

knitr::kable(aguas2)



otherbev <- filter(R24H, otherbev == 1)
unique(otherbev$COD_ITEM)

otherbev2 <- OtherBev[which(OtherBev$V9001 %in% unique(otherbev$COD_ITEM)), ]

knitr::kable(otherbev2)



ssb2 <- filter(R24H, ssb2 == 1)
unique(ssb2$COD_ITEM)

ssb <- SSB2[which(SSB2$V9001 %in% unique(ssb2$COD_ITEM)), ]

knitr::kable(ssb)



#Check for compatibility
length(unique(R24H$COD_ITEM[which(R24H$ssb == 1)])) #97 vs 604 in Paula's list
length(unique(R24H$COD_ITEM[which(R24H$ssb2 == 1)])) #70 vs 279 in Paula's list
length(unique(R24H$COD_ITEM[which(R24H$alcohol == 1)])) #27 vs 117 in Paula's list
length(unique(R24H$COD_ITEM[which(R24H$otherbev == 1)])) #38 vs 257 in Paula's list
length(unique(R24H$COD_ITEM[which(R24H$diet == 1)])) #29 vs 337 in Paula's list

#Same idea different coding just to check
ver <- filter(R24H, ssb == 1)
length(unique(ver$COD_ITEM)) #Same as above



#                        Individual consumption estimation
#====================================================================================


#Create unique id for each individual
#According to Ana's notes: UF ESTRATO_POF TIPO_SITUACAO_REG COD_UPA NUM_DOM NUM_UC COD_INFORMANTE
R24H$id <- paste0(R24H$UF, R24H$ESTRATO_POF, R24H$TIPO_SITUACAO_REG, R24H$COD_UPA, R24H$NUM_DOM, 
                  R24H$NUM_UC, R24H$COD_INFORMANTE)

individuals <- length(unique(R24H$id))

verids <- select(R24H, ID_NUM, ID_SEQ_UNICA, id)
#Apparently we are correct: "Temos um recordatório do consumo de 24h durante 2 dias 
#para uma amostra de 46.164 indivíduos (amostra expandida de 52.906.759 indivíduos), 
#totalizando 1.175.390 observações. Cada linha da base de dados representa um alimento"


#Uncomment to estimate total intake 
# R24H$energia_kcal <-  R24H$ENERGIA_KCAL
# 
# R24H$gramatura1 <- R24H$GRAMATURA1
# 
# R24H$qtd_gramas2 <- R24H$QTD_FINAL

#Includes diet beverages
R24H$ssb_kcal <- ifelse((R24H$ssb == 1),
                        R24H$ENERGIA_KCAL, 0)

R24H$ssb_gramatura1 <- ifelse((R24H$ssb == 1),
                              R24H$GRAMATURA1, 0)

R24H$ssb_qtd_gramas2 <- ifelse((R24H$ssb == 1),
                               R24H$QTD_FINAL, 0)

#Does not include diet beverages
R24H$ssb2_kcal <- ifelse((R24H$ssb2 == 1),
                         R24H$ENERGIA_KCAL, 0)

R24H$ssb2_gramatura1 <- ifelse((R24H$ssb2 == 1),
                               R24H$GRAMATURA1, 0)

R24H$ssb2_qtd_gramas2 <- ifelse((R24H$ssb2 == 1),
                                R24H$QTD_FINAL, 0)


R24H$alcohol_kcal <- ifelse((R24H$alcohol == 1),
                            R24H$ENERGIA_KCAL, 0)

R24H$alcohol_gramatura1 <- ifelse((R24H$alcohol == 1),
                                  R24H$GRAMATURA1, 0)

R24H$alcohol_qtd_gramas2 <- ifelse((R24H$alcohol == 1),
                                   R24H$QTD_FINAL, 0)


R24H$otherbev_kcal <- ifelse((R24H$otherbev == 1),
                             R24H$ENERGIA_KCAL, 0)

R24H$otherbev_gramatura1 <- ifelse((R24H$otherbev == 1),
                                   R24H$GRAMATURA1, 0)

R24H$otherbev_qtd_gramas2 <- ifelse((R24H$otherbev == 1),
                                    R24H$QTD_FINAL, 0)

R24H$diet_kcal <- ifelse((R24H$diet == 1),
                         R24H$ENERGIA_KCAL, 0)

R24H$diet_gramatura1 <- ifelse((R24H$diet == 1),
                               R24H$GRAMATURA1, 0)

R24H$diet_qtd_gramas2 <- ifelse((R24H$diet == 1),
                                R24H$QTD_FINAL, 0)

R24H$potable_qtd_gramas2 <- ifelse((R24H$agua_potable == 1),
                                   R24H$QTD_FINAL, 0)


#Change haven_labelled object to numeric
R24H$DIA_SEMANA <- as.numeric(as_factor(R24H$DIA_SEMANA))

#Select variables of interest
ver <- select(R24H, "id", "COD_TBCA", "QUADRO", "DIA_SEMANA",
              "ssb_kcal", "ssb_gramatura1",  "ssb_qtd_gramas2",        
              "ssb2_kcal", "ssb2_gramatura1", "ssb2_qtd_gramas2",        
              "alcohol_kcal", "alcohol_gramatura1", "alcohol_qtd_gramas2",     
              "otherbev_kcal", "otherbev_gramatura1", "otherbev_qtd_gramas2",
              "diet_kcal", "diet_gramatura1", "diet_qtd_gramas2", "potable_qtd_gramas2")

#Collapse database to calculate individual consumption of taxed beverages

Taxed_day1 <- ver %>%
  filter(QUADRO == 72) %>%
  group_by(id) %>%
  summarize(ssb_kcal = sum(ssb_kcal, na.rm = TRUE), 
            ssb_gramatura = sum(ssb_gramatura1, na.rm = TRUE),
            ssb_qtd_gramas = sum(ssb_qtd_gramas2, na.rm = TRUE),
            ssb2_kcal = sum(ssb2_kcal, na.rm = TRUE), 
            ssb2_gramatura = sum(ssb2_gramatura1, na.rm = TRUE),
            ssb2_qtd_gramas = sum(ssb2_qtd_gramas2, na.rm = TRUE),
            alcohol_kcal = sum(alcohol_kcal, na.rm = TRUE), 
            alcohol_gramatura = sum(alcohol_gramatura1, na.rm = TRUE),
            alcohol_qtd_gramas = sum(alcohol_qtd_gramas2, na.rm = TRUE),
            otherbev_kcal = sum(otherbev_kcal, na.rm = TRUE), 
            otherbev_gramatura = sum(otherbev_gramatura1, na.rm = TRUE),
            otherbev_qtd_gramas = sum(otherbev_qtd_gramas2, na.rm = TRUE),
            diet_kcal = sum(diet_kcal, na.rm = TRUE), 
            diet_gramatura = sum(diet_gramatura1, na.rm = TRUE),
            diet_qtd_gramas = sum(diet_qtd_gramas2, na.rm = TRUE),
            potable_qtd_gramas = sum(potable_qtd_gramas2, na.rm = TRUE)) 



#WARNING: There is not self-reported anthropometric data included in this database, 
#check for availability

#Select survey design and socioeconomic variables 
svydesign      <- c("id", "SEXO", "IDADE_ANOS",
                    "PESO_FINAL", "PESO", "ESTRATO_POF", "COD_UPA")

Svydesign <- select(R24H, svydesign)
Svydesign <-distinct(Svydesign , id, .keep_all = TRUE)


TaxedDay1 <- merge(Taxed_day1,Svydesign , by="id", all = T )


#Set survey design to summarise results
Design <- svydesign(id= ~id, strata= ~ESTRATO_POF, weights=~PESO_FINAL, PSU=~COD_UPA, data= TaxedDay1)

options(survey.lonely.psu = "adjust")


svymean(~ssb_kcal, Design, na.rm = TRUE)
mean(TaxedDay1$ssb_kcal, na.rm = TRUE)
#svyquantile(~taxed_kcal, Design, quantiles = c(0.25, 0.5, 0.75), na.rm =TRUE)
svymean(~ssb_qtd_gramas, Design, na.rm =TRUE)
mean(TaxedDay1$ssb_qtd_gramas, na.rm =TRUE)
#svyquantile(~taxed_qtd_gramas, Design, quantiles = c(0.25, 0.5, 0.75), na.rm =TRUE)

svymean(~ssb2_kcal, Design, na.rm = TRUE)
mean(TaxedDay1$ssb2_kcal, na.rm = TRUE)
#svyquantile(~taxed_kcal, Design, quantiles = c(0.25, 0.5, 0.75), na.rm =TRUE)
svymean(~ssb2_qtd_gramas, Design, na.rm =TRUE)
mean(TaxedDay1$ssb2_qtd_gramas, na.rm =TRUE)


svymean(~alcohol_kcal, Design, na.rm = TRUE)
mean(TaxedDay1$alcohol_kcal, na.rm = TRUE)
#svyquantile(~taxed_kcal, Design, quantiles = c(0.25, 0.5, 0.75), na.rm =TRUE)
svymean(~alcohol_qtd_gramas, Design, na.rm =TRUE)
mean(TaxedDay1$alcohol_qtd_gramas, na.rm =TRUE)


svymean(~otherbev_kcal, Design, na.rm = TRUE)
mean(TaxedDay1$otherbev_kcal, na.rm = TRUE)
#svyquantile(~taxed_kcal, Design, quantiles = c(0.25, 0.5, 0.75), na.rm =TRUE)
svymean(~otherbev_qtd_gramas, Design, na.rm =TRUE)
mean(TaxedDay1$otherbev_qtd_gramas, na.rm =TRUE)


svymean(~diet_kcal, Design, na.rm = TRUE)
mean(TaxedDay1$diet_kcal, na.rm = TRUE)
#svyquantile(~taxed_kcal, Design, quantiles = c(0.25, 0.5, 0.75), na.rm =TRUE)
svymean(~diet_qtd_gramas, Design, na.rm =TRUE)
mean(TaxedDay1$diet_qtd_gramas, na.rm =TRUE)

svymean(~potable_qtd_gramas, Design, na.rm = TRUE)

length(unique(TaxedDay1$id))

TaxedDay1S <- TaxedDay1



#Save database (already in folders)
write.csv(TaxedDay1,paste0(dir, "Data/POF/Clean/POF_SANDRA_indiv_sin_cavalinha.csv"))

save(TaxedDay1S,file = paste0(dir, "Data/POF/Clean/POF_SANDRA_indiv_sin_cavalinha.rda"))














