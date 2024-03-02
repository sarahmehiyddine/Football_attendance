#################################
# Nom : MEHIYDDINE              #             
# Prénom : Sarah                #                 
# Master SE                     #                           
#################################

rm(list=ls()) 
library(readxl)
library(dplyr)
library(tidyverse)
library(writexl)
library(ggplot2)
library(stargazer)
library(plm)
library(fBasics)

base <- read_excel("/Users/sarahmehiyddine/Desktop/Cours/Magistère/M1/S2/Économétrie appliquée /Données de panel/base_panel.xlsx")
base$attendance <- as.numeric(base$attendance)
base$t <- as.numeric(base$t)
base$Prix <- as.numeric(base$Prix)
attach(base)
base$proportion_stade <- base$attendance / base$capacity
basep <- pdata.frame(base, index = c("home_club", "t"), drop.index=
                       TRUE, row.names=TRUE)

#Statistiques descriptives : 
basicStats(base$Prix)
basicStats(base$equipe_nat)
basicStats(base$distance_km)


#----- Modèle OLS ------
reg2 <- lm(log(proportion_stade) ~ Prix + equipe_nat + chelsea + arsenal 
           + week_end + distance_km + victoire + relegation + diff, data=basep)
summary(reg2)


#------ Modèle WITHIN ------
within_i <- plm(log(proportion_stade) ~ Prix + equipe_nat + chelsea + arsenal 
                + week_end + distance_km + victoire + relegation + diff, data=basep, model="within", effect="indiv")
summary(within_i)


#------ Modèle GLS ------ 
Fgls_i <- plm(log(proportion_stade) ~ Prix + equipe_nat + chelsea + arsenal 
              + week_end + distance_km + victoire + relegation + diff, data=basep, model="random", effect="indiv")
summary(Fgls_i)


# On enregistre les résultats des 3 estimations dans un tableau latex
stargazer(reg2,within_i,Fgls_i, title = "Résultats",
          align = TRUE, header = FALSE, type ="latex", digits = 3)




# ------ TEST ------

#Nous disposons de 3 estimateurs, les tests suivants vont permettre de choisir lequel est le plus
#cohérent.

#------ Test multiplicateurs de Lagrange ------
F1 <- plmtest(log(proportion_stade) ~ Prix + equipe_nat + chelsea + arsenal 
              + week_end + distance_km + victoire + relegation + diff, data=basep, effect="indiv", type="bp")

F2 <- plmtest(log(proportion_stade) ~ Prix + equipe_nat + chelsea + arsenal 
              + week_end + distance_km + victoire + relegation + diff, data =basep, effect="time", type="bp")

F3 <- plmtest(log(proportion_stade) ~ Prix + equipe_nat + chelsea + arsenal 
              + week_end + distance_km + victoire + relegation + diff, data=basep, effect="twoways",
              type="bp")
F1;F2;F3 

#------ Test de Fisher ------
F4 <- pFtest(log(proportion_stade) ~ Prix + equipe_nat + chelsea + arsenal 
             + week_end + distance_km + victoire + relegation + diff, data=basep, effect="indiv")
F5 <- pFtest(log(proportion_stade) ~ Prix + equipe_nat + chelsea + arsenal 
             + week_end + distance_km + victoire + relegation + diff, data=basep, effect="time")
F6 <- pFtest(log(proportion_stade) ~ Prix + equipe_nat + chelsea + arsenal 
             + week_end + distance_km + victoire + relegation + diff, data=basep, effect="twoways")
F4;F5;F6


#------ Test d'Hausman ------ 
wi <- plm(log(proportion_stade) ~ Prix + equipe_nat + chelsea + arsenal 
          + week_end + distance_km + victoire + relegation + diff
          , data=basep, effect="indiv", model = "within")
glsi <- plm(log(proportion_stade) ~ Prix + equipe_nat + chelsea + arsenal 
            + week_end + distance_km + victoire + relegation + diff
            , data=basep, effect="indiv"
            , model = "random")
phtest(wi,glsi)