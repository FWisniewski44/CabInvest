# libraries survival analysis
library(survival)
library(survminer)
library(survMisc)
library(simPH)
library(flexsurv)
library(My.stepwise)

# libraries general
library(tidymodels)
library(tidyverse)
library(rms)
library(MASS)
library(corrplot)
library(lubridate)
library(Amelia)
library(xtable)
library(haven)
library(foreign)
library(broom)
library(psych)
library(Hmisc)
library(expss)
library(rockchalk)
library(effects)
library(modeldata)
library(randomForest)
library(compare)
library(psych)
library(desc)
library(PerformanceAnalytics)
library(reshape2)
library(d3heatmap)
library(kableExtra)
library(gtools)
library(gmodels)
library(summarytools)
library(data.table)
library(ggpubr)

##################################################### import dfs

erdda <- read_dta(file = "/Users/flo/Desktop/data/erdda/erdda_b-version-stata12.dta")

investErdda <- read_xls(path = "/Users/flo/Documents/GitHub/CabInvest/investERDDA.xls")

# pdda <- read_sav(file = "/Users/flo/Downloads/the Comparative Parliamentary Democracy Data Archive/the Comparative Parliamentary Democracy Data Archive.sav")

####

# renaming variables

investErdda <- investErdda %>%
  rename("formDur" = "v600e", "post_election" = "v303e", "country_name" = "v001e", "new_gov" = "v300e", 
         "effec_parties_parl" = "v309e", "polarization" = "v407e", "largest_party_dist" = "v409e",
         "majority_sit" = "v314e")

freq(investErdda$new_gov)

# generate factor (create individual levels) for the countries and change label names (for graphic used in paper)

investErdda$country_name <- factor(investErdda$country_name, labels = c("Austria", "Belgium", "Denmark", "Finland", "France",
                                            "Germany", "Greece", "Iceland", "Ireland", "Italy",
                                            "Luxembourg", "Netherlands", "Norway", "Portugal", "Spain",
                                            "Sweden", "UK", "Bulgaria", "Cyprus", "Czech Rep.",
                                            "Estonia", "Hungary", "Latvia", "Lithuania", "Malta",
                                            "Poland", "Romania", "Slovakia", "Slovenia"))

save(investErdda, file = "investERDDA_paper2021.RData")

####

# creation of subset for analysis

investSub <- subset(investErdda, select = c("formDur", "invest_timing", "invest_rightToNominate", "invest_whoVotes", 
                                            "invest_voteTarget", "invest_failure", "invest_decisionRule", "invest_Rounds",
                                            "post_election", "new_gov", "effec_parties_parl", "polarization",
                                            "largest_party_dist", "majority_sit"))

####

# (re)creation diermeier/van roozendaal 1998

corDier <- cor(na.omit(investSub))

d3heatmap(corDier, Rowv = F, Colv = F)

dierm_roozen <- coxph(data = investSub, Surv(time = formDur) ~ post_election + new_gov + invest_rightToNominate + invest_whoVotes +
        invest_voteTarget + invest_failure + invest_decisionRule + effec_parties_parl + polarization)

summary(dierm_roozen)


# (re)creation golder 2010

golder <- coxph(data = investSub, Surv(time = formDur) ~ post_election + new_gov + invest_rightToNominate + invest_whoVotes +
                  invest_voteTarget + invest_failure + invest_decisionRule + effec_parties_parl + polarization + majority_sit)

summary(golder)



freq(erdda$v600e)

firstEst <- coxph(data = investErdda, Surv(time = investErdda$v600e) ~ invest_timing + invest_whoVotes + 
                    invest_failure + invest_decisionRule)

# "event"	in Surv:
# The status indicator, normally 0=alive, 1=dead. Other choices are TRUE/FALSE (TRUE = death) or 1/2 (2=death).
# For interval censored data, the status indicator is 0=right censored, 1=event at time, 2=left censored, 3=interval censored.
# For multiple endpoint data the event variable will be a factor, whose first level is treated as censoring.
# Although unusual, the event indicator can be omitted, in which case all subjects are assumed to have an event.

summary(firstEst)
cox.zph(firstEst)

fre(investErdda$invest_decisionRule)










