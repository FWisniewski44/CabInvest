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

investErdda <- read_xls(path = "/Users/flo/Desktop/data/erdda/erdda_modified_invest.xls")

pdda <- read_sav(file = "/Users/flo/Downloads/the Comparative Parliamentary Democracy Data Archive/the Comparative Parliamentary Democracy Data Archive.sav")

Surv(time = pdda$v167y, time2 = pdda$v167y2)
Surv(time = pdda$v167y2)

Surv(time = investErdda$v600e, time2 = lead(investErdda$v600e))


# renaming

investErdda %>% rename("formDur" = "v600e", "post_election_cab" = "v303e",
                       )















freq(erdda$v600e)

firstEst <- coxph(data = investErdda, Surv(time = investErdda$v600e) ~ invest_timing + invest_rightToNominate + invest_whoVotes + 
                    invest_voteTarget + invest_failure + invest_decisionRule)

Surv(time = investErdda$v600e)
# "event"	in Surv:
# The status indicator, normally 0=alive, 1=dead. Other choices are TRUE/FALSE (TRUE = death) or 1/2 (2=death).
# For interval censored data, the status indicator is 0=right censored, 1=event at time, 2=left censored, 3=interval censored.
# For multiple endpoint data the event variable will be a factor, whose first level is treated as censoring.
# Although unusual, the event indicator can be omitted, in which case all subjects are assumed to have an event.



summary(firstEst)

fre(investErdda$v008e)









