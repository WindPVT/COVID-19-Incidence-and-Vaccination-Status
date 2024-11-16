#import libraries
library(survival)
library(survminer)
library(ggplot2)
library(epitools)

#attach data table
attach(Vaccination_Incidence_Analysis_Data)

#generate model and cumulative incidence plot
fit <- survfit(Surv(TIME, INFECT) ~ VAX, data = Vaccination_Incidence_Analysis_Data)
ggsurvplot(fit,
           fun ="event",
           pval = TRUE,
           pval.coord = c(0,0.30),
           conf.int = TRUE,
           cumevents = FALSE, cumevents.col ="strata",
           risk.table = "nrisk_cumevents", risk.table.col = "strata", 
           censor = TRUE, censor.size = 10,
           mark.time=TRUE,
           size = 2,
           legend.labs = c("Unvaccinated", "Vaccinated"),
           ylab = "Cumulative Incidence (%)",
           xlab = "Time (days)", 
           break.x.by = 25,
           #ylim = c(0.00,0.15),
           #xlim = c(0,225),
           surv.scale = "percent",
           break.time.by = 1)

#generate summary table
summary(fit)
surv_diff <- survdiff(Surv(TIME, INFECTION) ~ VACCINATION, data = Vaccination_Incidence_Analysis_Data)
surv_diff

#calculate IRR for Documented Infection and 95%CI
IRR_Documented<-matrix(c(39,134,45456,191171),nrow = 2, ncol = 2)
rateratio.wald(IRR_Documented)

#IRR for symptomatic infection
IRR_Symptomatic<-matrix(c(5,16,84953,29572),nrow = 2, ncol = 2)
rateratio.wald(IRR_Symptomatic)
