Sys.setenv(LANG = "en")
library(RPresence)
library(ggplot2)
library(tidyr)
library(sf)
library(ggpubr)
setwd("~/Kristina/MA/adjusted_grids")

load('single_species.RData')

#load grid cells of study area
grid <- st_read(dsn = "grid_5x5.gpkg")


#load data occupancy data
LS<-read.csv("Livestock_Det_clean.csv")[,-1]
CF<-read.csv("Conflict_Det_clean.csv")[,-1]
Sitecov<-read.csv("GridCovariates_clean.csv")[,-c(1,2,3,5)]
Surv_cov_Year<-read.csv("Surv_cov_Year_clean.csv")[,-1]
SurvCov_Summer<-read.csv("Surv_cov_Summer_clean.csv")[,-1]
Surv_cov_Area_covered<-read.csv("Surv_cov_Area_covered_clean.csv")[,-1]




#scale covariates
surv_cov_scaled <- list(area.z = as.matrix(scale(Surv_cov_Area_covered[,-1])),
                      year.z =as.matrix(scale(Surv_cov_Year[,-1])),
                      summer.z =as.matrix(scale(SurvCov_Summer[,-1])))

site_cov_scaled <- as.data.frame(scale(Sitecov[,-1]))


#### model LS


LS.pao <- createPao(data= LS[,-1],
                    unitnames = LS[,1],
                    unitcov = site_cov_scaled,
                    survcov = surv_cov_scaled)


LS.null<-occMod(model = list(psi~1, p~1), data = LS.pao, type = "so")

LS_1a <- occMod(model = list(psi~NDVI, p~1), data = LS.pao, type = "so")
LS_1b <- occMod(model = list(psi~NDVI, p~area.z), data = LS.pao, type = "so")
LS_1c <- occMod(model = list(psi~NDVI, p~area.z+year.z), data = LS.pao, type = "so")
LS_1d <- occMod(model = list(psi~NDVI, p~area.z+year.z+summer.z), data = LS.pao, type = "so")
LS_1e <- occMod(model = list(psi~NDVI, p~year.z), data = LS.pao, type = "so")

LS_2a <- occMod(model = list(psi~dist_PA, p~1), data = LS.pao, type = "so")
LS_2b <- occMod(model = list(psi~dist_PA, p~area.z), data = LS.pao, type = "so")
LS_2c <- occMod(model = list(psi~dist_PA, p~area.z+year.z), data = LS.pao, type = "so")
LS_2d <- occMod(model = list(psi~dist_PA, p~area.z+year.z+summer.z), data = LS.pao, type = "so")
LS_2e <- occMod(model = list(psi~dist_PA, p~year.z), data = LS.pao, type = "so")


LS_3a <- occMod(model = list(psi~dist_PA+NDVI, p~1), data = LS.pao, type = "so")
LS_3b <- occMod(model = list(psi~dist_PA+NDVI, p~area.z), data = LS.pao, type = "so")
LS_3c <- occMod(model = list(psi~dist_PA+NDVI, p~area.z+year.z), data = LS.pao, type = "so")
LS_3d <- occMod(model = list(psi~dist_PA+NDVI, p~area.z+year.z+summer.z), data = LS.pao, type = "so")
LS_3e <- occMod(model = list(psi~dist_PA+NDVI, p~year.z), data = LS.pao, type = "so")

LS_4a <- occMod(model = list(psi~tri, p~1), data = LS.pao, type = "so")
LS_4b <- occMod(model = list(psi~tri, p~area.z), data = LS.pao, type = "so")
LS_4c <- occMod(model = list(psi~tri, p~area.z+year.z), data = LS.pao, type = "so")
LS_4d <- occMod(model = list(psi~tri, p~area.z+year.z+summer.z), data = LS.pao, type = "so")
LS_4e <- occMod(model = list(psi~tri, p~year.z), data = LS.pao, type = "so")

LS_5a <- occMod(model = list(psi~dist_PA+NDVI+tri, p~1), data = LS.pao, type = "so")
LS_5b <- occMod(model = list(psi~dist_PA+NDVI+tri, p~area.z), data = LS.pao, type = "so")
LS_5c <- occMod(model = list(psi~dist_PA+NDVI+tri, p~area.z+year.z), data = LS.pao, type = "so")
LS_5d <- occMod(model = list(psi~dist_PA+NDVI+tri, p~area.z+year.z+summer.z), data = LS.pao, type = "so")
LS_5e <- occMod(model = list(psi~dist_PA+NDVI+tri, p~year.z), data = LS.pao, type = "so")

LS_6a <- occMod(model = list(psi~river_mean, p~1), data = LS.pao, type = "so")
LS_6b <- occMod(model = list(psi~river_mean, p~area.z), data = LS.pao, type = "so")
LS_6c <- occMod(model = list(psi~river_mean, p~area.z+year.z), data = LS.pao, type = "so")
LS_6d <- occMod(model = list(psi~river_mean, p~area.z+year.z+summer.z), data = LS.pao, type = "so")
LS_6e <- occMod(model = list(psi~river_mean, p~year.z), data = LS.pao, type = "so")

LS_7a <- occMod(model = list(psi~roads_mean, p~1), data = LS.pao, type = "so")
LS_7b <- occMod(model = list(psi~roads_mean, p~area.z), data = LS.pao, type = "so")
LS_7c <- occMod(model = list(psi~roads_mean, p~area.z+year.z), data = LS.pao, type = "so")
LS_7d <- occMod(model = list(psi~roads_mean, p~area.z+year.z+summer.z), data = LS.pao, type = "so")
LS_7e <- occMod(model = list(psi~roads_mean, p~year.z), data = LS.pao, type = "so")

LS_8a <- occMod(model = list(psi~dist_PA+NDVI+tri+river_mean, p~1), data = LS.pao, type = "so")
LS_8b <- occMod(model = list(psi~dist_PA+NDVI+tri+river_mean, p~area.z), data = LS.pao, type = "so")
LS_8c <- occMod(model = list(psi~dist_PA+NDVI+tri+river_mean, p~area.z+year.z), data = LS.pao, type = "so")
LS_8d <- occMod(model = list(psi~dist_PA+NDVI+tri+river_mean, p~area.z+year.z+summer.z), data = LS.pao, type = "so")
LS_8e <- occMod(model = list(psi~dist_PA+NDVI+tri+river_mean, p~year.z), data = LS.pao, type = "so")

LS_9a<-occMod(model = list(psi~1, p~area.z), data = LS.pao, type = "so")
LS_9b<-occMod(model = list(psi~1, p~area.z+year.z), data = LS.pao, type = "so")
LS_9c<-occMod(model = list(psi~1, p~area.z+year.z+summer.z), data = LS.pao, type = "so")
LS_9d<-occMod(model = list(psi~1, p~year.z), data = LS.pao, type = "so")
LS_9e<-occMod(model = list(psi~1, p~summer.z), data = LS.pao, type = "so")



LS_models <- list(LS.null,
                  LS_1a,LS_1b,LS_1c,LS_1d,LS_1e,
                  LS_2a,LS_2b,LS_2c,LS_2d,LS_2e,
                  LS_3a,LS_3b,LS_3c,LS_3d,LS_3e,
                  LS_4a,LS_4b,LS_4c,LS_4d,LS_4e,
                  LS_5a,LS_5b,LS_5c,LS_5d,LS_5e,
                  LS_6a,LS_6b,LS_6c,LS_6d,LS_6e,
                  LS_7a,LS_7b,LS_7c,LS_7d,LS_7e,
                  LS_8a,LS_8b,LS_8c,LS_8d,LS_8e,
                  LS_9a,LS_9b,LS_9c,LS_9d,LS_9e
)


LS.results <- createAicTable(LS_models,use.aicc = TRUE) # Put all models and their AIC values together

summary(LS.results) # Create comparable AIC table from the above list


#write summary to csv
write.table(summary(LS.results),'ls_table.csv', sep=";", dec=",", row.names = F)

Top.LS<-LS.results$models[[1]]
coef(Top.LS,"psi",prob = 0.95)
coef(Top.LS,"p",prob = 0.95)


Second.LS<-LS.results$models[[2]]
coef(Second.LS,"psi",prob = 0.95)
coef(Second.LS,"p",prob = 0.95)

LS_sel_model<-LS.results$models[[1]] 
summary(LS_sel_model)


# model average

LS_avg <- modAvg(
                LS.results,
                param = "psi",
                parmgrp = "real",
                index = 1:4,
                conf = 0.95,
                predict = FALSE,
                replaceNaN = TRUE
                )


#plot beta estimators
beta_LS <- coef(LS_sel_model,'psi',prob = 0.95)

plot_beta_LS <- ggplot(beta_LS, aes(x =c ('intercept','dist_PA','NDVI'), y = Beta_est)) +
                  labs(x = "Parameter", y = '\u03b2 coefficient') +
                  geom_bar(stat = 'identity') +
                  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI), width = 0.2) +
                  theme_bw()+ggtitle('livestock')

## sitecovariate response curves
covariates_LS <<- c('dist_PA','NDVI')

est_LS <- cbind(fitted(LS_sel_model,"psi", prob = 0.95),site_cov_scaled[covariates_LS])

psiLS_stats <- data.frame(psi_min= min(est_LS$Real_est), se_min = est_LS$se[which.min(est_LS$Real_est)],
                        psi_max= max(est_LS$Real_est), se_max = est_LS$se[which.max(est_LS$Real_est)],
                        rel_high = nrow(est_LS[est_LS$Real_est > 0.8,])/nrow(est_LS))


est_LS_long <- pivot_longer(data = est_LS,
                            cols = covariates_LS
                           )



plot_LS <-ggplot(est_LS_long,aes(y=Real_est,x=value))+
  geom_point()+
  geom_ribbon(aes(ymin= (Real_est-se),ymax = (Real_est+se)),alpha =.5)+
  facet_wrap(~name)+
  ylab('\u03C8 livestock')+
  ggtitle('response livestock')


# add fitted psi to grid
grid$fittedPsiLS <- fitted(LS_sel_model, "psi")[,1]


plot(grid['fittedPsiLS'])



## sitecovariate response curves model average
covariates_LS <<- c('dist_PA','NDVI')

est_LS_avg <- cbind(LS_avg[,1:2],site_cov_scaled[covariates_LS])


est_LS_avg_long <- pivot_longer(data = est_LS_avg,
                            cols = covariates_LS
)



plot_LS_avg <-ggplot(est_LS_avg_long,aes(y=est,x=value))+
  geom_point()+
  geom_ribbon(aes(ymin= (est-se),ymax = (est+se)),alpha =.5)+
  facet_wrap(~name)+
  ylab('\u03C8 livestock')+
  ggtitle('response livestock (average model)')


# add fitted psi to grid
grid$fittedPsiLS_avg <- LS_avg[,1]


plot(grid['fittedPsiLS_avg'])



##survey covariates
surv_covariates_LS <<- c('area.z','year.z')

p_LS <- coef(LS_sel_model,"p", prob = 0.95)

plot_p_LS <- ggplot(p_LS, aes(x =c('intercept','area.z','year.z'), y = Beta_est)) +
  labs(x = "Parameter", y = 'p coefficient') +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI), width = 0.2) +
  theme_bw()





#### model CF


CF.pao <- createPao(data= CF[,-1],
                    unitnames = CF[,1],
                    unitcov = site_cov_scaled,
                    survcov = surv_cov_scaled)


CF.null<-occMod(model = list(psi~1, p~1), data = CF.pao, type = "so")

CF_1a <- occMod(model = list(psi~NDVI, p~1), data = CF.pao, type = "so")
CF_1b <- occMod(model = list(psi~NDVI, p~area.z), data = CF.pao, type = "so")
CF_1c <- occMod(model = list(psi~NDVI, p~area.z+year.z), data = CF.pao, type = "so")
CF_1d <- occMod(model = list(psi~NDVI, p~area.z+year.z+summer.z), data = CF.pao, type = "so")
CF_1e <- occMod(model = list(psi~NDVI, p~year.z), data = CF.pao, type = "so")

CF_2a <- occMod(model = list(psi~dist_PA, p~1), data = CF.pao, type = "so")
CF_2b <- occMod(model = list(psi~dist_PA, p~area.z), data = CF.pao, type = "so")
CF_2c <- occMod(model = list(psi~dist_PA, p~area.z+year.z), data = CF.pao, type = "so")
CF_2d <- occMod(model = list(psi~dist_PA, p~area.z+year.z+summer.z), data = CF.pao, type = "so")
CF_2e <- occMod(model = list(psi~dist_PA, p~year.z), data = CF.pao, type = "so")


CF_3a <- occMod(model = list(psi~dist_PA+NDVI, p~1), data = CF.pao, type = "so")
CF_3b <- occMod(model = list(psi~dist_PA+NDVI, p~area.z), data = CF.pao, type = "so")
CF_3c <- occMod(model = list(psi~dist_PA+NDVI, p~area.z+year.z), data = CF.pao, type = "so")
CF_3d <- occMod(model = list(psi~dist_PA+NDVI, p~area.z+year.z+summer.z), data = CF.pao, type = "so")
CF_3e <- occMod(model = list(psi~dist_PA+NDVI, p~year.z), data = CF.pao, type = "so")

CF_4a <- occMod(model = list(psi~tri, p~1), data = CF.pao, type = "so")
CF_4b <- occMod(model = list(psi~tri, p~area.z), data = CF.pao, type = "so")
CF_4c <- occMod(model = list(psi~tri, p~area.z+year.z), data = CF.pao, type = "so")
CF_4d <- occMod(model = list(psi~tri, p~area.z+year.z+summer.z), data = CF.pao, type = "so")
CF_4e <- occMod(model = list(psi~tri, p~year.z), data = CF.pao, type = "so")

CF_5a <- occMod(model = list(psi~dist_PA+NDVI+tri, p~1), data = CF.pao, type = "so")
CF_5b <- occMod(model = list(psi~dist_PA+NDVI+tri, p~area.z), data = CF.pao, type = "so")
CF_5c <- occMod(model = list(psi~dist_PA+NDVI+tri, p~area.z+year.z), data = CF.pao, type = "so")
CF_5d <- occMod(model = list(psi~dist_PA+NDVI+tri, p~area.z+year.z+summer.z), data = CF.pao, type = "so")
CF_5e <- occMod(model = list(psi~dist_PA+NDVI+tri, p~year.z), data = CF.pao, type = "so")

CF_6a <- occMod(model = list(psi~river_mean, p~1), data = CF.pao, type = "so")
CF_6b <- occMod(model = list(psi~river_mean, p~area.z), data = CF.pao, type = "so")
CF_6c <- occMod(model = list(psi~river_mean, p~area.z+year.z), data = CF.pao, type = "so")
CF_6d <- occMod(model = list(psi~river_mean, p~area.z+year.z+summer.z), data = CF.pao, type = "so")
CF_6e <- occMod(model = list(psi~river_mean, p~year.z), data = CF.pao, type = "so")

CF_7a <- occMod(model = list(psi~roads_mean, p~1), data = CF.pao, type = "so")
CF_7b <- occMod(model = list(psi~roads_mean, p~area.z), data = CF.pao, type = "so")
CF_7c <- occMod(model = list(psi~roads_mean, p~area.z+year.z), data = CF.pao, type = "so")
CF_7d <- occMod(model = list(psi~roads_mean, p~area.z+year.z+summer.z), data = CF.pao, type = "so")
CF_7e <- occMod(model = list(psi~roads_mean, p~year.z), data = CF.pao, type = "so")

CF_8a <- occMod(model = list(psi~dist_PA+NDVI+tri+river_mean, p~1), data = CF.pao, type = "so")
CF_8b <- occMod(model = list(psi~dist_PA+NDVI+tri+river_mean, p~area.z), data = CF.pao, type = "so")
CF_8c <- occMod(model = list(psi~dist_PA+NDVI+tri+river_mean, p~area.z+year.z), data = CF.pao, type = "so")
CF_8d <- occMod(model = list(psi~dist_PA+NDVI+tri+river_mean, p~area.z+year.z+summer.z), data = CF.pao, type = "so")
CF_8e <- occMod(model = list(psi~dist_PA+NDVI+tri+river_mean, p~year.z), data = CF.pao, type = "so")

CF_9a <- occMod(model = list(psi~dist_PA+tri, p~1), data = CF.pao, type = "so")
CF_9b <- occMod(model = list(psi~dist_PA+tri, p~area.z), data = CF.pao, type = "so")
CF_9c <- occMod(model = list(psi~dist_PA+tri, p~area.z+year.z), data = CF.pao, type = "so")
CF_9d <- occMod(model = list(psi~dist_PA+tri, p~area.z+year.z+summer.z), data = CF.pao, type = "so")
CF_9e <- occMod(model = list(psi~dist_PA+tri, p~year.z), data = CF.pao, type = "so")

CF_10a<-occMod(model = list(psi~1, p~area.z), data = CF.pao, type = "so")
CF_10b<-occMod(model = list(psi~1, p~area.z+year.z), data = CF.pao, type = "so")
CF_10c<-occMod(model = list(psi~1, p~area.z+year.z+summer.z), data = CF.pao, type = "so")
CF_10d<-occMod(model = list(psi~1, p~year.z), data = CF.pao, type = "so")
CF_10e<-occMod(model = list(psi~1, p~summer.z), data = CF.pao, type = "so")



# models that do not converge are commented out
CF_models <- list(CF.null,
                  CF_1a,CF_1b,CF_1c,CF_1d,
                  CF_2a,CF_2b,
                  #CF_2c,
                  #CF_3a,
                  #CF_3b,
                  #CF_3c,
                  CF_3d,
                  #CF_3e,
                  CF_4a,CF_4b,CF_4c,CF_4d,CF_4e,
                  CF_5a,CF_5b,CF_5c,CF_5d,CF_5e,
                  CF_6a,CF_6b,CF_6c,CF_6d,CF_6e,
                  CF_7a,CF_7b,CF_7c,CF_7d,
                  #CF_7e,
                  CF_8a,
                  #CF_8b,
                  #CF_8c,
                  #CF_8d,
                  CF_8e,
                  CF_9a,CF_9b,CF_9c,CF_9d,CF_9e,
                  CF_10a,CF_10b,CF_10c,CF_10d,CF_10e
)


CF.results <- createAicTable(CF_models, use.aicc = TRUE) # Put all models and their AIC values together

summary(CF.results) # Create comparable AIC table from the above list

#write summary to csv
write.table(summary(CF.results),'cf_aic_table.csv',sep = ";", dec=",",row.names = F)

Top.CF<-CF.results$models[[1]]
coef(Top.CF,"psi")
coef(Top.CF,"p")


Second.CF<-CF.results$models[[2]]
coef(Second.CF,"psi")
coef(Second.CF,"p")


# select model 
CF_sel_Model <- CF.results$models[[1]] 

summary(CF_sel_Model)

# model average

CF_avg <- modAvg(
  CF.results,
  param = "psi",
  parmgrp = "real",
  index = 1:4,
  conf = 0.95,
  predict = FALSE,
  replaceNaN = TRUE
)

#plot beta estimators
beta_CF <- coef(CF_sel_Model,'psi',prob = 0.95)

plot_beta_CF <- ggplot(beta_CF, aes(x =c ('intercept','dist_PA','NDVI','TRI'), y = Beta_est)) +
                  labs(x = "Parameter", y = '\u03b2 coefficient') +
                  geom_bar(stat = 'identity') +
                  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI), width = 0.2) +
                  theme_bw()+ggtitle('conflict')


## response curves conflict
covariates_CF <- c('dist_PA','NDVI','tri')

est_CF <- cbind(fitted(CF_sel_Model,"psi"),site_cov_scaled[covariates_CF])


psiCF_stats <- data.frame(psi_min= min(est_CF$Real_est), se_min = est_CF$se[which.min(est_CF$Real_est)],
                          psi_max= max(est_CF$Real_est), se_max = est_CF$se[which.max(est_CF$Real_est)],
                          rel_high = nrow(est_CF[est_CF$Real_est > 0.8,])/nrow(est_CF))


est_CF_long <- pivot_longer(data = est_CF,
                            cols = covariates_CF
                            )



plot_CF <-ggplot(est_CF_long,aes(y=Real_est,x=value))+
  geom_point()+
  geom_ribbon(aes(ymin= (Real_est-se),ymax = (Real_est+se)),alpha =.5)+
  facet_wrap(~name)+
  ylab('\u03C8 conflict')+
  ggtitle('response conflict')


# add fitted psi to grid
grid$fittedPsiCF <- fitted(CF_sel_Model, "psi")[,1]
plot(grid['fittedPsiCF'])


## sitecovariate response curves model average
covariates_CF <- c('dist_PA','NDVI','tri')

est_CF_avg <- cbind(CF_avg[,1:2],site_cov_scaled[covariates_CF])


est_CF_avg_long <- pivot_longer(data = est_CF_avg,
                                cols = covariates_LS
)



plot_CF_avg <-ggplot(est_CF_avg_long,aes(y=est,x=value))+
  geom_point()+
  geom_ribbon(aes(ymin= (est-se),ymax = (est+se)),alpha =.5)+
  facet_wrap(~name)+
  ylab('\u03C8 conflict')+
  ggtitle('response conflict (average model)')


# add fitted psi to grid
grid$fittedPsiCF_avg <- LS_avg[,1]


plot(grid['fittedPsiCF_avg'])


#survey covariates
surv_covariates_CF <<- c('area.z')

p_CF <- coef(CF_sel_Model,"p",prob = 0.95)


plot_p_CF <- ggplot(p_CF, aes(x =c ('intercept','area.z'), y = Beta_est)) +
  labs(x = "Parameter", y = 'p coefficient') +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI), width = 0.2) +
  theme_bw()



# get conditional psi Conflict|livestock
grid$fittedPsiCF_LS <- fitted(CF_sel_Model, "psi")[,1] * fitted(LS_sel_model, "psi")[,1]


covariates_CF_LS <- c('dist_PA','NDVI','tri')


# 
est_CF_LS <- cbind(psiCF_LS=fitted(CF_sel_Model, "psi")[,1] * fitted(LS_sel_model,"psi")[,1],
                   se= sqrt((fitted(CF_sel_Model, "psi")[,2]/fitted(CF_sel_Model,"psi")[,1] )^2 +
                            (fitted(LS_sel_model, "psi")[,2]/(fitted(LS_sel_model,"psi")[,1])^2))*
                            (psiCF_LS=fitted(CF_sel_Model, "psi")[,1] * fitted(LS_sel_model,"psi")[,1]),
                   site_cov_scaled[covariates_CF_LS])

psiCF_LS_stats <- data.frame(psi_min= min(est_CF_LS$psiCF_LS), se_min = est_CF_LS$se[which.min(est_CF_LS$psiCF_LS)],
                          psi_max= max(est_CF_LS$psiCF_LS), se_max = est_CF_LS$se[which.max(est_CF_LS$psiCF_LS)],
                          rel_high = nrow(est_CF_LS[est_CF_LS$psiCF_LS > 0.8,])/nrow(est_CF_LS))


est_CF_LS_long <- pivot_longer(data = est_CF_LS,
                            cols = covariates_CF_LS
)



plot_CF_LS <- ggplot(est_CF_LS_long,aes(y=psiCF_LS,x=value))+
  geom_point()+
  facet_wrap(~name)+
  ylab('\u03C8 conflict|livestock')+
  ggtitle('response CF|LS')
  


ggarrange(plot_LS,plot_CF,plot_CF_LS)




# get conditional psi Conflict|livestock from model averag
grid$fittedPsiCF_LS_avg <- CF_avg[,1] * LS_avg[,1]


covariates_CF_LS_avg <- c('dist_PA','NDVI','tri')

est_CF_LS_avg <- cbind(psiCF_LS=CF_avg[,1] * LS_avg[,1],
                   site_cov_scaled[covariates_CF_LS])


est_CF_LS_avg_long <- pivot_longer(data = est_CF_LS_avg,
                               cols = covariates_CF_LS
)



plot_CF_LS_avg <- ggplot(est_CF_LS_avg_long,aes(y=psiCF_LS,x=value))+
  geom_point()+
  facet_wrap(~name)+
  ylab('\u03C8 conflict|livestock')+
  ggtitle('response CF|LS model average')




plot(grid['fittedPsiCF_LS_avg'])



ggarrange(plot_LS_avg,plot_CF_avg,plot_CF_LS_avg)

ggarrange(plot_beta_LS,plot_beta_CF,plot_p_LS,plot_p_CF)

# write grid with fitted data to disc
st_write(grid,"fitted_Psi.gpkg",layer = "grid")


#save.image(file = "single_species.RData")


