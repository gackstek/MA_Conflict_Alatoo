library(RPresence)
library(gplots)
library(sf)
library(sp)
#library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(ggmap)    # for fortifying shapefiles
library(gridExtra) # for creating panels
library(ggpubr)
library(data.table)
#library(gganimate)
register_google(key = "AIzaSyCTC-AJ9xgM_QuLQxwGZ2SL1kLFCfZrJM8", write = TRUE)
#setwd("C:/Users/koust/Snow Leopard Trust Dropbox/Koustubh Sharma/Papers in Prep/Kristina Conflict alatoo/adjusted grids")
setwd("~/Desktop/MA/code/adjust grid to new study area/adjusted grids/")
Kristina_LS<-read.csv("Livestock_Det_clean.csv")[,-1]
Kristina_CF<-read.csv("Conflict_Det_clean.csv")[,-1]
Kristina_Sitecov<-read.csv("GridCovariates_clean.csv")[,-1]
Kristina_Surv_cov_Year<-read.csv("Surv_cov_Year_clean.csv")[,-1]
Kristina_SurvCov_Summer<-read.csv("Surv_cov_Summer_clean.csv")[,-1]
Kristina_Surv_cov_Area_covered<-read.csv("Surv_cov_Area_covered_clean.csv")[,-1]

names(Kristina_Surv_cov_Year)
SurvCov_year.long<-reshape(Kristina_Surv_cov_Year,
                             varying = list(colnames(Kristina_Surv_cov_Year)[c(2:10)]),
                             direction = "long")

SurvCov_Summer.long<-reshape(Kristina_SurvCov_Summer,
                           varying = list(colnames(Kristina_SurvCov_Summer)[c(2:10)]),
                           direction = "long")

SurvCov_Area.long<-reshape(Kristina_Surv_cov_Area_covered,
                             varying = list(colnames(Kristina_Surv_cov_Area_covered)[c(2:10)]),
                             direction = "long")
names(SurvCov_year.long)
nrow(SurvCov_year.long)
names(SurvCov_Summer.long)
names(SurvCov_Area.long)
Kristina_Survcovs<-cbind(SurvCov_year.long, SurvCov_Summer.long,SurvCov_Area.long)
names(Kristina_Survcovs)
Kristina_Survcovs<-Kristina_Survcovs[,c(1,3,7,11)]
head(Kristina_Survcovs)

head(Kristina_Sitecov)
head(Kristina_Survcovs)
Kristina_Sitecov$river_mean.z<-scale(Kristina_Sitecov$river_mean)
Kristina_Sitecov$roads_mean.z<-scale(Kristina_Sitecov$roads_mean)
Kristina_Sitecov$dist_PA.z<-scale(Kristina_Sitecov$dist_PA)
Kristina_Sitecov$NDVI.z<-scale(Kristina_Sitecov$NDVI)
Kristina_Sitecov$Slope.z<-scale(Kristina_Sitecov$Slope)
Kristina_Sitecov$elev_k.z<-scale(Kristina_Sitecov$elev_k)
Kristina_Sitecov$tri.z<-scale(Kristina_Sitecov$tri)
Kristina_Survcovs$year.z<-scale(Kristina_Survcovs$SurvCov_year1)
Kristina_Survcovs$summ.z<-scale(Kristina_Survcovs$SurvCov_summ1)
Kristina_Survcovs$area.z<-scale(Kristina_Survcovs$SurvCov_Area1)


# Identify and enlist the candidate model set for site covariates
# LS ~ TRI
# LS ~ TRI + River + NDVI
# LS ~ TRI + PA
# Conflict ~ XXXXX 
# Conflict ~ XXXXX
# Conflict ~ XXXXX
# Conflict ~ XXXXX
# Run these models
# Add survey covariates on the top models
# Run Species co-occurrence models on the top models

cor(Kristina_Sitecov[,(10:18)])
head(Kristina_LS)
colnames(Kristina_LS)[2:10]<-as.character(c(1:9))
colnames(Kristina_CF)[2:10]<-c(1:9)

nrow(Kristina_LS)
nrow(Kristina_CF)

Kristina_LS.CF<-rbind(Kristina_LS,Kristina_CF)
head(Kristina_LS.CF)
nrow(Kristina_LS.CF)

# Transform from stacked to compressed format
nsites=nrow(Kristina_LS.CF)/2
head(Kristina_LS.CF)
head(Kristina_Survcovs[,5:7])

nrow(Kristina_LS.CF)

Kristina_LS.CF.compress=Kristina_LS.CF[1:nsites,(2:10)]+2*Kristina_LS.CF[nsites+1:nsites,(2:10)]
head(Kristina_LS.CF.compress)
nrow(Kristina_LS.CF.compress)*ncol(Kristina_LS.CF.compress)

head(Kristina_Sitecov)
nrow(Kristina_Sitecov)

nrow(Kristina_Survcovs)
head(Kristina_LS.CF)
head(Kristina_LS.CF.compress)
nrow(Kristina_LS.CF.compress)


Kristina_LS.CF.pao<-createPao(data= Kristina_LS.CF.compress, unitcov = Kristina_Sitecov, 
                              survcov = Kristina_Survcovs) # include covariates
str(Kristina_LS.CF.pao$det.data)
str(Kristina_LS.CF.pao$unitcov)

# Fit two species, static occupancy (single season)
# Static occupancy, 2 species, "psiBA" param	occMod_2SP	SURVEY,SP,INT,INT_o,INT_d,SEASON
# si.cov: A data frame containing the unit-specific covariates to use for the occupancy component of the model.
# p: The right-hand side of the formula for the model to fit for detection probability. 
# The terms SP, INT_o and INT_d can be used to define a species effect on detection, a detection-level interaction where the occurrence of one 
# species changes the detection probability of the other species and a second detection-level interaction where the detection of one species changes the detection probability of the other species in the same survey. 
# These terms do not have to be defined as variables in p.cov.
# SP species effect on occupancy (psiBA, psiBa != psiA)
# INT interaction effect on occupancy of species A (psiBa != psiBA)

LS.CF.null<-occMod(model = list(psi~1, p~1), data = Kristina_LS.CF.pao, type = "so.2sp.1", param = "psiBA")

##    occupancy: species-specific, no interaction, parameters: psiA, psiBA=psiBa
##    detection: Constant
LS_CF.2<-occMod(model = list(psi~SP, p~1), data = Kristina_LS.CF.pao, type = "so.2sp.1", param = "psiBA")

LS_CF.2a<-occMod(model = list(psi~1, p~area.z), data = Kristina_LS.CF.pao, type = "so.2sp.1", param = "psiBA") #test covariates
##new
LS_CF.2a<-occMod(model = list(psi~1, p~year.z), data = Kristina_LS.CF.pao, type = "so.2sp.1", param = "psiBA") #test covariates
LS_CF.2a<-occMod(model = list(psi~1, p~summ.z), data = Kristina_LS.CF.pao, type = "so.2sp.1", param = "psiBA") #test covariates

#test two best performing models together



##    occupancy: species-specific, interaction, parameters: psiA, psiBA, psiBa
##    detection: species-specific, p=r
LS_CF.3<-occMod(model=list(psi~SP+INT,p~SP),data=Kristina_LS.CF.pao,type="so.2sp.1",param="psiBA")

##    occupancy: species-specific, interaction, parameters: psiA, psiBA, psiBa
##    detection: species-specific, p=r
LS_CF.4<-occMod(model=list(psi~SP,p~SP),data=Kristina_LS.CF.pao,type="so.2sp.1",param="psiBA")

##    occupancy: species-specific, interaction, parameters: psiA, psiBA, psiBa. 
##    detection: species-specific, p=r
LS_CF.4<-occMod(model=list(psi~SP+INT+dist_PA.z,p~SP),data=Kristina_LS.CF.pao,type="so.2sp.1",param="psiBA")

##    occupancy: species-specific, interaction, parameters: psiA, psiBA, psiBa. 
##    detection: species-specific, p=r
LS_CF.5<-occMod(model=list(psi~SP+INT+Slope.z,p~SP),data=Kristina_LS.CF.pao,type="so.2sp.1",param="psiBA")


##    occupancy: species-specific, interaction, parameters: psiA, psiBA, psiBa. 
##    detection: species-specific, p=r
LS_CF.6<-occMod(model=list(psi~SP+INT+Slope.z+dist_PA.z,p~SP),data=Kristina_LS.CF.pao,type="so.2sp.1",param="psiBA")

LS_CF.7<-occMod(model=list(psi~SP+INT+NDVI.z+dist_PA.z,p~SP),data=Kristina_LS.CF.pao,type="so.2sp.1",param="psiBA")

LS_CF.8<-occMod(model=list(psi~SP+INT+NDVI.z,p~SP),data=Kristina_LS.CF.pao,type="so.2sp.1",param="psiBA")

LS_CF.9<-occMod(model=list(psi~SP+INT+tri.z,p~SP),data=Kristina_LS.CF.pao,type="so.2sp.1",param="psiBA")

LS_CF.10<-occMod(model=list(psi~SP+INT+tri.z+dist_PA.z,p~SP),data=Kristina_LS.CF.pao,type="so.2sp.1",param="psiBA")

LS_CF.11<-occMod(model=list(psi~SP+INT+tri.z+dist_PA.z+NDVI.z,p~SP),data=Kristina_LS.CF.pao,type="so.2sp.1",param="psiBA")

names(Kristina_LS.CF.pao$unitcov)

LS.CF.models<-list(LS_CF.3,LS_CF.4, LS_CF.5, LS_CF.6, LS_CF.7,LS_CF.8,LS_CF.9,LS_CF.10,LS_CF.11) # Bring together all models run so far
LS.CF.results<-createAicTable(LS.CF.models) # Put all models and their AIC values together

summary(LS.CF.results) # Create comparable AIC table from the above list

Top.LS.CF<-LS.CF.results$models[[1]]
coef(Top.LS.CF,"psi")
coef(Top.LS.CF,"p")

Second.LS.CF<-LS.CF.results$models[[2]]
coef(Second.LS.CF,"psi")
coef(Second.LS.CF,"p")


names(Kristina_Sitecov)
LS_Null<-occMod(model = list(psi~1, p~1), data = Kristina_LS.pao, type = "so")
LS_river<-occMod(model = list(psi~river_mean.z, p~1), data = Kristina_LS.pao, type = "so")
LS_roads<-occMod(model = list(psi~roads_mean.z, p~1), data = Kristina_LS.pao, type = "so")
LS_dist_PA<-occMod(model = list(psi~dist_PA.z, p~1), data = Kristina_LS.pao, type = "so")
LS_NDVI<-occMod(model = list(psi~NDVI.z, p~1), data = Kristina_LS.pao, type = "so")
#LS_Slope<-occMod(model = list(psi~Slope.z, p~1), data = Kristina_LS.pao, type = "so")
#LS_elev_k<-occMod(model = list(psi~elev_k.z, p~1), data = Kristina_LS.pao, type = "so")
LS_tri.z<-occMod(model = list(psi~tri.z, p~1), data = Kristina_LS.pao, type = "so")
LS_1<-occMod(model = list(psi~NDVI.z+dist_PA.z, p~1), data = Kristina_LS.pao, type = "so")
LS_2 <- occMod(model = list(psi~tri.z+river_mean.z+NDVI.z,
                            p~1), data = Kristina_LS.pao, type = "so")
LS_3 <- occMod(model = list(psi~tri.z+dist_PA.z,
                            p~1), data = Kristina_LS.pao, type = "so")
LS_4 <- occMod(model = list(psi~NDVI.z+roads_mean.z,
                            p~1), data = Kristina_LS.pao, type = "so")
LS_5 <- occMod(model = list(psi~tri.z+river_mean.z,
                            p~1), data = Kristina_LS.pao, type = "so")
LS_6 <- occMod(model = list(psi~tri.z+NDVI.z,
                            p~1), data = Kristina_LS.pao, type = "so")
LS_7 <- occMod(model = list(psi~river_mean.z+NDVI.z,
                            p~1), data = Kristina_LS.pao, type = "so")
LS_8 <- occMod(model = list(psi~dist_PA.z+elev_k.z,
                            p~1), data = Kristina_LS.pao, type = "so")

LS.models<-list(LS_Null, LS_river, LS_roads, LS_dist_PA, LS_NDVI,
                LS_tri.z, LS_1, LS_2, LS_3, LS_4, LS_5, LS_6, LS_7,
                LS_8) # Bring together all models run so far
LS.results<-createAicTable(LS.models) # Put all models and their AIC values together

summary(LS.results) # Create comparable AIC table from the above list
top.LS.model<-LS.results$models[[1]]
top.LS.model$modname

coef(top.LS.model, "psi")
coef(top.LS.model, "p")
LS.Psi<-top.LS.model$real$psi #LS Occ
unique(LS.Psi)
