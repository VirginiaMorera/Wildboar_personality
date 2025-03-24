rm(list = ls())
source("Scripts/setup.R")

#### 0. Load and prepare data ####

## load
gps_weekly <- readRDS("Data/FINAL_gps_weekly.RDS")


## prepare 

# first, filter rows with NA in any of the used variables
gps_weekly %<>%
  ungroup() %>% 
  filter(!is.na(mean_mvmntrate_Night)) %>% 
  filter(!is.na(age_class)) %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(mean_forestCover)) %>% 
  filter(!is.na(mean_hfi)) %>% 
  filter(!is.na(NFI)) %>% 
  filter(!is.na(mean_intensity_use)) %>% 
  filter(!is.na(prop_forest_cover)) %>% 
  filter(!is.na(mean_diurnality)) %>%
  filter(mean_intensity_use != Inf) %>% 
  filter(mean_daily_tort != Inf) %>% 
  filter(!is.na(max_dist_weekly)) %>% 
  mutate(week = as.numeric(week))

# recuperate animalID from uniqueID and use it as ind random factor
gps_weekly <- gps_weekly %>% 
  separate(uniqueID, 
           into = c(NA, "animalID", NA, NA), 
           remove = FALSE)

# now check if there are any individuals with only 1 or 2 weeks tracked
wks <- gps_weekly %>% 
  group_by(uniqueID) %>% 
  summarise(n_weeks = n_distinct(week))

# remove ind with less than 3 weeks tracked
# remove an individual with irregular tracking
gps_weekly %<>% 
  group_by(uniqueID) %>% 
  filter(n() >= 3) %>% 
  ungroup() %>% 
  filter(uniqueID != "UniPrag_178_2399") 


#### 1. movement rate (activity proxy) ####

#### 1.1 check correlation between involved variables ####
corrplot::corrplot(cor(gps_weekly %>% 
                         dplyr::select("mean_forestCover", "mean_tri", 
                                       "mean_dayLength", "mean_p2ptime")), 
                   method="number", type = "lower", diag = TRUE)


#### 1.2 model, just to check effects and make sure it makes sense ####
m1 <- lmer(mean_mvmntrate ~  sex + age_class + study_areas +   
             scale(mean_forestCover) + scale(mean_forestCover^2) +
             scale(mean_forestCover)*study_areas + 
             scale(mean_tri) + scale(mean_tri^2) +
             scale(mean_dayLength) + scale(mean_dayLength^2)+
             scale(mean_p2ptime) + scale(mean_p2ptime^2) + 
             scale(week) + scale(week^2) + scale(week^3) + scale(week^4) +
             (1|animalID) + (1|year),
           data = gps_weekly)

summary(m1)

#### 1.3 evaluate model ####

# diagnostic plots
simulationOutput <- simulateResiduals(fittedModel = m1, plot = T)

# r squared
r.squaredGLMM(m1)

model_outputs <- data.frame(model = c("m1", "m2", "m3"), 
                            response = c("Movement rate", "Diurnality", 
                                         "Intensity of use"), 
                            R2m = c(r.squaredGLMM(m1)[1], NA, NA), 
                            R2c = c(r.squaredGLMM(m1)[2], NA, NA), 
                            rpt_mean = NA,
                            rpt_sd = NA, 
                            rpt_lowCI = NA, 
                            rpt_highCI = NA, 
                            rpt_pvalue = NA)

#### 1.4 covariate effects ####

m1_eff <- broom::tidy(m1)
write.csv(m1_eff, file = "Outputs/single_trait_models/m1_effects.csv")


#### 1.5  calculate repeatability with same model ####

rpt1 <- rptR::rpt(mean_mvmntrate ~  sex + age_class + study_areas +   
                    scale(mean_forestCover) + scale(mean_forestCover^2) +
                    scale(mean_tri) + scale(mean_tri^2) +
                    scale(mean_forestCover)*study_areas + 
                    scale(mean_dayLength) + scale(mean_dayLength^2)+
                    scale(mean_p2ptime) + scale(mean_p2ptime^2) + 
                    scale(week) + scale(week^2) + scale(week^3) + scale(week^4) + 
                    (1|animalID) + (1|year),
                  grname = "animalID",
          datatype = "Gaussian",
          npermut = 1000,
          nboot = 1000,
          data = gps_weekly)

# saveRDS(rpt1, "Outputs/rpt_mvmntrate.RDS")
rpt1 <- readRDS("Outputs/rpt_mvmntrate.RDS")

summary(rpt1)

model_outputs$rpt_mean[1] <- 0.349
model_outputs$rpt_sd[1] <- 0.0443
model_outputs$rpt_lowCI[1] <- 0.265
model_outputs$rpt_highCI[1] <- 0.432
model_outputs$rpt_pvalue[1] <- 0.001

# plot repeatability
pdf(file = "Outputs/single_trait_models/m1_rpt.pdf", 
    height = 7, width = 7)
plot(rpt1)
dev.off()

#### 1.6 plot random effects per individual ####

# first simulate out of our model's random effects
randomSims1 <- REsim(m1, n.sims = 1000)

# select only the simulations for the uniqueID random effect 
randomSims1 %<>% 
  filter(groupFctr == "animalID") %>% 
  dplyr::select(animalID = groupID, mean, median, sd)


# add to the simulation ds sex, age, study_area, 
id_summary <- gps_weekly %>% 
  dplyr::select(animalID, sex, age_class, study_areas) %>% 
  distinct()

randomSims1 %<>% 
  left_join(id_summary, by = "animalID") %>% 
  mutate(variable = "movement_rate") 

randomSims1 <- randomSims1 %>% 
  mutate(lower = mean-sd, 
         higher = mean+sd, 
         position = if_else(lower > 0, "above", 
                            if_else(higher < 0, "below", "overlap")))
#### 2. nocturnality (boldness proxy) ####

#### 2.1 check correlation ####
corrplot::corrplot(cor(gps_weekly %>% 
                         dplyr::select("mean_distRoads", "mean_hfi", 
                                       "mean_distPaths", "mean_tri", 
                                       "mean_dayLength", "mean_p2ptime")), 
                   method="number", type = "lower", diag = TRUE)

#### 2.2 model, just to check effects and make sure it makes sense ####

# transform diurnality into a binary variable, threshold at the median
hist(gps_weekly$mean_diurnality, breaks = 100)
abline(v = median(gps_weekly$mean_diurnality), col="red", lwd=3, lty=2)

gps_weekly <- gps_weekly %>% 
  mutate(mean_diurnality_bin = if_else(mean_diurnality > median(gps_weekly$mean_diurnality), 1, 0))

hist(gps_weekly$mean_diurnality_bin, breaks = 100)


m2 <- glmer(mean_diurnality_bin ~   
             age_class + sex + study_areas + 
             scale(mean_hfi) + scale(mean_hfi^2) +
             scale(mean_distRoads) + scale(mean_distRoads^2) +
             scale(mean_distPaths) + scale(mean_distPaths^2) +
             scale(mean_tri) + scale(mean_tri^2) +
             scale(mean_dayLength) + scale(mean_dayLength^2) +
             # scale(mean_p2ptime) + scale(mean_p2ptime^2) +
             scale(week) + scale(week^2) + 
             scale(week^3) + scale(week^4) +
             (1|animalID) + (1|year),
            family = "binomial",
            data = gps_weekly)

summary(m2)

# calc variance explained by year 

# Extract variance components
variance_components <- as.data.frame(VarCorr(m2))

# Get the variances for each random effect
var_ID <- variance_components[variance_components$grp == "animalID", "vcov"]
var_year <- variance_components[variance_components$grp == "year", "vcov"]


residual_var <- (pi^2) / 3

var_expl_year <- var_year/(var_year + var_ID + residual_var)
var_expl_year*100

#### 2.3 evaluate model ####

# diagnsotic plots
simulationOutput <- simulateResiduals(fittedModel = m2, plot = T) 

plot(simulationOutput)

# r squared
r.squaredGLMM(m2)

model_outputs$R2m[2] <- r.squaredGLMM(m2)[2,1]
model_outputs$R2c[2] <- r.squaredGLMM(m2)[2,2]


#### 2.4 covariate effects ####

m2_eff <- broom::tidy(m2)
write.csv(m2_eff, file = "Outputs/single_trait_models/m2_effects.csv")

m2_eff2 <- read.csv("Outputs/single_trait_models/m2_effects.csv")
#### 2.5 calculate repeatability with same model ####
rpt2 <- rptR::rptBinary(mean_diurnality_bin ~   
                    age_class + sex + study_areas + 
                    scale(mean_hfi) + scale(mean_hfi^2) +
                    scale(mean_distRoads) + scale(mean_distRoads^2) +
                    scale(mean_distPaths) + scale(mean_distPaths^2) +
                    scale(mean_tri) + scale(mean_tri^2) +
                    scale(mean_dayLength) + scale(mean_dayLength^2) +
                    # scale(mean_p2ptime) + scale(mean_p2ptime^2) +
                    scale(week) + scale(week^2) +
                    scale(week^3) + scale(week^4) +
                    (1|animalID) + (1|year),
                  grname = "animalID",
                  # link = "logit",
                  npermut = 1000,
                  nboot = 1000,
                  data = gps_weekly)


# saveRDS(rpt2, "Outputs/rpt_diurnality.RDS")
rpt2 <- readRDS("Outputs/rpt_diurnality.RDS")

summary(rpt2)

model_outputs$rpt_mean[2] <- 0.157
model_outputs$rpt_sd[2] <- 0.0380
model_outputs$rpt_lowCI[2] <- 0.0611
model_outputs$rpt_highCI[2] <- 0.211
model_outputs$rpt_pvalue[2] <- 0.001

# plot repeatability

plot(rpt2)

#### 2.6 plot random effects per individual ####

# first simulate out of our model's random effects
randomSims2 <- REsim(m2, n.sims = 1000, oddsRatio = F)

# select only the simulations for the uniqueID random effect 
randomSims2 %<>% 
  filter(groupFctr == "animalID") %>% 
  dplyr::select(animalID = groupID, mean, median, sd)


# add to the simulation ds sex, age, study_area, 
id_summary <- gps_weekly %>% 
  dplyr::select(animalID, sex, age_class, study_areas) %>% 
  distinct()

randomSims2 %<>% 
  left_join(id_summary, by = "animalID") %>% 
  mutate(variable = "diurnality")


randomSims2 <- randomSims2 %>% 
  mutate(lower = mean-sd, 
         higher = mean+sd, 
         position = if_else(lower > 0, "above", 
                            if_else(higher < 0, "below", "overlap")))

#### 3. Intensity of use (proxy for exploration) ####

#### 3.1 check correlation ####

corrplot::corrplot(cor(gps_weekly %>% 
                         dplyr::select("mean_forestCover", "mean_tri", 
                                       "mean_dayLength", "mean_p2ptime")), 
                   method="number", type = "lower", diag = TRUE)



#### 3.2 model, just to check effects and make sure it makes sense ####

# remove two outliers over 15
# log transform
gps_weekly <- gps_weekly %>% 
  filter(mean_intensity_use < 15) %>% 
  mutate(mean_intensity_use_trans = log(mean_intensity_use+1))

m3 <- lmer(mean_intensity_use_trans ~  sex + age_class + study_areas +   
             scale(mean_forestCover) + scale(mean_forestCover^2) +
             scale(mean_tri) + scale(mean_tri^2) +
             scale(mean_dayLength) + scale(mean_dayLength^2)+
             scale(mean_p2ptime) + scale(mean_p2ptime^2) + 
             scale(week) + scale(week^2) + scale(week^3) + scale(week^4) +
             (1|animalID) + (1|year),
           data = gps_weekly)

summary(m3)

vars <- as.data.frame(VarCorr(m3))

var_year <- vars[2,4]/(vars[1,4] + vars[2,4] + vars[3,4])
var_year*100

#### 3.3 evaluate model ####

# diagnsotic plots
simulationOutput <- simulateResiduals(fittedModel = m3, plot = T) 

plot(simulationOutput)

# r squared
r.squaredGLMM(m3)

model_outputs$R2m[3] <- r.squaredGLMM(m3)[1]
model_outputs$R2c[3] <- r.squaredGLMM(m3)[2]

#### 3.4 Covariate effects ####

m3_eff <- broom::tidy(m3)
write.csv(m3_eff, file = "Outputs/single_trait_models/m3_effects.csv")

#### 3.5 calculate repeatability with same model ####
rpt3 <- rptR::rpt(log(mean_intensity_use+1) ~  sex + age_class + study_areas +   
                    scale(mean_forestCover) + scale(mean_forestCover^2) +
                    scale(mean_tri) + scale(mean_tri^2) +
                    scale(mean_dayLength) + scale(mean_dayLength^2)+
                    scale(mean_p2ptime) + scale(mean_p2ptime^2) + 
                    scale(week) + scale(week^2) + scale(week^3) + scale(week^4) +
                    (1|animalID) + (1|year),
                  grname = "animalID",
                  datatype = "Gaussian",
                  npermut = 1000,
                  nboot = 1000,
                  data = gps_weekly)

# saveRDS(rpt3, "Outputs/rpt_intensity.RDS")
rpt3 <- readRDS("Outputs/rpt_intensity.RDS")

summary(rpt3)

model_outputs$rpt_mean[3] <- 0.203
model_outputs$rpt_sd[3] <- 0.0342
model_outputs$rpt_lowCI[3] <- 0.136
model_outputs$rpt_highCI[3] <- 0.27
model_outputs$rpt_pvalue[3] <- 0.001

write.csv(model_outputs, file = "Outputs/model_outputs.csv", row.names = F)

# plot repeatability

plot(rpt3)

#### 3.6 plot random effects per individual ####

# first simulate out of our model's random effects
randomSims3 <- REsim(m3, n.sims = 1000)

# select only the simulations for the uniqueID random effect 
randomSims3 %<>% 
  filter(groupFctr == "animalID") %>% 
  dplyr::select(animalID = groupID, mean, median, sd)


# add to the simulation ds sex, age, study_area, 
id_summary <- gps_weekly %>% 
  dplyr::select(animalID, sex, age_class, study_areas) %>% 
  distinct()

randomSims3 %<>% 
  left_join(id_summary, by = "animalID") %>% 
  mutate(variable = "intensity_use")

randomSims3 <- randomSims3 %>% 
  mutate(lower = mean-sd, 
         higher = mean+sd, 
         position = if_else(lower > 0, "above", 
                            if_else(higher < 0, "below", "overlap")))# fe <- fixef(m3)
# 
# randomSims3 <- randomSims3 %>% 
#   mutate(
#     mean = if_else(
#       sex == "f" & age_class == 2 & study_areas == "doupov", 
#       mean + fe["(Intercept)"], if_else(
#         sex == "f" & age_class == 3 & study_areas == "doupov", 
#         mean + fe["(Intercept)"] + fe["age_class3"], if_else(
#           sex == "f" & age_class == 4 & study_areas == "doupov", 
#           mean + fe["(Intercept)"] + fe["age_class4"], if_else(
#             
#             sex == "m" & age_class == 2 & study_areas == "doupov", 
#             mean + fe["(Intercept)"] + fe["sexm"], if_else(
#               sex == "m" & age_class == 3 & study_areas == "doupov", 
#               mean + fe["(Intercept)"] + fe["sexm"] + fe["age_class3"], if_else(
#                 sex == "m" & age_class == 4 & study_areas == "doupov", 
#                 mean + fe["(Intercept)"] + fe["sexm"] + fe["age_class4"], if_else(
#                   
#                   sex == "f" & age_class == 2 & study_areas == "kostelec", 
#                   mean + fe["(Intercept)"] + fe["study_areaskostelec"], if_else(
#                     sex == "f" & age_class == 3 & study_areas == "kostelec", 
#                     mean + fe["(Intercept)"] + fe["age_class3"] + fe["study_areaskostelec"], if_else(
#                       sex == "f" & age_class == 4 & study_areas == "kostelec", 
#                       mean + fe["(Intercept)"] + fe["age_class4"] + fe["study_areaskostelec"], if_else(
#                         
#                         sex == "m" & age_class == 2 & study_areas == "kostelec", 
#                         mean + fe["(Intercept)"] + fe["sexm"] + fe["study_areaskostelec"], if_else(
#                           sex == "m" & age_class == 3 & study_areas == "kostelec", 
#                           mean + fe["(Intercept)"] + fe["sexm"] + fe["age_class3"] + fe["study_areaskostelec"], if_else(
#                             sex == "m" & age_class == 4 & study_areas == "kostelec", 
#                             mean + fe["(Intercept)"] + fe["sexm"] + fe["age_class4"] + fe["study_areaskostelec"], mean
#                           )))))))))))))


