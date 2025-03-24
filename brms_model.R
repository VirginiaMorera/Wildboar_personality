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
# add transformations that we've performed for the rpt models 
gps_weekly %<>% 
  group_by(uniqueID) %>% 
  filter(n() >= 3) %>% 
  ungroup() %>% 
  filter(uniqueID != "UniPrag_178_2399") %>% 
  filter(mean_intensity_use < 15) %>% 
  mutate(mean_diurnality_bin = if_else(mean_diurnality > -0.7262, 1, 0), 
         mean_intensity_use_trans = log(mean_intensity_use+1))


# set up cores for brms model
my.cores <- detectCores()-2


#### 1. Run brms model with the three repeatable traits ####
m1_brm <- brm(mvbind(scale(mean_mvmntrate), 
                     mean_diurnality_bin, 
                     scale(mean_intensity_use_trans)) ~
                sex + age_class + study_areas +
                scale(mean_forestCover^2) + scale(mean_forestCover) + 
                scale(mean_tri^2) + scale(mean_tri) + 
                scale(mean_distRoads^2) + scale(mean_distRoads) +
                scale(mean_dayLength) + scale(mean_dayLength^2)+
                scale(mean_p2ptime) + scale(mean_p2ptime^2) + 
                scale(week) + scale(week^2) + scale(week^3) + scale(week^4) + 
                (1|p|uniqueID),
              data = gps_weekly, family = c(gaussian, bernoulli, gaussian),
              warmup = 1500,iter = 5000, thin = 2,
              chains = 2, init = "random",
              seed = 123,
              cores = 50, 
              control = list(max_treedepth = 20))

# this has ben run in the Rstudio server and taken only a few minutes

m1_brm <- readRDS("Outputs/final_brm_model.RDS")

summary(m1_brm)

fef <- fixef(m1_brm)
write.csv(fef, file = "Outputs/brm_fef.csv")

#### 2. eval checks ####
pp1 <- pp_check(m1_brm, resp = "scalemeanmvmntrate") + 
  ggtitle("Movement rate")  +
  theme_bw()

pp2 <- pp_check(m1_brm, resp = "scalemeanintensityusetrans") + 
  ggtitle("Intensity of use") + 
  theme_bw()

pp3 <- pp_check(m1_brm, resp = "meandiurnalitybin")  + 
  ggtitle("Diurnality") + 
  theme_bw()

bayes_R2(m1_brm)


#### 3. correlations ####

colnames(posterior_samples(m1_brm))[1:63]

Mvmnt_diur <- data.frame(
  pair = "Movement rate - Diurnality", 
  posterior = posterior_samples(m1_brm, pars =  )[,61]
)

colnames(posterior_samples(m1_brm))[62]

Mvmnt_int <- data.frame(
  pair = "Movement rate - Intensity of use", 
  posterior = posterior_samples(m1_brm, pars =  )[,62]
)

colnames(posterior_samples(m1_brm))[63]

Int_diur <- data.frame(
  pair = "Intensity of use - Diurnality", 
  posterior = posterior_samples(m1_brm, pars =  )[,63]
)

sd_mvmnt <- data.frame(
  pair = "Movement rate sd", 
  posterior = posterior_samples(m1_brm, pars =  )[,58]
)

sd_diur <- data.frame(
  pair = "Diurnality sd", 
  posterior = posterior_samples(m1_brm, pars =  )[,59]
)

sd_int <- data.frame(
  pair = "Intensity of use sd", 
  posterior = posterior_samples(m1_brm, pars =  )[,60]
)

all_corr_posteriors <- bind_rows(Mvmnt_diur, Mvmnt_int, Int_diur) %>% 
  filter(pair %in% c("Intensity of use - Diurnality", "Movement rate - Diurnality", "Movement rate - Intensity of use")) %>% 
  mutate(
    pair = as.factor(pair), 
    pair = factor(pair, levels = c("Movement rate - Intensity of use", "Movement rate - Diurnality", "Intensity of use - Diurnality"))
  )

posterior_means <- all_corr_posteriors %>% 
  group_by(pair) %>% 
  summarise(median = median(posterior), 
            low = HPDinterval(as.mcmc(posterior))[1], 
            high = HPDinterval(as.mcmc(posterior))[2])


#### 3. plotting effects ####
# equivalent to plot(allEffects())
conditional_effects(m1_brm, "mean_forestCover", resp = "scalemeanmvmntrate")


#### 4. Extract BLUPS ####
# we can plot the among individual correlation of behaviors. For this we need to
# extract the best linear unbiased prediction (BLUP) of each behaviour and 
# calculate the slope between behaviors.

# obtain `posterior sample
ps_m1 <- posterior_samples(m1_brm, 
                           pars = "^r_")

# reorganise the output of posterior sample for plotting
BLUP <- data_frame(Trait = names(ps_m1),
                   Value = colMeans(ps_m1)) %>%
  separate(Trait, 
           into = c("Trait", "animalID"),
           sep = "\\[", fill = "right") %>%
  separate(Trait, 
           into = c(NA, NA, NA, "Trait"), 
           sep = "_") %>% 
  mutate(Trait = str_replace_all(Trait, pattern = "scale", 
                                 replacement = "")) %>% 
  mutate(animalID = str_replace(animalID, pattern = ",Intercept]", 
                                replacement = "")) %>% 
  spread(Trait, Value) %>% 
  dplyr::select(animalID, diurnality = meandiurnalitybin, 
                intensity_use = meanintensityusetrans, 
                movement_rate = meanmvmntrate)

write.csv(BLUP, file = "Outputs/BLUP_FINAL.csv", row.names = F)


