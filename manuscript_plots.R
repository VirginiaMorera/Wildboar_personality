## Figure 1 ####

# study area map

eur <- ne_countries(continent = "Europe", returnclass = "sf", scale = "large")

cz <- ne_countries(country = "Czechia", returnclass = "sf")


gps_locs <- gps_data_clean %>% 
  filter(study_areas %in% c("kostelec", "doupov")) %>% 
  dplyr::select(study_areas) %>% 
  mutate(study_areas = if_else(study_areas == "kostelec", "Kostelec", "Doupov"))

eur <- eur %>% 
  mutate(country_name = if_else(name == "Czechia", "Czechia", NA_character_))

(map1 <- ggplot(eur) + 
    geom_sf(aes(fill = country_name)) +
    scale_color_discrete(na.value = 'lightgray') +
    coord_sf(xlim = c(-10, +60), ylim = c(35, 70)) + 
    theme_minimal_grid() + 
    theme(legend.position = "none"))


map2 <- ggplot(cz) + 
  geom_sf(fill = "gray80", alpha = 0.5) + 
  geom_sf(data = gps_locs, aes(col = study_areas), size = 0.5) + 
  labs(col = "Study area") +
  theme_bw() + 
  guides(color = guide_legend(override.aes = list(size = 3)))


png("map.png", width = 10, height = 10, units = "in", res = 1200)
print(map1)
dev.off()

png("map2.png", width = 5, height = 3, units = "in", res = 300)
print(map2)
dev.off()


## Figure 2 ####

# histogram of model responses 

for_hist_w <- gps_weekly %>% 
  dplyr::select(mean_mvmntrate, mean_diurnality, mean_intensity_use)

for_hist <- for_hist_w %>% 
  pivot_longer(cols = 1:3, names_to = "Variable", values_to = "value") %>% 
  mutate(Variable = recode(Variable, mean_mvmntrate = "Movement rate", 
                           mean_diurnality = "Diurnality", 
                           mean_intensity_use = "Intensity of use"), 
         Variable = factor(Variable, levels = c("Movement rate", "Intensity of use", "Diurnality")))

for_hist_med <- for_hist %>% 
  group_by(Variable) %>% 
  summarise(mean = mean(value), 
            sd = sd(value))

hist <- ggplot(for_hist) + 
  geom_histogram(aes(x = value, fill = Variable), bins = 25) + 
  scale_fill_tableau() +
  geom_vline(data = for_hist_med, aes(xintercept = med)) +
  facet_wrap(~Variable, scales = "free") + 
  labs(x = "Weekly mean value", y = "Frequency") + 
  theme_bw() + 
  theme(legend.position="bottom")


pdf("var_hists.pdf", width = 16*0.5, height = 10*0.5)
print(hist)
dev.off()


## Figure 3 ####

# lmm random effect plots

p1 <- ggplot(randomSims1, aes(x = reorder(animalID, mean))) +
  geom_errorbar(aes(ymin = mean-sd,
                    ymax = mean+sd)) +
  geom_point(aes(y = mean, col = mean), size = 2) +
  scale_color_viridis_c(option = "D") +
  geom_hline(aes(yintercept = median(mean))) + 
  labs(y = "Movement rate random effect", col = "Mean effect") + 
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    # axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

p2 <- ggplot(randomSims2, aes(x = reorder(animalID, mean))) +
  geom_errorbar(aes(ymin = mean-sd,
                    ymax = mean+sd)) +
  geom_point(aes(y = mean, col = mean), size = 2) +
  scale_color_viridis_c(option = "D") +
  geom_hline(aes(yintercept = median(mean))) + 
  labs(y = "Intensity of use rndom effect", col = "Mean effect") + 
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    # axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )


p3<- ggplot(randomSims3, aes(x = reorder(animalID, mean))) +
  geom_errorbar(aes(ymin = mean-sd,
                    ymax = mean+sd)) +
  geom_point(aes(y = mean, col = mean), size = 2) +
  scale_color_viridis_c(option = "D") +
  geom_hline(aes(yintercept = median(mean))) + 
  labs(y = "Diurnality random effect", col = "Mean effect") + 
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    # axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

pdf("Outputs/randomEffects.pdf", width = 13, height = 9)
gridExtra::grid.arrange(p1, p2, p3, nrow = 3)
dev.off()

## Figure 4 ####

# brms correlation plots  

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
  summarise(median = median(posterior)) %>% 
  mutate(
    pair = as.factor(pair), 
    pair = factor(pair, levels = c("Movement rate - Intensity of use", "Movement rate - Diurnality", "Intensity of use - Diurnality"))
  )

lowerHDP <- all_corr_posteriors %>% 
  group_by(pair) %>% 
  summarise(low = HPDinterval(as.mcmc(posterior))[1]) %>% 
  mutate(
    pair = as.factor(pair), 
    pair = factor(pair, levels = c("Movement rate - Intensity of use", "Movement rate - Diurnality", "Intensity of use - Diurnality"))
  )

higherHDP <- all_corr_posteriors %>% 
  group_by(pair) %>% 
  summarise(high = HPDinterval(as.mcmc(posterior))[2]) %>% 
  mutate(
    pair = as.factor(pair), 
    pair = factor(pair, levels = c("Movement rate - Intensity of use", "Movement rate - Diurnality", "Intensity of use - Diurnality"))
  )

HDP <- full_join(lowerHDP, higherHDP) %>% 
  mutate(
    pair = as.factor(pair), 
    pair = factor(pair, levels = c("Movement rate - Intensity of use", "Movement rate - Diurnality", "Intensity of use - Diurnality"))
  )


cplot <- ggplot(all_corr_posteriors) +
  geom_rect(data = HDP, ymin=0, ymax=Inf,
            aes(xmin = low, xmax = high), fill = "lightgray") +
  xlim(-1, 1) +
  ylim(0, 4.5) + 
  geom_vline(xintercept = 0, lty = 2, col = "gray") +
  geom_vline(data = posterior_means, aes(xintercept = median), col = "red") + 
  geom_vline(data = lowerHDP, aes(xintercept = low), col = "gray") + 
  geom_vline(data = higherHDP, aes(xintercept = high), col = "gray") + 
  geom_density(aes(x = posterior), lwd = 0.8) + 
  facet_wrap(~pair) + 
  theme_bw() + 
  labs(x = "Posterior sample", y = "Density")

pdf("Outputs/corr_posteriors.pdf", height = 6, width = 10)
print(cplot)
dev.off()

## Figure 5 ####

# Blup plots
# BLUP <- read.csv("Outputs/BLUP_FINAL.csv")


# animal metadata
animals <- gps_weekly %>% 
  ungroup() %>% 
  dplyr::select(animalID, sex, age_class, study_areas) %>% 
  distinct()


# merge with BLUPs dataset
BLUP3 <- BLUP %>%
  # dplyr::select(animalID, diurnality, intensity_use, movement_rate) %>%
  mutate(animalID = as.character(animalID)) %>% 
  left_join(animals, by = "animalID") 




# 1 intensity vs. movement rate
(p1 <- ggplot(BLUP3,
              aes(y = intensity_use, x = movement_rate)) + 
   geom_point(aes(col = diurnality), size = 2) +
   # geom_point() +
   scale_colour_viridis_c() +
   stat_smooth(method = "lm",
               formula = y ~ x,
               geom = "smooth") +
   theme_bw() + 
   # facet_wrap(~death) + 
   labs(y = "Intensity of use (scaled)", 
        x = "Movement rate (scaled)", 
        col = "Diurnality (scaled)") + 
   NULL)


# 2 intensity vs diurnality
(p2 <- ggplot(BLUP3, 
              # %>% filter(death %in% c("alive", "drive hunt", "individual hunt")),
              aes(x = movement_rate, y = diurnality)) + 
    geom_point(aes(col = intensity_use), size = 2) +
    # geom_point() + 
    scale_colour_viridis_c() +
    stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth") +
    theme_bw() + 
    # facet_wrap(~death) + 
    labs(x = "Movement rate (scaled)", 
         y = "Diurnality (scaled)", 
         col = "Intensity of use (scaled)") + 
    NULL)


# 3 movement rate vs diurnality
(p3 <- ggplot(BLUP3, 
              # %>% filter(death %in% c("alive", "drive hunt", "individual hunt")),
              aes(x = intensity_use, y = diurnality)) + 
    geom_point(aes(col = movement_rate), size = 2) +
    # geom_point() + 
    scale_colour_viridis_c() +
    stat_smooth(method = "lm",
                formula = y ~ x,
                geom = "smooth") +
    theme_bw() + 
    # facet_wrap(~death) + 
    labs(x = "Intensity of use (scaled)", 
         y = "Diurnality (scaled)", 
         col = "Movement rate (scaled)"))

pdf("Outputs/BLUPS.pdf", width = 16*0.8, height = 10*0.8)
cowplot::plot_grid(p1, p2, p3, ncol = 2)
dev.off()



## Figure S1 ####

# m1 fixed effects

p1 <- plot(Effect("sex", m1), 
           main = "Sex", xlab = "Sex", ylab = "Mean movement rate") 
p2 <- plot(Effect("age_class", m1), 
           main = "Age class", 
           xlab = "Age class", ylab = "Mean movement rate") 
p3 <- plot(Effect("study_areas", m1), 
           main = "Study area", 
           xlab = "Study area", ylab = "Mean movement rate")
p4 <- plot(Effect("mean_forestCover", m1), 
           main = "Corest cover", 
           xlab = "Vorest cover", ylab = "Mean movement rate")
p5 <- plot(Effect("mean_tri", m1), 
           main = "Terrain ruggedness index", 
           xlab = "Terrain ruggedness index", ylab = "Mean movement rate")
p6 <- plot(Effect("mean_dayLength", m1), 
           main = "Day length", 
           xlab = "Day length", ylab = "Mean movement rate")
p7 <- plot(Effect("mean_p2ptime", m1), 
           main = "Time between fixes", 
           xlab = "Time between fixes", ylab = "Mean movement rate")
p8 <- plot(Effect("week", m1), 
           main = "Week", xlab = "Week", ylab = "Mean movement rate")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 3)

pdf(file = "Outputs/single_trait_models/m1_effects.pdf", 
    height = 10, width = 10*1.5)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 3)
dev.off()

## Figure S2 ####

# m3 fixed effects
p1 <- plot(Effect("sex", m3), 
           main = "Sex", xlab = "Sex", ylab = "Mean intensity of use") 
p2 <- plot(Effect("age_class", m3), 
           main = "Age class", 
           xlab = "Age class", ylab = "Mean intensity of use") 
p3 <- plot(Effect("study_areas", m3), 
           main = "Study area", 
           xlab = "Study area", ylab = "Mean intensity of use")
p4 <- plot(Effect("mean_forestCover", m1), 
           main = "Forest cover", 
           xlab = "Forest cover", ylab = "Mean intensity of use")
p5 <- plot(Effect("mean_tri", m1), 
           main = "Terrain ruggedness index", 
           xlab = "Terrain ruggedness index", ylab = "Mean intensity of use")
p6 <- plot(Effect("mean_p2ptime", m1), 
           main = "Time between fixes", 
           xlab = "Time between fixes", ylab = "Mean intensity of use")
p7 <- plot(Effect("mean_dayLength", m1), 
           main = "Day length", 
           xlab = "Day length", ylab = "Mean intensity of use")
p8 <- plot(Effect("week", m1), 
           main = "Week", xlab = "Week", ylab = "Mean intensity of use")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 3)

pdf(file = "Outputs/single_trait_models/m2_effects.pdf", 
    height = 10, width = 10*1.5)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 3)
dev.off()



## Figure S3 ####

# m2 fixed effects

p1 <- plot(Effect("sex", m2), 
           main = "Sex", xlab = "Sex", ylab = "Mean diurnality") 

p2 <- plot(Effect("age_class", m2), 
           main = "Age class", xlab = "Age class", ylab = "Mean diurnality")

p3 <- plot(Effect("study_areas", m2), 
           main = "Study area", xlab = "Study area", ylab = "Mean diurnality") 

p4 <- plot(Effect("mean_hfi", m2), 
           main = "Human footprint index", 
           xlab = "Human footprint index", ylab = "Mean diurnality") 

p5 <- plot(Effect("mean_distRoads", m2),
           main = "Distance to roads", 
           xlab = "Distance to roads (m)", ylab = "Mean diurnality")

p6 <- plot(Effect("mean_distPaths", m2),
           main = "Distance to paths", 
           xlab = "Distance to paths (m)", ylab = "Mean diurnality")

p7 <- plot(Effect("mean_tri", m2),
           main = "Terrain ruggedness index", 
           xlab = "Terrain ruggedness index", ylab = "Mean diurnality")

p8 <- plot(Effect("mean_dayLength", m2), 
           main = "Day length", 
           xlab = "Day length", ylab = "Mean diurnality")

p9 <- plot(Effect("week", m2), 
           main = "Week", xlab = "Week", ylab = "Mean diurnality")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol = 3)

pdf(file = "Outputs/single_trait_models/m3_effects.pdf", 
    height = 10, width = 10*1.5)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, nrow = 3)
dev.off()

## Figure S4 ####

# random effects with study area 

p1 <- ggplot(randomSims1, aes(x = reorder(animalID, mean))) +
  geom_errorbar(aes(ymin = mean-sd,
                    ymax = mean+sd)) +
  geom_point(aes(y = mean, col = study_areas), size = 2) +
  # scale_color_viridis_c(option = "D") +
  geom_hline(aes(yintercept = median(mean))) + 
  labs(y = "Movement rate random effect", col = "Study area") + 
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    # axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

p2 <- ggplot(randomSims2, aes(x = reorder(animalID, mean))) +
  geom_errorbar(aes(ymin = mean-sd,
                    ymax = mean+sd)) +
  geom_point(aes(y = mean, col = study_areas), size = 2) +
  # scale_color_viridis_c(option = "D") +
  geom_hline(aes(yintercept = median(mean))) + 
  labs(y = "Intensity of use rndom effect", col = "Study area") + 
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    # axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )


p3<- ggplot(randomSims3, aes(x = reorder(animalID, mean))) +
  geom_errorbar(aes(ymin = mean-sd,
                    ymax = mean+sd)) +
  geom_point(aes(y = mean, col = study_areas), size = 2) +
  # scale_color_viridis_c(option = "D") +
  geom_hline(aes(yintercept = median(mean))) + 
  labs(y = "Diurnality random effect", col = "Study area") + 
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    # axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

pdf("Outputs/randomEffects_areas.pdf", width = 13, height = 9)
gridExtra::grid.arrange(p1, p2, p3, nrow = 3)
dev.off()

## Figure S5 ####

# pp_check predictive plots 
pp1 <- pp_check(m1_brm, resp = "scalemeanmvmntrate") + 
  ggtitle("Movement rate")  +
  theme_bw()

pp2 <- pp_check(m1_brm, resp = "scalemeanintensityusetrans") + 
  ggtitle("Intensity of use") + 
  theme_bw()

pp3 <- pp_check(m1_brm, resp = "meandiurnalitybin")  + 
  ggtitle("Diurnality") + 
  theme_bw()

pdf("Outputs/ppchcks.pdf", height = 8, width = 10)
gridExtra::grid.arrange(pp1, pp2, pp3, nrow = 2)
dev.off()


# Figure S6 ####

# brms fixed effects 

sex_effect <- conditional_effects(m1_brm, "sex")

s1 <- plot(sex_effect, plot = FALSE)[[1]] +
  theme_bw() + 
  labs(x = "Sex", y = "Scaled movement rate effect")

s2 <- plot(sex_effect, plot = FALSE)[[3]] +
  theme_bw() + 
  labs(x = "Sex", y = "Scaled intensity of use effect")

s3 <- plot(sex_effect, plot = FALSE)[[2]] +
  theme_bw() + 
  labs(x = "Sex", y = "Scaled diurnality effect")

age_effect <- conditional_effects(m1_brm, "age_class")

a1 <- plot(age_effect, plot = FALSE)[[1]] +
  theme_bw() + 
  labs(x = "Age class", y = "Scaled movement rate effect")

a2 <- plot(age_effect, plot = FALSE)[[3]] +
  theme_bw() + 
  labs(x = "Age class", y = "Scaled intensity of use effect")

a3 <- plot(age_effect, plot = FALSE)[[2]] +
  theme_bw() + 
  labs(x = "Age class", y = "Scaled diurnality effect")

studyarea_effect <- conditional_effects(m1_brm, "study_areas")

r1 <- plot(studyarea_effect, plot = FALSE)[[1]] +
  theme_bw() + 
  labs(x = "Study area", y = "Scaled movement rate effect")

r2 <- plot(studyarea_effect, plot = FALSE)[[3]] +
  theme_bw() + 
  labs(x = "Study area", y = "Scaled intensity of use effect")

r3 <- plot(studyarea_effect, plot = FALSE)[[2]] +
  theme_bw() + 
  labs(x = "Study area", y = "Scaled diurnality effect")

pdf("Outputs/fixed_effects_brms.pdf", width = 16*0.8, height = 10*0.8)
gridExtra::grid.arrange(s1, s2, s3, 
                        a1, a2, a3, 
                        r1, r2, r3, nrow = 3)
dev.off()


forestCover <- conditional_effects(m1_brm, "mean_forestCover", spaghetti = T)

f1 <- plot(forestCover, plot = FALSE)[[1]] +
  theme_bw() + 
  labs(x = "Forest cover", y = "Scaled movement rate effect")

f2 <- plot(forestCover, plot = FALSE)[[3]] +
  theme_bw() + 
  labs(x = "Forest cover", y = "Scaled intensity of use effect")

f3 <- plot(forestCover, plot = FALSE)[[2]] +
  theme_bw() + 
  labs(x = "Forest cover", y = "Scaled diurnality effect")

tri <- conditional_effects(m1_brm, "mean_tri")

t1 <- plot(tri, plot = FALSE)[[1]] +
  theme_bw() + 
  labs(x = "Terrain ruggedness index", y = "Scaled movement rate effect")

t2 <- plot(tri, plot = FALSE)[[3]] +
  theme_bw() + 
  labs(x = "Terrain ruggedness index", y = "Scaled intensity of use effect")

t3 <- plot(tri, plot = FALSE)[[2]] +
  theme_bw() + 
  labs(x = "Terrain ruggedness index", y = "Scaled diurnality effect")

dr <- conditional_effects(m1_brm, "mean_distRoads")

d1 <- plot(dr, plot = FALSE)[[1]] +
  theme_bw() + 
  labs(x = "Distance to roads", y = "Scaled movement rate effect")

d2 <- plot(dr, plot = FALSE)[[3]] +
  theme_bw() + 
  labs(x = "Distance to roads", y = "Scaled intensity of use effect")

d3 <- plot(dr, plot = FALSE)[[2]] +
  theme_bw() + 
  labs(x = "Distance to roads", y = "Scaled diurnality effect")


pdf("Outputs/fixed_effects2_brms.pdf", width = 16*0.8, height = 10*0.8)
gridExtra::grid.arrange(f1, f2, f3, 
                        t1, t2, t3,
                        d1, d2, d3, nrow = 3)
dev.off()


dl <- conditional_effects(m1_brm, "mean_dayLength")

dl1 <- plot(dl, plot = FALSE)[[1]] +
  theme_bw() + 
  labs(x = "Day length", y = "Scaled movement rate effect")

dl2 <- plot(dl, plot = FALSE)[[3]] +
  theme_bw() + 
  labs(x = "Day length", y = "Scaled intensity of use effect")

dl3 <- plot(dl, plot = FALSE)[[2]] +
  theme_bw() + 
  labs(x = "Day length", y = "Scaled diurnality effect")

p2p <- conditional_effects(m1_brm, "mean_p2ptime")

p2p1 <- plot(p2p, plot = FALSE)[[1]] +
  theme_bw() + 
  labs(x = "Time between fixes", y = "Scaled movement rate effect")

p2p2 <- plot(p2p, plot = FALSE)[[3]] +
  theme_bw() + 
  labs(x = "Time between fixes", y = "Scaled intensity of use effect")

p2p3 <- plot(p2p, plot = FALSE)[[2]] +
  theme_bw() + 
  labs(x = "Time between fixes", y = "Scaled diurnality effect")

w <- conditional_effects(m1_brm, "week", rug = T)

w1 <- plot(w, plot = FALSE)[[1]] +
  theme_bw() + 
  labs(x = "Week", y = "Scaled movement rate effect")

w2 <- plot(w, plot = FALSE)[[3]] +
  theme_bw() + 
  labs(x = "Week", y = "Scaled intensity of use effect")

w3 <- plot(w, plot = FALSE)[[2]] +
  theme_bw() + 
  labs(x = "week", y = "Scaled diurnality effect")


pdf("Outputs/fixed_effects3_brms.pdf", width = 16*0.8, height = 10*0.8)
gridExtra::grid.arrange(dl1, dl2, dl3, 
                        p2p1, p2p2, p2p3,
                        w1, w2, w3, nrow = 3)
dev.off()


