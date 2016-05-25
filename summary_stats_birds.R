# Calculate summary statistics of birds (table 1 in MS)


# Load data ----
birds <- read.csv("deployments_details.csv")


# Make some summaries -----
library(plyr)


bird.summary <- ddply(birds, .(species, sex.morph.),
                      summarise,
                      mass_mean = mean(weight.kg., na.rm = TRUE),
                      mass_sd = sd(weight.kg., na.rm = TRUE),
                      mass_n = sum(!is.na(weight.kg.)),
                      span_mean = mean(photo_wing_span/1000, na.rm = TRUE),
                      span_sd = sd(photo_wing_span/1000, na.rm = TRUE),
                      span_n = sum(!is.na(photo_wing_span/1000)),
                      area_mean = mean(wing_area.m2., na.rm = TRUE),
                      area_sd = sd(wing_area.m2., na.rm = TRUE),
                      area_n = sum(!is.na(wing_area.m2.)),
                      loading_mean = mean(wing_loading..kg.m.2.*10, na.rm = TRUE),
                      loading_sd = sd(wing_loading..kg.m.2.*10, na.rm = TRUE),
                      loading_n = sum(!is.na(wing_loading..kg.m.2.*10)),
                      AR_mean = mean(AR, na.rm = TRUE),
                      AR_sd = sd(AR, na.rm = TRUE),
                      AR_n = sum(!is.na(AR)),
                      wing_beat_mean = mean(acc.median.f, na.rm = TRUE),
                      wing_beat_sd = sd(acc.median.f, na.rm = TRUE),
                      wing_beat_n = sum(!is.na(acc.median.f))
)


bird.summary2 <- ddply(birds, .(species),
                      summarise,
                      mass_mean = mean(weight.kg., na.rm = TRUE),
                      mass_sd = sd(weight.kg., na.rm = TRUE),
                      mass_n = sum(!is.na(weight.kg.)),
                      span_mean = mean(photo_wing_span/1000, na.rm = TRUE),
                      span_sd = sd(photo_wing_span/1000, na.rm = TRUE),
                      span_n = sum(!is.na(photo_wing_span/1000)),
                      area_mean = mean(wing_area.m2., na.rm = TRUE),
                      area_sd = sd(wing_area.m2., na.rm = TRUE),
                      area_n = sum(!is.na(wing_area.m2.)),
                      loading_mean = mean(wing_loading..kg.m.2.*10, na.rm = TRUE),
                      loading_sd = sd(wing_loading..kg.m.2.*10, na.rm = TRUE),
                      loading_n = sum(!is.na(wing_loading..kg.m.2.*10)),
                      AR_mean = mean(AR, na.rm = TRUE),
                      AR_sd = sd(AR, na.rm = TRUE),
                      AR_n = sum(!is.na(AR)),
                      wing_beat_mean = mean(acc.median.f, na.rm = TRUE),
                      wing_beat_sd = sd(acc.median.f, na.rm = TRUE),
                      wing_beat_n = sum(!is.na(acc.median.f))
)


# Combine above ----
bird.summary <- bird.summary[-1,]

bird.summary3 <- cbind.data.frame(bird.summary2[2,1],
                                  sex = "All",
                                  bird.summary2[2,2:19])
names(bird.summary3) <- names(bird.summary)
birds.summary.comb <- rbind.data.frame(bird.summary3,bird.summary)

write.csv(birds.summary.comb, file = "birds_summary_tab.csv")
