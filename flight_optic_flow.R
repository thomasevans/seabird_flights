
# Load in data -----

# Load flight summary data
load("flight_details.RData")

# Make data subsets ----

# Add species name column
flight.details$sp_name <- flight.details$species
flight.details$sp_name[flight.details$species == "gull"] <- "Lesser black-backed gull"
flight.details$sp_name[flight.details$species == "murre"] <- "Common murre"
flight.details$sp_name <- as.factor(flight.details$sp_name)

flights.gulls <- flight.details[flight.details$species == "gull",]
flights.gulls <- flight.details[flight.details$species == "murre",]

# levels(flight.details$sp_name)

# For using wind @10 m (i.e. ignoring altitude)
flight_10m <- flight.details

# For using altitude data - calculated wind at flight height
# (excluding those with low Q altitude values)
flight_alt_ok <- flight.details[flight.details$altitude_filter_n >= 1,]

# Using altitude data - but all - calculated wind at flight height
flight_alt <- flight.details


unique(flight.details$species)

# WEAK CROSS-WINDS ---
no_cross <- flight.details$species == "gull" & ( abs(flight.details$track_cross_wind_10m) < 2)
 no_cross <- flight.details$species == "gull" 
 no_cross <- flight.details$species == "murre" 
 
 
summary(no_cross)
flights_c <- flight.details[no_cross,]


# Plot
plot(flights_c$vg~flights_c$track_head_wind_10m,
     col = as.factor(flights_c$ring_number))

plot(flights_c$vg~flights_c$track_head_wind_10m,
     col = as.factor(flights_c$ring_number),
     log = "xy")


birds <- read.csv("deployments_details_export.csv")
birds_flight <- read.csv("flight_calc_birds.csv")
flights_bird1 <- merge(birds_flight, birds, by = "ring_number", all = TRUE)
flights_bird <- merge(flights_c, flights_bird1, by = "ring_number", all = TRUE)

# ?merge

weight_range <- max(flights_bird$weight.kg., na.rm = TRUE) - min(flights_bird$weight.kg., na.rm = TRUE)
weight_min <- min(flights_bird$weight.kg., na.rm = TRUE)
col.alpha <- 0.3 + 0.5*(flights_bird$weight.kg. - weight_min)/weight_range
hist(col.alpha)


addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

keep <- !is.na(col.alpha)
# s

pdf("OF_plots.pdf")
plot(flights_bird$vg[keep]~flights_bird$track_head_wind_10m[keep],
     # col = addalpha(rep("red", sum(keep)), alpha = col.alpha[keep]),
     col = addalpha(as.factor(flights_c$ring_number[keep]), alpha = col.alpha[keep]),
     pch = 20, cex = 2)
# addalpha("red", alpha = 0.5) 
# range(col.alpha[keep])



plot(flights_c$altitude_callib_extm_05~flights_c$track_head_wind_10m,
     col = as.factor(flights_c$ring_number), pch = 20, cex = 2)


plot(flights_c$va_flt_ht~flights_c$track_head_wind_10m,
     col = as.factor(flights_c$ring_number), pch = 20, cex = 2)
dev.off()

# library(ggplot2)

# Make plots with ggplot



gdata <- cbind.data.frame(flights_bird$vg[keep], flights_bird$track_head_wind_10m[keep], col.alpha[keep],
                          flights_c$ring_number[keep], flights_bird$weight.kg.[keep],
                          flights_bird$Vmr[keep],
                          abs(flights_bird$track_cross_wind_10m[keep]),
                          flights_bird$altitude_callib_extm_05[keep])

names(gdata) <- c("vg", "vw_head", "alpha", "ring_number", "weight", "vmr", "vwc", "altitude")


ggplot(gdata,aes(x = vw_head, y = vg)) + 
#   geom_raster(aes(fill=z3))+
#   scale_fill_gradient2(low = muted("blue"), mid = "white",
#                        high = muted("red"), midpoint = median(
#                          flight_alt_ok_gg$z, na.rm = TRUE
#                        ), space = "Lab",
#                        na.value = "grey50", guide = "colourbar") +
  # scale_x_continuous(expand=c(0,0))+
  # scale_y_continuous(expand=c(0,0))+
  # coord_fixed() +
  geom_point(aes(fill = weight), 
             shape=21,na.rm=T, size=1.5) +
  labs( x = expression("Vw"["s"]~~~~"Wind assitance ("~ms^{-1}~")"),
        y = expression("Vg"~~~~"Ground speed ("~ms^{-1}~")"),
        # fill = "Altitude\n(m)",
        parse = TRUE) +
  scale_fill_gradient2(low = muted("blue"), mid = "white",
                       high = muted("red"), midpoint = median(
                         gdata$weight, na.rm = TRUE
                       ), space = "Lab",
                       na.value = "grey50", guide = "colourbar") +
  theme_new
ggsave("vg_vw_plot_mass.pdf")



ggplot(gdata,aes(x = vw_head, y = vg)) + 
  #   geom_raster(aes(fill=z3))+
  #   scale_fill_gradient2(low = muted("blue"), mid = "white",
  #                        high = muted("red"), midpoint = median(
  #                          flight_alt_ok_gg$z, na.rm = TRUE
  #                        ), space = "Lab",
  #                        na.value = "grey50", guide = "colourbar") +
  # scale_x_continuous(expand=c(0,0))+
  # scale_y_continuous(expand=c(0,0))+
  # coord_fixed() +
  geom_point(aes(fill = vmr), 
             shape=21,na.rm=T, size=1.5) +
  labs( x = expression("Vw"["s"]~~~~"Wind assitance ("~ms^{-1}~")"),
        y = expression("Vg"~~~~"Ground speed ("~ms^{-1}~")"),
        # fill = "Altitude\n(m)",
        parse = TRUE) +
  scale_fill_gradient2(low = muted("blue"), mid = "white",
                       high = muted("red"), midpoint = median(
                         gdata$vmr, na.rm = TRUE
                       ), space = "Lab",
                       na.value = "grey50", guide = "colourbar") +
  theme_new
ggsave("vg_vw_plot_vmr.pdf")



gdata$vwc


ggplot(gdata,aes(x = vw_head, y = vg)) + 
  #   geom_raster(aes(fill=z3))+
  #   scale_fill_gradient2(low = muted("blue"), mid = "white",
  #                        high = muted("red"), midpoint = median(
  #                          flight_alt_ok_gg$z, na.rm = TRUE
  #                        ), space = "Lab",
  #                        na.value = "grey50", guide = "colourbar") +
  # scale_x_continuous(expand=c(0,0))+
  # scale_y_continuous(expand=c(0,0))+
  # coord_fixed() +
  geom_point(aes(fill = vwc), 
             shape=21,na.rm=T, size=2, alpha = 0.7) +
  labs( x = expression("Vw"["s"]~~~~"Wind assitance ("~ms^{-1}~")"),
        y = expression("Vg"~~~~"Ground speed ("~ms^{-1}~")"),
        # fill = "Altitude\n(m)",
        parse = TRUE) +
  scale_fill_gradient2(low = muted("blue"), mid = "white",
                       high = muted("red"), midpoint = median(
                         gdata$vwc, na.rm = TRUE
                       ), space = "Lab",
                       na.value = "grey50", guide = "colourbar") +
  theme_new
ggsave("vg_vw_plot_vwc2.pdf")






ggplot(gdata,aes(x = vg , y = altitude)) + 
  #   geom_raster(aes(fill=z3))+
  #   scale_fill_gradient2(low = muted("blue"), mid = "white",
  #                        high = muted("red"), midpoint = median(
  #                          flight_alt_ok_gg$z, na.rm = TRUE
  #                        ), space = "Lab",
  #                        na.value = "grey50", guide = "colourbar") +
  # scale_x_continuous(expand=c(0,0))+
  # scale_y_continuous(expand=c(0,0))+
  # coord_fixed() +
  geom_point(aes(fill = vwc), 
             shape=21,na.rm=T, size=2, alpha = 0.7) +
  labs( y = "height (m)",
        x = expression("Vg"~~~~"Ground speed ("~ms^{-1}~")"),
        # fill = "Altitude\n(m)",
        parse = TRUE) +
  scale_fill_gradient2(low = muted("blue"), mid = "white",
                       high = muted("red"), midpoint = median(
                         gdata$vwc, na.rm = TRUE
                       ), space = "Lab",
                       na.value = "grey50", guide = "colourbar") +
  theme_new
ggsave("vg_h_plot_vwc2.pdf")





ggplot(gdata[gdata$vwc<2,],aes(x = vw_head , y = altitude)) + 
  #   geom_raster(aes(fill=z3))+
  #   scale_fill_gradient2(low = muted("blue"), mid = "white",
  #                        high = muted("red"), midpoint = median(
  #                          flight_alt_ok_gg$z, na.rm = TRUE
  #                        ), space = "Lab",
  #                        na.value = "grey50", guide = "colourbar") +
  # scale_x_continuous(expand=c(0,0))+
  # scale_y_continuous(expand=c(0,0))+
  # coord_fixed() +
  geom_point(aes(fill = vwc), 
             shape=21,na.rm=T, size=2, alpha = 0.7) +
  labs( y = "height (m)",
        x = expression("Vw"~~~~"Tail wind ("~ms^{-1}~")"),
        # fill = "Altitude\n(m)",
        parse = TRUE) +
  scale_fill_gradient2(low = muted("blue"), mid = "white",
                       high = muted("red"), midpoint = median(
                         gdata$vwc, na.rm = TRUE
                       ), space = "Lab",
                       na.value = "grey50", guide = "colourbar") +
  theme_new
ggsave("vw_h_plot_vwc_less2ms.pdf")






# Statistical test of polynomial relationship ----
library(lme4)
library(arm)
library(lattice)
library(MuMIn)
flights.gulls.original <- flights.gulls
flights.gulls <- flights.gulls.original
flights.gulls <- flights.gulls.original[flights.gulls.original$track_cross_wind_10m<2,]

mods.of <- list()
mods.of[[3]] <- glmer(vg ~ track_head_wind_10m + I(track_head_wind_10m^2)
                              + (1|ring_number),
                            data = flights.gulls)
# summary(mods.of[[1]])
mods.of[[2]] <- glmer(vg ~ track_head_wind_10m
                      + (1|ring_number),
                      data = flights.gulls)

mods.of[[1]] <- glmer(vg ~ 1
                      + (1|ring_number),
                      data = flights.gulls)


mods.of[[4]] <- glmer(vg ~ 
                        track_cross_wind_10m
                      + (1|ring_number),
                      data = flights.gulls)

mods.of[[5]] <- glmer(vg ~ track_head_wind_10m + I(track_head_wind_10m^2) +
                        track_cross_wind_10m
                      + (1|ring_number),
                      data = flights.gulls)

mods.of[[6]] <- glmer(vg ~ track_head_wind_10m +
                        track_cross_wind_10m
                      + (1|ring_number),
                      data = flights.gulls)

mods.of[[7]] <- glmer(vg ~ exp(track_head_wind_10m) +
                        track_cross_wind_10m
                      + (1|ring_number),
                      data = flights.gulls)

mods.of[[8]] <- glmer(vg ~ track_head_wind_10m + exp(track_head_wind_10m) +
                        track_cross_wind_10m
                      + (1|ring_number),
                      data = flights.gulls)

# mods.of[[6]] <- glmer(vg ~ track_head_wind_10m +
#                         track_cross_wind_10m
#                       + (1|ring_number),
#                       data = flights.gulls)

models.gull.aicc <- sapply(mods.of, AICc)

models.gull.aicc.dif <- models.gull.aicc-min(models.gull.aicc)


models.gull.r2m <- sapply(mods.of, r.squaredGLMM)
t(models.gull.r2m)
d_r2m <- t(models.gull.r2m)[,1] - max(t(models.gull.r2m)[,1])
x <- cbind.data.frame(models.gull.aicc, models.gull.aicc.dif, t(models.gull.r2m), d_r2m)
write.csv(x, file = "models_optic_flow_gulls_all.csv")

write.csv(x, file = "models_optic_flow_murres_sub_2_cross.csv")

# x <- c(-10:10)
# y <- exp(x)
# plot(y~x)
# log(y)

# p values

library(pbkrtest)

KRSumFun <- function(object, objectDrop, ...) {
  krnames <- c("ndf","ddf","Fstat","p.value","F.scaling")
  r <- if (missing(objectDrop)) {
    setNames(rep(NA,length(krnames)),krnames)
  } else {
    krtest <- KRmodcomp(object,objectDrop)
    unlist(krtest$stats[krnames])
  }
  attr(r,"method") <- c("Kenward-Roger via pbkrtest package")
  r
}

# P values for gull model
drop1(mods.of[[3]], test="user", sumFun=KRSumFun)


lbbg_model_va_coef <- summary(mods.of[[3]])$coef[, 1]
lbbg_model_va_ci <- confint(mods.of[[3]], method="Wald")
lbbg_model_va_par_df <- cbind.data.frame(lbbg_model_va_coef,lbbg_model_va_ci[-c(1:2),])


anova(mods.of[[3]], mods.of[[2]])

# behaviour of models

plot(mods.of[[3]])
qqmath(mods.of[[3]])


# Model predcitions


wind.side <- 0
wind.assist <- seq(-10,10,.1)
birds <- unique(flights.gulls$ring_number)

gg <- expand.grid(x=wind.assist,y=wind.side, z = birds)


lbbg.new.data.df <- cbind.data.frame(
  track_head_wind_10m = (gg$x),
  track_cross_wind_10m = (gg$y),
  ring_number = (gg$z)
)


# mean
pred.va <- predict(mods.of[[3]],
                   newdata = lbbg.new.data.df[gg$z == birds[1],],
                   re.form=NA)
plot(pred.va~gg$x[gg$z == birds[1]])

# ran ef
pred.va <- predict(mods.of[[2]],
                   newdata = lbbg.new.data.df,
                   re.form=NULL)
plot(flights.gulls$vg~flights.gulls$track_head_wind_10m,
       col = addalpha(as.numeric(flights.gulls$ring_number), 0.5))

for(i in 1:length(birds)){
  f <- gg$z == birds[i]
  points(pred.va[f]~gg$x[f], col = as.numeric(gg$z[f]), type = "l")  
  
  }

# points(pred.va2~gg$x, lwd = 2, type = "l", lty = 2, col = addalpha("black", 0.5))

summary(mods.of[[3]])


names(flights.gulls)

theme_new_rt_legend <- theme_bw(base_size = 14, base_family = "serif") +
  theme(legend.position = "right",
        legend.justification = c(1, 1),
        legend.key.size =   unit(1, "lines"),
        legend.key = element_rect(colour =NA),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text.align = 0,
        legend.key.width = unit(1.5, "lines"),
        # legend.key.height = unit(1, "lines"),
        legend.title = element_blank(),
        legend.key.height=unit(0.7,"line")
  )

funcx <- expression("Vx"~"="~11.3~+0.67~"Vw"~+~0.03~"Vw"^2~+~0.17~"Vw"["c"])
funcx <- expression("Vx"~"="~11.8~+0.69~"Vw"~+~0.17~"Vw"["c"])

funcx <- expression("Vx"~"="~14.4~+0.46~"Vw"~+~0.05~"Vw"^2)
funcx <- expression("Vx"~"="~15.2~+0.46~"Vw")


library(cowplot)

fit.data <- cbind.data.frame(pred.va, gg$x, gg$y, gg$z)
names(fit.data) <- c("vg", "track_head_wind_10m", "track_cross_wind_10m", "ring_number" )
ggplot(flights.gulls,aes(x = track_head_wind_10m , y = vg, col = ring_number)) + 
  geom_line(data = fit.data, alpha = 0.5,lwd = 1) +
  geom_point(shape=21,na.rm=T, size=2, alpha = 0.7) +
  labs( y = expression("Vx"~~~~"Ground speed ("~ms^{-1}~")"),
        x = expression("Vw"~~~~"Tail wind ("~ms^{-1}~")"),
        # fill = "Altitude\n(m)",
        parse = TRUE) +
  theme_new_rt_legend +
  annotate('text', x = -4, y = 20, size=3, 
             label = paste(funcx), parse = TRUE) +
  guides(col=guide_legend(ncol=1))
           # parse = TRUE) 
# ?annotate
# ggsave("vx_vw_fit_linear_mean_form.svg", width = 6, height = 5, units = "in")
ggsave("vx_vw_fit_linear_all_form_murres.svg", width = 6, height = 5, units = "in")
ggsave("vx_vw_fit_quad_all_form_murres.svg", width = 6, height = 5, units = "in")

ggsave("vx_vw_fit_linear_mean_form_murres.svg", width = 6, height = 5, units = "in")
ggsave("vx_vw_fit_quad_mean_form_murres.svg", width = 6, height = 5, units = "in")





# vx ~ Vw ----
ggplot(flights.gulls,aes(x = vg  , y = track_head_wind_10m, col = ring_number)) + 
  # geom_line(data = fit.data, alpha = 0.6, col = "black", lwd = 1.5) +
  geom_point(shape=21,na.rm=T, size=2, alpha = 0.7) +
  labs( x = expression("Vx"~~~~"Ground speed ("~ms^{-1}~")"),
        y = expression("Vw"~~~~"Tail wind ("~ms^{-1}~")"),
        # fill = "Altitude\n(m)",
        parse = TRUE) +
  theme_new_rt_legend 
  

?nlsModel
nlmod <- nls(flights.gulls$track_head_wind_10m ~   A *flights.gulls$vg^B)


             # exp(B * x))

df <- cbind.data.frame(flights.gulls$track_head_wind_10m, flights.gulls$vg)
# df <- df[df$x >0,]
names(df) <- c("y", "x")

df <- df[df$y >0,]

# m <- nls(y ~ b + a*(I(x^power)), data = df, start = list(power = 0.2, a = -0.1), trace = T)


plot(df$y~df$x)
s <- seq(0,25,1)
m <- nls(y ~(I( b +  a*x^power)), data = df, start = list(power = 1, a =1,  b = 1), trace = T)
lines(s, predict(m, list(x = s)), col = "blue", lwd = 2)
summary(m)
# ?nls




# rearrange data ----
df.flights <- cbind.data.frame(
  vw_h = NA,
  h = flights.gulls$altitude_callib_extm_05,
  vw_10 = flights.gulls$track_head_wind_10m,
  bird = flights.gulls$ring_number,
  vx = flights.gulls$vg,
  vw_c_10 = flights.gulls$track_cross_wind_10m,
  flight_id_combined = flights.gulls$flight_id_combined
)

# nrow(flights.gulls)
# 
# Vw_h
# h
# Vw_10
# bird
# vx
# vw_c_10

df.flights$vw_h <- df.flights$vw_10* ((df.flights$h/10)^0.11)
  
plot(df.flights$vw_h~df.flights$h)



plot(df.flights$vw_h ~ df.flights$h)


plot(df.flights$h ~ df.flights$vw_10)

df.flights.cross <- df.flights[abs(df.flights$vw_c_10)<2,]
plot(df.flights.cross$h ~ df.flights.cross$vw_10)
nrow(df.flights)
nrow(df.flights.cross)

flight_exp <- df.flights.cross$flight_id_combined[df.flights.cross$vw_10 < - 2 &
                             df.flights.cross$h > 40 ]
flight_exp <- flight_exp[!is.na(flight_exp)]
i <- 1
pdf("strange_flights.pdf")
for(i in 1:length(flight_exp)){
  par(mfrow = c(2,1))
  fid <- flight_exp[i]
  st <- flights.gulls$start_time[flights.gulls$flight_id_combined == fid]
  et <- flights.gulls$end_time[flights.gulls$flight_id_combined == fid]
  f <- points.all$flight_id_combined == fid &
    points.all$date_time >= st &
    points.all$date_time <= et
  
  plot(points.all$altitude[f]~points.all$date_time[f], main = paste(fid))
  plot(points.all$longitude[f]~points.all$latitude[f])
  }
dev.off()


mod <- lm(df.flights.cross$h ~ df.flights.cross$vw_10)
anova(mod)
summary(mod)
cor.test(df.flights.cross$h, df.flights.cross$vw_10)






# Final Vg models for gulls and murres -----
flights.gulls <- flight.details[flight.details$species == "gull",]
flights.murres <- flight.details[flight.details$species == "murre",]

library(lme4)
library(arm)
library(lattice)
library(MuMIn)
library(ggplot2)
library(cowplot)

# Two best fit models for two species (linear and quadratic fit models)


mod.gull.5 <- glmer(vg ~ track_head_wind_10m + I(track_head_wind_10m^2) +
                        track_cross_wind_10m
                      + (1|ring_number),
                      data = flights.gulls)

mod.gull.6 <- glmer(vg ~ track_head_wind_10m +
                        track_cross_wind_10m
                      + (1|ring_number),
                      data = flights.gulls)



mod.murre.3 <- glmer(vg ~ track_head_wind_10m + I(track_head_wind_10m^2)
                      + (1|ring_number),
                      data = flights.murres)

mod.murre.2 <- glmer(vg ~ track_head_wind_10m
                      + (1|ring_number),
                      data = flights.murres)



# Make coef plot thing (mod 3 for murres, mod 5 for gulls) -----

mod_gull_frame <- data.frame(Variable = rownames(summary(mod.gull.5)$coef)[-1],
                             Coefficient = summary(mod.gull.5)$coef[-1, 1],
                             CI_low = confint(mod.gull.5, method="Wald")[-c(1:3), 1],
                             CI_high = confint(mod.gull.5, method="Wald")[-c(1:3), 2],
                             modelName = "Lesser Black-backed Gulls")
mod_murre_frame <- data.frame(Variable = rownames(summary(mod.murre.3)$coef)[-1],
                              Coefficient = summary(mod.murre.3)$coef[-1, 1],
                              CI_low = confint(mod.murre.3, method="Wald")[-c(1:3), 1],
                              CI_high = confint(mod.murre.3, method="Wald")[-c(1:3), 2],
                              modelName = "Common Murres")
# Combine these data.frames
allModelFrame <- data.frame(rbind(mod_gull_frame, mod_murre_frame))  

str(allModelFrame)
allModelFrame$modelName <-  factor(allModelFrame$modelName,
                                   levels(allModelFrame$modelName)[c(2,1)])

levels(allModelFrame$Variable)

allModelFrame$Variable <-  factor(allModelFrame$Variable,
                                   levels(allModelFrame$Variable)[c(2,1,3)])


# lab.1 <- expression("Vw"["s"]+~"(tail-wind)")
# lab.2 <- expression("Vw"["s"]-~"(head-wind)")
# lab.3 <- expression("Vw"["c"]~"Absolute speed")
# lab.4 <- expression("Vw"["c"]-~"(to left)")
# parse=TRUE



theme_new_top_legend <- theme_bw(base_size = 14, base_family = "serif") +
  theme(legend.position = c(1, 0.4),
        legend.justification = c(1, 1),
        legend.key.size =   unit(2, "lines"),
        legend.key = element_rect(colour =NA),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text.align = 0,
        legend.key.width = unit(3, "lines"),
        legend.title = element_blank()
  )

zp1 <- ggplot(allModelFrame, aes(colour = modelName))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2,
                        show.legend = TRUE)
zp1 <- zp1 + geom_linerange(aes(x = Variable,
                                ymin = CI_low,
                                ymax = CI_high),
                            lwd = 1, position = position_dodge(width = 1/2),
                            show.legend = TRUE)
zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Coefficient,
                                 ymin = CI_low,
                                 ymax = CI_high),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = NA,
                             show.legend = TRUE)
zp1 <- zp1 + scale_x_discrete("",
                              labels = c(expression("Vw"["c"]),
                                         expression("Vw"["s"]^2~""),
                                         expression("Vw"["s"])
                              ))

# expression("Vw"["s"]+~"(tail-wind)")

zp1 <- zp1 + coord_flip() 
# zp1 <- zp1 + theme_new + ylim(-4,2.5)
zp1 <- zp1 + scale_y_continuous(breaks = seq(0, 1, .1),
                                minor_breaks = seq(-0.1, 1, 0.05),
                                limits = c(-0.06, 0.8))
# ?scale_y_continuous
# zp1 <- zp1 + theme(axis.text = element_text(size = 12))
# zp1 <- zp1 +  theme_new
zp1 <- zp1 + theme_new_top_legend
# zp1 <- zp1 + theme(legend.position = c(0.5, 0.3))
zp1 <- zp1 + labs(x = "", y = expression("Coefficient   ("~Delta~"Vg ["~ms^{-1}~"])"))
zp1 <- zp1 +
  annotate("text",  x= layer_scales(zp1)$x$range$range[3],
           y = layer_scales(zp1)$y$range$range[1], label = "(a)",
           vjust= - 4, hjust= -2, size = 5)
# zp1 <- zp1 + theme(legend.justification=c(0,0), legend.position=c(0,0))
zp1

ggsave(zp1, filename = "vg_model_coef_fig.svg", width = 6, height = 8,
       units = "in")



# Make model illustration thing (show curve + mayber dashed line for linear) for two species -----

# Gulls


wind.side <- 0
wind.assist <- seq(-10,10,.1)
birds <- unique(flights.gulls$ring_number)

gg <- expand.grid(x=wind.assist,y=wind.side, z = birds)


lbbg.new.data.df <- cbind.data.frame(
  track_head_wind_10m = (gg$x),
  track_cross_wind_10m = (gg$y),
  ring_number = (gg$z)
)


# mean
pred.vg_quad <- predict(mod.gull.5,
                   newdata = lbbg.new.data.df[gg$z == birds[1],],
                   re.form=NA)
plot(pred.vg_quad~gg$x[gg$z == birds[1]])

pred.vg_linear <- predict(mod.gull.6,
                   newdata = lbbg.new.data.df[gg$z == birds[1],],
                   re.form=NA)
plot(pred.vg_linear~gg$x[gg$z == birds[1]])


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols.new <- gg_color_hue(2)



fit.data <- cbind.data.frame(pred.vg_quad, pred.vg_linear, gg$x, gg$y, gg$z)
names(fit.data) <- c("vg", "vg_linear", "track_head_wind_10m", "track_cross_wind_10m", "ring_number" )
g_gulls <- ggplot(flights.gulls,aes(x = track_head_wind_10m , y = vg)) + 
  geom_vline(xintercept = 0) +
  geom_line(data = fit.data, alpha = 0.8,lwd = 1.2,
            col = cols.new[2]) +
  geom_line(aes(x = track_head_wind_10m , y = vg_linear),
            data = fit.data, alpha = 0.8,lwd = 1.2,
            lty = 2,
            col = cols.new[2]) +
  scale_x_continuous(breaks = seq(-10, 10, 2),
                       minor_breaks = seq(-12, 12, 1),
                       limits = c(-11 , 11)) +
  geom_point(shape=21,na.rm=T, size=2, alpha = 0.5, col = "grey10") +
  labs( y = expression("Vg"~~~~"Ground speed ("~ms^{-1}~")"),
        x = expression("Vw"~~~~"Wind assistance ("~ms^{-1}~")"),
        # fill = "Altitude\n(m)",
        parse = TRUE) +
  theme_new_top_legend 
g_gulls + annotate("text",  x= -10,
           y = layer_scales(g_gulls)$y$range$range[2], label = "(b)",
           vjust= 2, hjust= 0, size = 5)
# parse = TRUE) 
ggsave("vg_gull_prediction.svg", width = 5, height = 4, units = "in")

# Murres


wind.side <- 0
wind.assist <- seq(-10,10,.1)
birds <- unique(flights.murres$ring_number)

gg <- expand.grid(x=wind.assist,y=wind.side, z = birds)


lbbg.new.data.df <- cbind.data.frame(
  track_head_wind_10m = (gg$x),
  track_cross_wind_10m = (gg$y),
  ring_number = (gg$z)
)


# mean
pred.vg_quad <- predict(mod.murre.3,
                        newdata = lbbg.new.data.df[gg$z == birds[1],],
                        re.form=NA)
plot(pred.vg_quad~gg$x[gg$z == birds[1]])

pred.vg_linear <- predict(mod.murre.2,
                          newdata = lbbg.new.data.df[gg$z == birds[1],],
                          re.form=NA)
plot(pred.vg_linear~gg$x[gg$z == birds[1]])






fit.data <- cbind.data.frame(pred.vg_quad, pred.vg_linear, gg$x, gg$y, gg$z)
names(fit.data) <- c("vg", "vg_linear", "track_head_wind_10m", "track_cross_wind_10m", "ring_number" )
g_murres <- ggplot(flights.murres,aes(x = track_head_wind_10m , y = vg)) + 
  geom_vline(xintercept = 0) +
  geom_line(data = fit.data, alpha = 0.8,lwd = 1.2,
            col = cols.new[1]) +
  geom_line(aes(x = track_head_wind_10m , y = vg_linear),
            data = fit.data, alpha = 0.8,lwd = 1.2,
            lty = 2,
            col = cols.new[1]) +
  scale_x_continuous(breaks = seq(-10, 10, 2),
                     minor_breaks = seq(-12, 12, 1),
                     limits = c(-11 , 11)) +
  geom_point(shape=21,na.rm=T, size=2, alpha = 0.5, col = "grey10") +
  labs( y = expression("Vg"~~~~"Ground speed ("~ms^{-1}~")"),
        x = expression("Vw"~~~~"Wind assistance ("~ms^{-1}~")"),
        # fill = "Altitude\n(m)",
        parse = TRUE) +
  theme_new_top_legend 
g_murres + annotate("text",  x= -10,
                   y = layer_scales(g_murres)$y$range$range[2], label = "(c)",
                   vjust= 2, hjust= 0, size = 5)
# parse = TRUE)
# parse = TRUE) 
ggsave("vg_murre_prediction.svg", width = 5, height = 4, units = "in")



# plot(mod.murre.3)
# qqmath(mod.murre.3)
