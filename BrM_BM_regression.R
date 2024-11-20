# BLAKELY DEER
# Brain mass to body mass regression (populations split)
# Claire Geiman and Dr. Eric Long
# September 15, 2022

# set working directory
setwd(dir = "~/Desktop/Research/Odocoileus hemionus research")

# set plot_folder
plot_folder <- "03. Plots/00. 1200dpi RStudio/"

# load packages
library(ggplot2)
library(ggpp)
library(ggpmisc)
library(devtools)
library(dplyr)
library(ggpubr)
library(scales)

# read in csv
data <- read.csv("01. Data/csv/Deer Skull Data (Sept 2022).csv")

# remove NAs
summary(data)
data <- filter(data, !is.na(CC_mL))
summary(data) # looks spiffy

#==============================================================================#
# add log of BrM_g and log of BM_kg columns
# BrM
class(data$BrM_g)
data$BrM_g <- as.numeric(data$BrM_g) # character -> numeric
class(data$BrM_g) # looks good
data$logBrM <- log10(data$BrM_g)

# BM
class(data$BM_kg)
data$BM_kg <- as.numeric(data$BM_kg) # character -> numeric
class(data$BM_kg) # looks good
data$logBM <- log10(data$BM_kg)

#==============================================================================#
# create population groups for separate regression lines
island <- filter(data, Location == "Island")
mainland <- filter(data, Location == "Mainland")

#==============================================================================#
# find regline equations, including standard error for slope and intercept
# island
eq_island <- lm(logBrM ~ logBM, island)
coef(eq_island) # slope = 0.31, intercept = 1.69
summary(eq_island) # y = 0.31x (+/-0.04) + 1.69 (+/-0.07)
# mainland
eq_mainland <- lm(logBrM ~ logBM, mainland)
coef(eq_mainland) # slope = 0.32, intercept = 1.69
summary(eq_mainland) # y = 0.32x (+/-0.06) + 1.69 (+/-0.10)

#==============================================================================#
# specify decimal places for the regline equations
# view ggpubr source code:
trace(ggpubr:::.stat_lm, edit = TRUE)

# in pop-up window, you’ll see lines 13-14:
  
### => eq.char <- as.character(signif(polynom::as.polynomial(coefs), 2))

# in order to modify package ggpubr so that it reports regline equations with 2 decimal places (instead of the default 2 sig figs), modify the above code to be:
  
### => eq.char <- as.character(round(polynom::as.polynomial(coefs), 2))

#==============================================================================#
# COLOR PLOT
# ggplot object
BrM_BM_regression_color <- ggplot(data = data, aes(x = logBM, y = logBrM, group = Location, color = Location, fill = Location)) +
  geom_point(aes(color = Location)) +
  scale_x_continuous(limits = c(1.4, 1.9)) +
  scale_y_continuous(limits = c(2.1, 2.3),
                     labels = scales::number_format(accuracy = 0.01)) + # to specify decimal places on y axis
  scale_color_manual(values = c("coral", "darkslategrey")) +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  stat_regline_equation(data = island, label.x = 1.77, label.y = 2.135, aes(label = ..eq.label..)) +
  stat_regline_equation(data = island, label.x = 1.77, label.y = 2.12, aes(label = ..rr.label..)) +
  stat_regline_equation(data = mainland, label.x = 1.425, label.y = 2.28, aes(label = ..eq.label..)) +
  stat_regline_equation(data = mainland, label.x = 1.425, label.y = 2.265, aes(label = ..rr.label..)) +
  xlab("Log Body Mass (kg)") +
  ylab("Log Brain Mass (g)") +
  guides(
    fill = guide_legend(title = "Location",
                        override.aes = aes(label = ""))) + # this is to get rid of the weird little "a" on the legend labels
  theme_bw()

# plot
plot(BrM_BM_regression_color)

# save plot
ggsave(paste0(plot_folder, "BrM_BM_regression.jpg"), BrM_BM_regression_color,
       device = "jpeg", units = "in", width = 8, height = 5, dpi = 300)

#==============================================================================#
# B&W PLOT (with no equation or r^2; to be added later in PowerPoint, with SE.
# Also no legend)
# ggplot object
BrM_BM_regression_bw <- ggplot(data = data, aes(x = logBM, y = logBrM, group = Location, color = Location, fill = Location, lty = Location)) +
  geom_point(aes(color = Location, shape = Location)) +
  scale_x_continuous(limits = c(1.4, 1.9)) +
  scale_y_continuous(limits = c(2.1, 2.3),
                     labels = scales::number_format(accuracy = 0.01)) + # to specify decimal places on y axis
  scale_color_manual(values = c("black", "black")) +
  scale_shape_manual(values = c(1, 16)) +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  xlab("Log Body Mass (kg)") +
  ylab("Log Brain Mass (g)") +
  guides(
    fill = guide_legend(title = "Location",
                        override.aes = aes(label = ""))) + # this is to get rid of the weird little "a" on the legend labels
  theme_classic() +
  theme(legend.position = "none")

# plot
BrM_BM_regression_bw

# save plot
ggsave(paste0(plot_folder, "BrM_BM_regression.tiff"), BrM_BM_regression_bw,
       device = "tiff", units = "mm", width = 174, height = 124, dpi = 1200)

#==============================================================================#
################################################################################
#==============================================================================#
# We gonna do a BrM_OC plot for Reviewer 2
# B&W PLOT (with no equation or r^2; to be added later in PowerPoint, with SE.
# Also no legend)

# Add log of OC_mm first
class(data$OC_mm)
data$OC_mm <- as.numeric(data$OC_mm) # character -> numeric
class(data$OC_mm) # looks good
data$logOC <- log10(data$OC_mm)

# re-make to have logOC
island <- filter(data, Location == "Island")
mainland <- filter(data, Location == "Mainland")

# get equations
eq_island_OC <- lm(logBrM ~ logOC, island)
summary(eq_island_OC)

eq_mainland_OC <- lm(logBrM ~ logOC, mainland)
summary(eq_mainland_OC)

# ggplot object
BrM_OC_regression_bw <- ggplot(data = data, aes(x = logOC, y = logBrM, group = Location, color = Location, fill = Location, lty = Location)) +
  geom_point(aes(color = Location, shape = Location)) +
  scale_x_continuous(limits = c(1.6, 1.77)) +
  scale_y_continuous(limits = c(2.1, 2.3),
                     labels = scales::number_format(accuracy = 0.01)) + # to specify decimal places on y axis
  scale_color_manual(values = c("black", "black")) +
  scale_shape_manual(values = c(1, 16)) +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  xlab("Log Occipital Condyle Width (mm)") +
  ylab("Log Brain Mass (g)") +
  guides(
    fill = guide_legend(title = "Location",
                        override.aes = aes(label = ""))) + # this is to get rid of the weird little "a" on the legend labels
  theme_classic() +
  theme(legend.position = "none")

# plot
BrM_OC_regression_bw

# save
ggsave(paste0(plot_folder, "BrM_OC_regression.tiff"), BrM_OC_regression_bw,
       device = "tiff", units = "mm", width = 174, height = 124, dpi = 1200)

