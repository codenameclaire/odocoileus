# BLAKELY DEER
# Foramen magnum surface area to body mass regression (populations split)
# Claire Geiman and Dr. Eric Long
# September 25, 2022

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

# change BM_kg to numeric for later
class(data$BM_kg)
summary(data$BM_kg)
data$BM_kg <- as.numeric(data$BM_kg) # character -> numeric
# you'll get a warning message saying that NAs were introduced by coercion. That's
# okay. That's just because there were already blank values from the excel/csv
# (due to blank occipital condyle measurements) but they showed up as "#NUM" in 
# the dataframe (character) instead of "NA" because that's how excel lists NA values. 
# You can go ahead and filter out the 6 new NAs.
class(data$BM_kg) # looks good
summary(data$BM_kg) # 6 NAs
data <- filter(data, !is.na(BM_kg))
summary(data$BM_kg) # no NAs; looks good!

#==============================================================================#
# FORAMEN MAGNUM SURFACE AREA

# multiply data$FMW_mm*data$FMH_mm to get foramen magnum surface area
data$FMA_mm <- (data$FMW_mm*data$FMH_mm)
summary(data$FMA_mm) # no NAs; dope

#==============================================================================#
# LOG BODY MASS
data$logBM <- log10(data$BM_kg^(1/3))

# LOG ORBITAL SURFACE AREA
data$logFMA <- log10(data$FMA_mm^(1/2))

#==============================================================================#
# create population groups for separate regression lines
island <- filter(data, Location == "Island")
mainland <- filter(data, Location == "Mainland")

#==============================================================================#
# find regline equations, including standard error for slope and intercept
# island
eq_island <- lm(logFMA ~ logBM, island)
coef(eq_island) # slope = 0.02, intercept = 1.34
summary(eq_island) # y = 0.02x (+/-0.10 + 1.34 (+/-0.05)
# mainland
eq_mainland <- lm(logFMA ~ logBM, mainland)
coef(eq_mainland) # slope = 0.28, intercept = 1.20
summary(eq_mainland) # y = 0.28x (+/-0.16) + 1.20 (+/-0.09)

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
FMA_BM_regression_color <- ggplot(data = data, aes(x = logBM, y = logFMA, group = Location, color = Location, fill = Location)) +
  geom_point(aes(color = Location)) +
  scale_x_continuous(limits = c(0.45, 0.645), labels = scales::number_format(accuracy = 0.01),
                     breaks = c(0.45, 0.475, 0.50, 0.525, 0.55, 0.575, 0.60, 0.625, 0.65)) +
  scale_y_continuous(limits = c(1.28, 1.43), labels = scales::number_format(accuracy = 0.01),
                     breaks = c(1.28, 1.30, 1.32, 1.34, 1.36, 1.38, 1.40, 1.42)) +
  scale_color_manual(values = c("coral", "darkslategrey")) +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  stat_regline_equation(data = island, label.x = 0.595, label.y = 1.305, aes(label = ..eq.label..)) +
  stat_regline_equation(data = island, label.x = 0.595, label.y = 1.295, aes(label = ..rr.label..)) +
  stat_regline_equation(data = mainland, label.x = 0.46, label.y = 1.415, aes(label = ..eq.label..)) +
  stat_regline_equation(data = mainland, label.x = 0.46, label.y = 1.405, aes(label = ..rr.label..)) +
  xlab("Log Body Mass (kg^1/3)") +
  ylab("Log Foramen Magnum Surface Area (mm^1/2)") +
  guides(
    fill = guide_legend(title = "Location",
                        override.aes = aes(label = ""))) + # this is to get rid of the weird little "a" on the legend labels
  theme_bw()

# plot
plot(FMA_BM_regression_color)

# save plot
ggsave(paste0(plot_folder, "FMA_BM_regression.jpeg"), FMA_BM_regression_color,
       device = "jpeg", units = "in", width = 8, height = 5, dpi = 300)

#==============================================================================#
# B&W PLOT (with no equation or r^2; to be added later in PowerPoint, with SE.
# Also no legend)
# ggplot object
FMA_BM_regression_bw <- ggplot(data = data, aes(x = logBM, y = logFMA, group = Location, color = Location, fill = Location, lty = Location)) +
  geom_point(aes(color = Location, shape = Location)) +
  scale_x_continuous(limits = c(0.45, 0.645), labels = scales::number_format(accuracy = 0.01),
                     breaks = c(0.45, 0.475, 0.50, 0.525, 0.55, 0.575, 0.60, 0.625, 0.65)) +
  scale_y_continuous(limits = c(1.28, 1.43), labels = scales::number_format(accuracy = 0.01),
                     breaks = c(1.28, 1.30, 1.32, 1.34, 1.36, 1.38, 1.40, 1.42)) +
  scale_color_manual(values = c("black", "black")) +
  scale_shape_manual(values = c(1, 16)) +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  xlab("Log Body Mass (kg^1/3)") +
  ylab("Log Foramen Magnum Surface Area (mm^1/2)") +
  guides(
    fill = guide_legend(title = "Location",
                        override.aes = aes(label = ""))) + # this is to get rid of the weird little "a" on the legend labels
  theme_classic() +
  theme(legend.position = "none")

# plot
FMA_BM_regression_bw

# save plot
ggsave(paste0(plot_folder, "FMA_BM_regression.tiff"), FMA_BM_regression_bw,
       device = "tiff", units = "mm", width = 174, height = 124, dpi = 1200)

#==============================================================================#
################################################################################
#==============================================================================#
# We gonna do a FMA_OC plot for Reviewer 2
# B&W PLOT (with no equation or r^2; to be added later in PowerPoint, with SE.
# Also no legend)

# Add log of OC_mm first
class(data$OC_mm)
data$OC_mm <- as.numeric(data$OC_mm) # character -> numeric
class(data$OC_mm) # looks good
data$logOC <- log10(data$OC_mm^(1/3)) # see logBM above; "^1/3" is Köhler's transformation I think

# re-make to have logOC
island <- filter(data, Location == "Island")
mainland <- filter(data, Location == "Mainland")

# get equations
eq_island_OC <- lm(logFMA ~ logOC, island)
summary(eq_island_OC)

eq_mainland_OC <- lm(logFMA ~ logOC, mainland)
summary(eq_mainland_OC)

# ggplot object
FMA_OC_regression_bw <- ggplot(data = data, aes(x = logOC, y = logFMA, group = Location, color = Location, fill = Location, lty = Location)) +
  geom_point(aes(color = Location, shape = Location)) +
  scale_x_continuous(limits = c(0.525, 0.595), labels = scales::number_format(accuracy = 0.01),
                     breaks = c(0.52, 0.53, 0.54, 0.55, 0.56, 0.57, 0.58, 0.59, 0.60)) +
  scale_y_continuous(limits = c(1.28, 1.43), labels = scales::number_format(accuracy = 0.01),
                     breaks = c(1.28, 1.30, 1.32, 1.34, 1.36, 1.38, 1.40, 1.42)) +
  scale_color_manual(values = c("black", "black")) +
  scale_shape_manual(values = c(1, 16)) +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  xlab("Log Occipital Condyle Width (mm^1/3)") +
  ylab("Log Foramen Magnum Surface Area (mm^1/2)") +
  guides(
    fill = guide_legend(title = "Location",
                        override.aes = aes(label = ""))) + # this is to get rid of the weird little "a" on the legend labels
  theme_classic() +
  theme(legend.position = "none")

# plot
FMA_OC_regression_bw

# save
ggsave(paste0(plot_folder, "FMA_OC_regression.tiff"), FMA_OC_regression_bw,
       device = "tiff", units = "mm", width = 174, height = 124, dpi = 1200)

