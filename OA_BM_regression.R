# BLAKELY DEER
# Orbital surface area to body mass regression (populations split)
# Claire Geiman and Dr. Eric Long
# September 23, 2022

# set working directory
setwd(dir = "~/Desktop/Research/Odocoileus hemionus research")

# set plot_folder
plot_folder <- "03. Plots/"

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
# ORBITAL WIDTH

# function to add LOW: if ROW_mm is NA, use LOW_mm as OW (otherwise use the mean of ROW_mm and LOW_mm)
LOW_function = function(data){
  ifelse(is.na(data$ROW_mm) == TRUE, data$LOW_mm, (data$ROW_mm + data$LOW_mm)/2)
}
# apply LOW_function to a new column
data$OW <- LOW_function(data)

# function to replace all NAs with ROW_mm values
ROW_function = function(data){
  ifelse(is.na(data$OW) == TRUE, data$ROW_mm, data$OW)
}

# apply ROW_function to a new column
data$OW_mm <- ROW_function(data)

#==============================================================================#
# ORBITAL HEIGHT

# function to add LOH: if ROH is NA, use LOH_mm as OW (otherwise use the mean of ROH_mm and LOH_mm))
LOH_function = function(data){
  ifelse(is.na(data$ROH_mm) == TRUE, data$LOH_mm, (data$ROH_mm + data$LOH_mm)/2)
}

# apply LOH_function to a new column
data$OH <- LOH_function(data)

# function to replace all NAs with ROH_mm values
ROH_function = function(data){
  ifelse(is.na(data$OH) == TRUE, data$ROH_mm, data$OH)
}

# apply ROH_function to a new column
data$OH_mm <- ROH_function(data)

#==============================================================================#
# ORBITAL SURFACE AREA

# multiply data$OW_mm*data$OH_mm to get orbital surface area (OA)
data$OA_mm <- data$OW_mm*data$OH_mm

# filter dataframe for no NAs in OA
summary(data) # 6 NAs
data <- filter(data, !is.na(OA_mm))
summary(data) # no NAs

#==============================================================================#
# LOG BODY MASS
data$logBM <- log10(data$BM_kg^(1/3))

# LOG ORBITAL SURFACE AREA
data$logOA <- log10(data$OA_mm^(1/2))

#==============================================================================#
# create population groups for separate regression lines
island <- filter(data, Location == "Island")
mainland <- filter(data, Location == "Mainland")

#==============================================================================#
# find regline equations, including standard error for slope and intercept
# island
eq_island <- lm(logOA ~ logBM, island)
coef(eq_island) # slope = 0.44, intercept = 1.34
summary(eq_island) # y = 0.44x (+/-0.07) + 1.34 (+/-0.04)
# mainland
eq_mainland <- lm(logOA ~ logBM, mainland)
coef(eq_mainland) # slope = 0.10, intercept = 1.53
summary(eq_mainland) # y = 0.10x (+/-0.18) + 1.53 (+/-0.10)

# WARNING: I got non-identical values for the mainland equation (compared to Eric's)
# UPDATE 23/04/12: He says it's okay; likely just rounding error

#==============================================================================#
# specify decimal places for the regline equations
# view ggpubr source code:
trace(ggpubr:::.stat_lm, edit = TRUE)

# in pop-up window, you’ll see lines 13-14:

### => eq.char <- as.character(signif(polynom::as.polynomial(coefs), 
###                            2))

# in order to modify package ggpubr so that it reports regline equations with 2 decimal places (instead of the default 2 sig figs), modify the above code to be:

### => eq.char <- as.character(round(polynom::as.polynomial(coefs), 
###                            2))

#==============================================================================#
# COLOR PLOT
# ggplot object
OA_BM_regression_color <- ggplot(data = data, aes(x = logBM, y = logOA, group = Location, color = Location, fill = Location)) +
  geom_point(aes(color = Location)) +
  scale_x_continuous(limits = c(0.45, 0.645), labels = scales::number_format(accuracy = 0.01),
                     breaks = c(0.45, 0.475, 0.50, 0.525, 0.55, 0.575, 0.60, 0.625, 0.65)) +
  scale_y_continuous(limits = c(1.5, 1.65), labels = scales::number_format(accuracy = 0.01),
                     breaks = c(1.5, 1.525, 1.55, 1.575, 1.6, 1.625, 1.65)) +
  scale_color_manual(values = c("coral", "darkslategrey")) +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  stat_regline_equation(data = island, label.x = 0.595, label.y = 1.525, aes(label = ..eq.label..)) +
  stat_regline_equation(data = island, label.x = 0.595, label.y = 1.515, aes(label = ..rr.label..)) +
  stat_regline_equation(data = mainland, label.x = 0.46, label.y = 1.635, aes(label = ..eq.label..)) +
  stat_regline_equation(data = mainland, label.x = 0.46, label.y = 1.625, aes(label = ..rr.label..)) +
  xlab("Log Body Mass (kg^1/3)") +
  ylab("Log Orbital Surface Area (mm^1/2)") +
  guides(
    fill = guide_legend(title = "Location",
                        override.aes = aes(label = ""))) + # this is to get rid of the weird little "a" on the legend labels
  theme_bw()

# plot
plot(OA_BM_regression_color)

# save plot
ggsave(paste0(plot_folder, "OA_BM_regression.jpeg"), OA_BM_regression_color,
       device = "jpeg", units = "in", width = 8, height = 5, dpi = 300)
       
# WARNING: I got non-identical values for the mainland equation (compared to Eric's)

#==============================================================================#
# B&W PLOT (with no equation or r^2; to be added later in PowerPoint, with SE.
# Also no legend)
# ggplot object
OA_BM_regression_bw <- ggplot(data = data, aes(x = logBM, y = logOA, group = Location, color = Location, fill = Location, lty = Location)) +
  geom_point(aes(color = Location, shape = Location)) +
  scale_x_continuous(limits = c(0.45, 0.645), labels = scales::number_format(accuracy = 0.01),
                     breaks = c(0.45, 0.475, 0.50, 0.525, 0.55, 0.575, 0.60, 0.625, 0.65)) +
  scale_y_continuous(limits = c(1.5, 1.65), labels = scales::number_format(accuracy = 0.01),
                     breaks = c(1.5, 1.525, 1.55, 1.575, 1.6, 1.625, 1.65)) +
  scale_color_manual(values = c("black", "black")) +
  scale_shape_manual(values = c(1, 16)) +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  xlab("Log Body Mass (kg^1/3)") +
  ylab("Log Orbital Surface Area (mm^1/2)") +
  guides(
    fill = guide_legend(title = "Location",
                        override.aes = aes(label = ""))) + # this is to get rid of the weird little "a" on the legend labels
  theme_classic() +
  theme(legend.position = "none")

# plot
OA_BM_regression_bw

# save plot
ggsave(paste0(plot_folder, "OA_BM_regression.tiff"), OA_BM_regression_bw,
       device = "tiff", units = "mm", width = 174, height = 124, dpi = 1200)

#==============================================================================#
################################################################################
#==============================================================================#
# We gonna do a OA_OC plot for Reviewer 2
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
eq_island_OC <- lm(logOA ~ logOC, island)
summary(eq_island_OC)

eq_mainland_OC <- lm(logOA ~ logOC, mainland)
summary(eq_mainland_OC)

# ggplot object
OA_OC_regression_bw <- ggplot(data = data, aes(x = logOC, y = logOA, group = Location, color = Location, fill = Location, lty = Location)) +
  geom_point(aes(color = Location, shape = Location)) +
  scale_x_continuous(limits = c(0.53, 0.595), labels = scales::number_format(accuracy = 0.01),
                     breaks = c(0.53, 0.54, 0.55, 0.56, 0.57, 0.58, 0.59)) +
  scale_y_continuous(limits = c(1.5, 1.65), labels = scales::number_format(accuracy = 0.01),
                     breaks = c(1.5, 1.525, 1.55, 1.575, 1.6, 1.625, 1.65)) +
  scale_color_manual(values = c("black", "black")) +
  scale_shape_manual(values = c(1, 16)) +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  xlab("Log Occipital Condyle Width (mm^1/3)") +
  ylab("Log Orbital Surface Area (mm^1/2)") +
  guides(
    fill = guide_legend(title = "Location",
                        override.aes = aes(label = ""))) + # this is to get rid of the weird little "a" on the legend labels
  theme_classic() +
  theme(legend.position = "none")

# plot
OA_OC_regression_bw

# save
ggsave(paste0(plot_folder, "OA_OC_regression_bw.jpeg"), OA_OC_regression_bw,
       device = "jpeg", units = "in", width = 7, height = 5, dpi = 300)
