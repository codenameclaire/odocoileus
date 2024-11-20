# BLAKELY DEER
# Occipital condyle width to body mass regression (museum data)
# Claire Geiman and Dr. Eric Long
# September 15, 2022
# Updated Jan 18, 2023: for Pearson correlation
# Updated April 7, 2023: for additional plot with black-/white-tailed deer denoted
# Updated May 26, 2023: combine three transparent plots into one with regline.

# set working directory
setwd(dir = "~/Desktop/Research/Odocoileus hemionus research/")

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
data <- read.csv("01. Data/csv/Occipital Condyle vs Body Mass BTD WTD.csv")

#==============================================================================#
# identify the four types of deer in a new column
data$Group <- NA

data$Group <- ifelse(data$Species == "O. hemionus" & data$Sex == "M", "MBTD",
                     ifelse(data$Species == "O. hemionus" & data$Sex == "F", "FBTD",
                            ifelse(data$Species == "O. virginianus" & data$Sex == "M", "MWTD", "FWTD")))

#==============================================================================#
# find regline equation, including standard error for slope and intercept
eq <- lm(OC_mm ~ BM_kg, data)
coef <- coef(eq) # slope = 0.27, intercept = 34.17
std_error <- summary(eq)$coef[, "Std. Error"]
summary(eq) # y = 0.27x (+/-0.08) + 34.17 (+/-4.30)

regression_eq <- paste("OCW = ", round(coef[2], 2), "BM", "(+/-", round(std_error[2], 2), ")",
                       " + ", 
                       round(coef[1], 2), "(+/-", round(std_error[1], 2), ")")

#==============================================================================#
# Since we want a Pearson correlation, let's check that before we get to plotting

# To find R value:
cor(data$BM_kg, data$OC_mm, method = "pearson") # returns 0.61 for R value; 
# matches Eric's value, yay!

# To find p-value:
cor.test(data$BM_kg, data$OC_mm, method = "pearson") # returns 0.004649 for p-value;
# matches Eric's value, yay!

#==============================================================================#
# specify decimal places for the regline equations
# view ggpubr source code:
trace(ggpubr:::.stat_lm, edit = TRUE)

# in pop-up window, you’ll see lines 13-14:

### => eq.char <- as.character(signif(polynom::as.polynomial(coefs), 2))

# in order to modify package ggpubr so that it reports regline equations with 2 decimal places (instead of the default 2 sig figs), modify the above code to be:

### => eq.char <- as.character(round(polynom::as.polynomial(coefs), 2))


#==============================================================================#
# COMBINED PLOT (FINAL)
OC_BM_regression <- ggplot(data = data, aes(x = BM_kg, y = OC_mm)) +
  geom_point(color = "black", size = 2.75, aes(shape = Group, fill = Group)) +
  scale_x_continuous(limits = c(25, 75), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(limits = c(40, 56), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_manual(values = c("black", "black", "black", "black")) +
  scale_fill_manual(values = c("black", "black","black", "black")) +
  scale_shape_manual(values = c(21, 1, 22, 0)) + # female BTD black circle, male BTD black square
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, color = "grey60", linetype = "solid", ) + 
  xlab("Body Mass (kg)") +
  ylab("Occipital Condyle Width (mm)") +
  theme_classic() +
  theme(legend.position = "none")

# plot
OC_BM_regression

# save
ggsave(paste0(plot_folder, "OC_BM_regression.tiff"), OC_BM_regression,
       device = "tiff", units = "mm", width = 174, height = 124, dpi = 1200)

