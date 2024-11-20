# BLAKELY DEER
# Occipital condyle width to body mass regression (museum data)
# Claire Geiman and Dr. Eric Long
# September 15, 2022
# Updated Jan 18, 2023: for Pearson correlation
# Updated April 7, 2023: for additional plot with black-/white-tailed deer denoted

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
# find regline equation, including standard error for slope and intercept
eq <- lm(OC_mm ~ BM_kg, data)
coef(eq) # slope = 0.27, intercept = 34.17
summary(eq) # y = 0.27x (+/-0.08) + 34.17 (+/-4.30)

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
# COLOR PLOT
# ggplot object
OC_BM_regression <- ggplot(data = data, aes(x = BM_kg, y = OC_mm)) +
  geom_point(color = "black") +
  scale_x_continuous(limits = c(25, 75), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(limits = c(40, 56), labels = scales::number_format(accuracy = 0.1)) +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, color = "coral") +
  stat_regline_equation(label.x = 27, label.y = 54, aes(label = ..eq.label..)) +
  stat_regline_equation(label.x = 27, label.y = 52.5, aes(label = ..rr.label..)) +
  xlab("Body Mass (kg)") +
  ylab("Occipital Condyle Width (mm)") +
  theme_bw()

# plot
plot(OC_BM_regression)

# save plot
ggsave(paste0(plot_folder, "OC_BM_regression.jpeg"), OC_BM_regression,
       device = "jpeg", units = "in", width = 8, height = 5, dpi = 300)

#==============================================================================#
# B&W PLOT (with no equation or r^2; to be added later in PowerPoint, with SE)
# ggplot object
OC_BM_regression_bw <- ggplot(data = data, aes(x = BM_kg, y = OC_mm)) +
  geom_point(color = "black") +
  scale_x_continuous(limits = c(25, 75), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(limits = c(40, 56), labels = scales::number_format(accuracy = 0.1)) +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, color = "grey60", linetype = "solid") +
  xlab("Body Mass (kg)") +
  ylab("Occipital Condyle Width (mm)") +
  theme_classic()

# plot
plot(OC_BM_regression_bw)

# save plot
ggsave(paste0(plot_folder, "OC_BM_regression_bw.jpeg"), OC_BM_regression_bw,
       device = "jpeg", units = "in", width = 7, height = 5, dpi = 300)

#==============================================================================#
# B&W PLOT (same as above plot, but with black- and white-tailed deer denoted separately)
# ggplot object
OC_BM_regression_bw_BTD_WTD <- ggplot(data = data, aes(x = BM_kg, y = OC_mm)) +
  geom_point(color = "black", aes(shape = Species, fill = Species, color = Species)) +
  scale_x_continuous(limits = c(25, 75), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(limits = c(40, 56), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("black", "black")) +
  scale_shape_manual(values = c(21, 24)) +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, color = "grey60", linetype = "solid") + 
  xlab("Body Mass (kg)") +
  ylab("Occipital Condyle Width (mm)") +
  theme_classic() +
  theme(legend.position = "none")

# plot
plot(OC_BM_regression_bw_BTD_WTD)

# save plot
ggsave(paste0(plot_folder, "OC_BM_regression_bw_BTD_WTD.jpeg"), OC_BM_regression_bw_BTD_WTD,
       device = "jpeg", units = "in", width = 7, height = 5, dpi = 300)

#==============================================================================#
#==============================================================================#
#==============================================================================#
# B&W PLOT (same as above plot, but with black- and white-tailed deer denoted separately,
# and by sex as well)

# So after extensive googling, I actually can't figure out how to make ggplot denote
# species AND sex separately. So what am I going to do? I am going to make three plots, 
# one for black-tailed deer, one for white tailed deer, and one for the regression line. 
# The secret is the plots will have transparent backgrounds, and I will layer them on 
# top of each other. I AM GENIUS.
# So let's make two dfs: one for BTD and one for WTD
data_btd <- filter(data, data$Species == "O. hemionus") # black-tailed deer
data_wtd <- filter(data, data$Species == "O. virginianus") # white-tailed deer

#==============================================================================#
# ggplot object for BTD
OC_BM_regression_bw_BTD <- ggplot(data = data_btd, aes(x = BM_kg, y = OC_mm)) +
  geom_point(color = "black", size = 2.75, aes(shape = Sex, fill = Sex, color = Sex)) +
  scale_x_continuous(limits = c(25, 75), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(limits = c(40, 56), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("black", "black")) +
  scale_shape_manual(values = c(21, 22)) + # female BTD black circle, male BTD black square
  #geom_smooth(method = lm, se = FALSE, fullrange = TRUE, color = "grey60", linetype = "solid") + 
  xlab("Body Mass (kg)") +
  ylab("Occipital Condyle Width (mm)") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

# plot
OC_BM_regression_bw_BTD

# save as png to test
ggsave(paste0(plot_folder, "OC_BM_regression_bw_BTD.png"), OC_BM_regression_bw_BTD,
       device = "png", units = "in", width = 7, height = 5, dpi = 300, bg = "transparent")

#==============================================================================#
# ggplot object for WTD
OC_BM_regression_bw_WTD <- ggplot(data = data_wtd, aes(x = BM_kg, y = OC_mm)) +
  geom_point(color = "black", size = 2.75, aes(shape = Sex, fill = Sex, color = Sex)) +
  scale_x_continuous(limits = c(25, 75), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(limits = c(40, 56), labels = scales::number_format(accuracy = 0.1)) +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("black", "black")) +
  scale_shape_manual(values = c(1, 0)) + # female WTD open circle, male WTD open square
  #geom_smooth(method = lm, se = FALSE, fullrange = TRUE, color = "grey60", linetype = "solid") + 
  xlab("Body Mass (kg)") +
  ylab("Occipital Condyle Width (mm)") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

# plot
OC_BM_regression_bw_WTD

# save as png to test
ggsave(paste0(plot_folder, "OC_BM_regression_bw_WTD.png"), OC_BM_regression_bw_WTD,
       device = "png", units = "in", width = 7, height = 5, dpi = 300, bg = "transparent")

#==============================================================================#
# ggplot object for regline
OC_BM_regression_bw_BTD_WTD <- ggplot(data = data, aes(x = BM_kg, y = OC_mm)) +
  scale_x_continuous(limits = c(25, 75), labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(limits = c(40, 56), labels = scales::number_format(accuracy = 0.1)) +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, color = "grey60", linetype = "solid") + 
  xlab("Body Mass (kg)") +
  ylab("Occipital Condyle Width (mm)") +
  theme_classic() +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

# plot
OC_BM_regression_bw_BTD_WTD

# save
ggsave(paste0(plot_folder, "OC_BM_regression_bw_BTD_WTD.png"), OC_BM_regression_bw_BTD_WTD,
       device = "png", units = "in", width = 7, height = 5, dpi = 300, bg = "transparent")
