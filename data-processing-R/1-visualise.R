library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

setwd("D:/CIAT/_tools/workshop_R")
# Minor fixes for the included data
weather <- read_csv("weather.csv", col_types = list(
  date = col_datetime("%Y-%m-%d %H:%M:%S"),
  precip = col_double(),
  visib = col_double()
)) %>% mutate(
  temp = (temp - 32) * 5 / 9,
  dewp = (dewp - 32) * 5 / 9
)

# A data frame has named columns, each column is the same length
weather
str(weather)
View(weather)

# Introduction to data frames --------------------------------------------------

daily <- filter(weather, hour == 12)
jfk <- filter(weather, origin == "JFK")

# Basic graphics ---------------------------------------------------------------
# What is a plot? Mapping between variables and aesthietcs.
# Three most useful graphics are:
# scatterplot, histogram & line chart

qplot(temp, humid, data = daily)
qplot(wind_speed,  data = daily)
qplot(date, temp, data = jfk, geom = "line")
# Add extra properties with shape or colour
qplot(temp, humid, data = daily, shape = origin)
qplot(temp, humid, data = daily, colour = origin)
qplot(temp, humid, data = daily, colour = dewp)+scale_color_continuous(low="yellow",high = "purple")

# Discrete data
qplot(wind_dir, wind_speed, data = daily)
set.seed(5)
qplot(wind_dir, wind_speed, data = daily, geom = "jitter")

# Line charts ------------------------------------------------------------------

qplot(date, temp, data = daily)
qplot(date, temp, data = daily, colour = origin)
qplot(date, temp, data = daily, colour = origin, geom = "line")

# Alternative to aesthetics
qplot(date, temp, data = daily, geom = "line") + facet_wrap(~ origin)

# Will learn how to do this later
daily <- daily %>% group_by(date) %>% mutate(temp_diff = temp - mean(temp))
qplot(date, temp_diff, data = daily, colour = origin, geom = "line")

qplot(hour, temp, data = jfk, geom = "line", group = interaction(month, day))
qplot(hour, temp, data = jfk, geom = "line", group = interaction(day, origin)) +
  facet_wrap(~month)

#personalize your plot
p <- ggplot(daily,aes(temp, humid)) + geom_point()
p <- p + xlab("Temperatura") + ylab("Humedad Relativa")
p + theme_bw()
p + theme_classic()
p + theme_dark()

p <- p + ggtitle("Gráfico de dispersión") + theme(plot.title = element_text(lineheight=.8))
p <- p + theme(plot.title = element_text(lineheight=.8, face="bold"))
p <- p + theme(panel.background = element_rect(fill = "white", colour = "grey50"))

print(p)

ggsave("plot.png", width = 5, height = 5)


# Histograms -------------------------------------------------------------------

qplot(wind_speed, data = daily)
qplot(wind_dir, data = daily)
qplot(wind_dir, data = daily, binwidth = 5)

qplot(precip, data = daily)
qplot(precip, data = daily, binwidth = 0.01)

# ALWAYS EXPERIMENT WITH BIN WIDTH
# What's strange about temp and why?
qplot(temp, data = jfk)
qplot(temp, data = jfk, binwidth = 1)
qplot(temp, data = jfk, binwidth = 0.1)

