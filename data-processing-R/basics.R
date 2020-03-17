## Hello world!
## Welcome to the R environment

## What is R?
# R is a language and environment for statistical computing and graphics.
# R provides a wide variety of statistical (linear and nonlinear modelling, classical statistical tests,
# time-series analysis, classification, clustering, .) and graphical techniques, and is highly extensible. 
# One of R's strengths is the ease with which well-designed publication-quality plots can be produced, 
# including mathematical symbols and formulae where needed.
# R is available as Free Software

## R Studio
# RStudio is an application in which you can load data, execute R commands, prepare scripts, get information 
# on commands and environments (the current state of your work), visualize graphics and more.

## The R studio interface

# The top-left pane is the editor, where you can write scripts, and other types of documents. 
# The bottom-left one contains the "Console", where you can directly type R commands.
# The top-right one contains information about the "Environment": values currently defined in your R session.
# The bottom-right one contains various tabs, including the following ones:
    # a file explorer
    # a help viewer
    # a plot visualizer
    # a package manager (packages are a way to extend the functionalities of R).

## Define variables

# Numbers
n <- 1

# Strings
s <- "string"

#Vector
v <- c(6, -1, 0, 6)

# Named vectors
v_n <- c(first=6, second=-1, third=0, fourth=6)

# Bolean
t <- F

#Sequences
seq <- 2:9

# Variable type
class(s)

# Length 
length(seq)

#Access elements of a vector
seq[4]

#Bolean sub-setting
seq_greater_4 <- seq > 4 

#Repeating values
seq_rep <- rep(1:2, times = 3)

#Sequences of values
seq_by_2 <- seq(0, 8, by = 2)

#Combining strings
str_com <- paste("I", "can", "paste", "words", "together")
str_com_0 <- paste0("I", "don't", "have", "space", "bar")

#Command help
?class

#Load data
bDir <- "C:/Users/cenavarro/Dropbox/Training Materials/Week_2/R_examples"
monthly_var <- read.csv(paste0(bDir, "/battambang_st.csv"), header=T)

yrs <- 1981:2010
monthly_var_yr <- monthly_var[ which(monthly_var$Year <= 2010),]
sum_AMJJASO <- rowSums(monthly_var_yr[,5:11])
monthly_var_yr$AMJJASO <- sum_AMJJASO

#Statistical Graphics

#Histogram
hist(sum_AMJJASO)

#Density
plot(density(sum_AMJJASO))

# Barplot
years <- monthly_var_yr$Year
barplot(sum_AMJJASO,names.arg=years,xlab="Years",ylab="Precipitation (mm)",col="blue",
        main="Accumulated Precipitation AMJJASO (mm)",border="red")

#Median, ranges, diff, std, quantile, interquantile range
median(sum_AMJJASO)
range(sum_AMJJASO)
diff(range(sum_AMJJASO))
sd(sum_AMJJASO)
quantile(sum_AMJJASO)
IQR(sum_AMJJASO)

#Simple Boxplot
boxplot(monthly_var_yr[,5:11])

#Scatter plot
plot(x = years, y = sum_AMJJASO)
summary(sum_AMJJASO)

