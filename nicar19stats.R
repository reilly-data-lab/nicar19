# ----------------------------------------------------------------------------------
# | AN INTRODUCTION TO STATISTICS IN R
# | STEVE REILLY (sreilly@usatoday.com, @BySteveReilly)
# | NICAR 2019, NEWPORT BEACH, CA
# --------------------------------------------------------------------------------
# | CONTENTS
# | 
# | We will use this R script to analyze the following datasets:
# | 
# | 1. Diamond prices (pre-loaded practice dataset)
# | 2. Massachusetts school test scores (pre-loaded practice dataset)
# | 3. California school test scores (pre-loaded practice dataset)
# | 4. NFL game TV ratings and 2016 presidential election results, 2016-2017 (USA Today analysis)
# -------------------------------------------------------------------------------
# | SUPPLEMENTARY MATERIAL:
# | 
# | - Notebook: https://steve-reilly.github.io/nicar19/nicar19.html 
# | - GitHub repo: https://github.com/steve-reilly/nicar19 
# | - Slides: tinyurl.com/ire2019rstats 
# -------------------------------------------------------------------------------

## Load libraries
library("Ecdat")
library("ggplot2")

#############################
# Analysis 1: Diamond Prices #
#############################

#Load and explore data 
data(Diamond)
View(Diamond)
?Diamond
head(Diamond,5)
tail(Diamond)
names(Diamond)
str(Diamond)
summary(Diamond)

# Basic visualization with trendline
plot(Diamond$carat, Diamond$price, main = "Diamond Carats vs. price", xlab = 'Carat', ylab = 'Price', pch = 21, bg = 'gold', ylim = c(0,16000))
abline(lm(Diamond$price~Diamond$carat))
 
# Visualize with ggplot
ggplot(Diamond,aes(x=carat,y=price))+ 
  geom_point(colour="red",
             alpha = 0.2,
             size = 2) + 
  geom_smooth(method="lm") +
  labs(x= "Carat Size",
       y = "Price ($)",
       title = "Diamond Carats vs. price")
 
# Linear regresion analysis
lm.out <- lm(Diamond$price~Diamond$carat)
summary(lm.out)

########################################## 
## Analysis 2: Massachusetts Test Results #
##########################################

### Explore the data frame
data(MCAS)
?MCAS
View(MCAS)
head(MCAS)
tail(MCAS)
names(MCAS)
summary(MCAS) 
str(MCAS)

### Visualize the data 

# Create a histogram.
a <- ggplot(MCAS,aes(totsc4)) 
a + geom_histogram(binwidth=10,color="black",fill="light blue") +
scale_x_continuous(breaks=seq(0,50,10))+
labs(x= "Fourth-grade test scores",
y = "Student-teacher ratio",
title = "Fourth-grade test scores and student-teacher ratio")

# View scatterplot matrix
plot(MCAS[,c(4,11:14,16)])

# View scatterplot
ggplot(MCAS,aes(x=totsc4,y=tchratio))+ geom_point() + geom_smooth(method="lm") +
  labs(x= "Fourth-grade test scores",
       y = "Student-teacher ratio",
       title = "Fourth-grade test scores and student-teacher ratio")
 
### Regression analysis

# Bivariate regression: Test scores and student-teacher ratio
Model1 <-lm(MCAS$totsc4~MCAS$tchratio)
summary(Model1)
 
# Linear regression: Test scores and student-teacher ratio+spending per pupil
Model2 <-lm(MCAS$totsc4~MCAS$tchratio+MCAS$regday)
summary(Model2)
 
# Linear regression: Test scores and student-teacher ratio+spending per pupil+percent of students receiving free lunch
Model3 <-lm(MCAS$totsc4~MCAS$tchratio+MCAS$percap+MCAS$lnchpct)
summary(Model3)

#######################################
## Analysis 3: California test results#
#######################################

### Explore the data frame
data(Caschool)
View(Caschool)
names(Caschool)
str(Caschool)
summary(Caschool)
head(Caschool)

### Visualize the data

# Visualize individual variables with histograms
hist(Caschool$testscr, main="Test Scores")
hist(Caschool$avginc, main="Average Income")
hist(Caschool$compstu, main="Computers Per Student")
hist(Caschool$str, main="Student Teacher Ratio")
 
# View scatterplot matrix
plot(Caschool[,c(10,11,13,14)])
 
### Regression analysis 

# Linear regression: Test scores and student-teacher ratio
Model4 <-lm(Caschool$testscr~Caschool$str)
summary(Model4)

# Multiple regression: Test scores and student-teacher ratio + computers-per-student
Model5 <-lm(Caschool$testscr~Caschool$str+Caschool$compstu)
summary(Model5)

# Multiple regression analysis: Test scores and student-teacher ratio + computers-per-student + average income
Model6 <- lm(Caschool$testscr~Caschool$str+Caschool$compstu+Caschool$avginc)
summary(Model6)

##########################################
# Analysis 4: NFL TV Ratings and politics#
##########################################

### Import data
NFL.ratings <- read.csv("https://raw.githubusercontent.com/steve-reilly/nicar19/master/nfl_ratings.csv")
 
#### View summary statistics
View(NFL.ratings)
str(NFL.ratings)
summary(NFL.ratings)
 
#### Visualize data
ggplot(NFL.ratings,aes(y=NFL.ratings$ratings.change, x=NFL.ratings$trump.percent))+ geom_point() + geom_smooth(method="lm") + 
labs(x= "Percent of vote won by Trump (%)",
y = "Change in NFL ratings 2016-2017",
title = "Trump vote vs. change in NFL ratings")

### Linear regresion analysis: NFL ratings change vs. Trump voting percentage
lm.out <- lm(NFL.ratings$ratings.change~NFL.ratings$trump.percent)
summary(lm.out)
