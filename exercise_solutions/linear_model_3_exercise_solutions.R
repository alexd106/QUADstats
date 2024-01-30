## ----Q2, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------
loyn <- read.table("./data/loyn.txt", header = TRUE)
str(loyn)


## ----Q3, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------
loyn$LOGAREA <- log10(loyn$AREA)
# create factor GRAZE as it was originally coded as an integer
loyn$FGRAZE <- factor(loyn$GRAZE)




## ----Q5, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------
birds.inter.1 <- lm(ABUND ~ FGRAZE + LOGAREA + FGRAZE:LOGAREA, data = loyn)

# Or use the 'shortcut' - its is equivalent to the model above
# birds.inter.1 <- lm(ABUND ~ FGRAZE * LOGAREA, data = loyn)




## ----Q7, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------
anova(birds.inter.1)

# The null hypothesis is that there is no significant interaction between 
# FGRAZE and LOGAREA. 
# As the P value is smaller than our cutoff of 0.05 (p = 0.005) we reject the 
# null hypothesis and conclude that there is a significant interaction. 

# This means that there is a significant relationship between bird abundance 
# and log area, and that this relationship is different for different levels of 
# graze (at least one of them is different). Put another way, the slopes of the 
# relationship between abundance and log area for each level of graze are different.

# As there is a significant interaction, it's difficult to interpret the main
# effects of FGRAZE and LOGAREA as by definition the effect of one variable is 
# dependent on the value of the other variable.


## ----Q8, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=TRUE----------------------------------------------------------------
summary(birds.inter.1)

# (Intercept)
# Here the Intercept (baseline) is the predicted ABUND when LOGAREA = 0,
# for FGRAZE level 1.
# the null hypothesis for the intercept is that the intercept = 0
# As the P value < 0.05 (7.09e-08) we reject this null hypothesis.

# LOGAREA 
# Represents the slope of the relationship between ABUND and LOGAREA, 
# specific to FGRAZE = 1.
# The null hypothesis is that the slope of the relationship
# between LOGAREA and ABUND = 0, for level FGRAZE = 1 only. 
# So, for graze 1 the slope is 4.14. This means, that for a 1 unit increase in LOGAREA
# we get a corresponding increase of 4.4 birds on average.
# As the P value (0.022) is < 0.05 we conclude that the slope is
# significantly different than 0.

# FGRAZE2
# Is the estimated difference (contrasts) between the *intercept* of FGRAZE level
# 2 and the reference level intercept, FGRAZE = 1.
# The null hypothesis associated with this estimate is that the difference
# in the intercepts between graze level 1 and graze level 2 = 0.
# As the P value (0.118) is > 0.05 we fail to reject this null hypothesis and
# conclude that the intercepts for graze level 1 and graze level 2 are the same.

# FGRAZE3, FGRAZE4, FGRAZE5
# The parameter estimates have the same interpretation as for FGRAZE2 (above). 
# They are all estimates of the difference between FGRAZE at the appropriate level 
# and FGRAZE 1 (Intercept).

# FGRAZE2:LOGAREA
# This represents the difference in the slope of the relationship between ABUND 
# and LOGAREA between graze level 2 and graze level 1.
# The null hypothesis is that the difference in slopes between graze level 1 
# and 2 = 0 (i.e. no difference).
# As the P value (0.082) > 0.05 we conclude that the slopes are not different 
# (i.e. they are the same).

# FGRAZE3:LOGAREA
# This represents the difference in the slope of the relationship between ABUND 
# and LOGAREA between graze level 3 and graze level 1.
# The null hypothesis is that the difference in slopes between graze level 1 
# and 3 = 0 (i.e. no difference).
# As the P value (0.004) < 0.05 we conclude that the slopes are different (and
# therefore the relationship is different).

# FGRAZE4:LOGAREA, # FGRAZE5:LOGAREA
# These parameter estimates and null hypotheses are interpreted in the same way 
# as for FGRAZE4:LOGAREA and FGRAZE5:LOGAREA


# The Multiple R-square value is 0.79, so 79% of the variation in the data is 
# explained by the model. This is quite a bit more than the models with only
# LOGAREA and FGRAZE as single explanatory variables.


## ----Q9a, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=FALSE--------------------------------------------------------------
par(mfrow= c(1, 1))
plot(ABUND ~ LOGAREA, data = loyn, col = GRAZE, pch = 16)
# Note: # colour 1 means black in R
# colour 2 means red in R
# colour 3 means green in R
# colour 4 means blue in R
# colour 5 means cyan in R

# FGRAZE1
# create a sequence of increasing LOGAREA within the observed range
LOGAREA.seq <- seq(from = min(loyn$LOGAREA[loyn$FGRAZE == 1]),
					to = max(loyn$LOGAREA[loyn$FGRAZE == 1]),
					length = 20)

# create data frame for prediction
dat4pred <- data.frame(FGRAZE = "1", LOGAREA = LOGAREA.seq)

# predict for new data
P1 <- predict(birds.inter.1, newdata = dat4pred)

# add the predictions to the plot of the data
lines(dat4pred$LOGAREA, P1, col = 1, lwd = 2)

# FGRAZE2
LOGAREA.seq <- seq(from = min(loyn$LOGAREA[loyn$FGRAZE == 2]),
					to = max(loyn$LOGAREA[loyn$FGRAZE == 2]),
					length = 20)

dat4pred <- data.frame(FGRAZE = "2", LOGAREA = LOGAREA.seq)

P2 <- predict(birds.inter.1, newdata = dat4pred)

lines(dat4pred$LOGAREA, P2, col = 2, lwd = 2)

# FGRAZE3
LOGAREA.seq <- seq(from = min(loyn$LOGAREA[loyn$FGRAZE == 3]),
					to = max(loyn$LOGAREA[loyn$FGRAZE == 3]),
					length = 20)

dat4pred <- data.frame(FGRAZE = "3", LOGAREA = LOGAREA.seq)

P3 <- predict(birds.inter.1, newdata = dat4pred)

lines(dat4pred$LOGAREA, P3, col = 3, lwd = 2)

# FGRAZE4
LOGAREA.seq <- seq(from = min(loyn$LOGAREA[loyn$FGRAZE == 4]),
					to = max(loyn$LOGAREA[loyn$FGRAZE == 4]),
					length = 20)

dat4pred <- data.frame(FGRAZE = "4", LOGAREA = LOGAREA.seq)

P4 <- predict(birds.inter.1, newdata = dat4pred)

lines(dat4pred$LOGAREA, P4, col = 4, lwd = 2)

# FGRAZE5
LOGAREA.seq <- seq(from = min(loyn$LOGAREA[loyn$FGRAZE == 5]),
					to = max(loyn$LOGAREA[loyn$FGRAZE == 5]),
					length = 20)

dat4pred <- data.frame(FGRAZE = "5", LOGAREA = LOGAREA.seq)

P5 <- predict(birds.inter.1, newdata = dat4pred)

lines(dat4pred$LOGAREA, P5, col = 5, lwd = 2)

legend("topleft", 
 legend = paste("Graze = ", 5:1), 
 col = c(5:1), bty = "n",
 lty = c(1, 1, 1), 
 lwd = c(1, 1, 1))


## ----Q9b, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=FALSE--------------------------------------------------------------
# Okay, that was a long-winded way of doing this.
# If, like me, you prefer more compact code and less risks of errors,
# you can use a loop, to save repeating the sequence 5 times:
par(mfrow = c(1, 1))
plot(ABUND ~ LOGAREA, data = loyn, col = GRAZE, pch = 16)

for(g in levels(loyn$FGRAZE)){ # g will take the values "1", "2",..., "5" in turn
	LOGAREA.seq <- seq(from = min(loyn$LOGAREA[loyn$FGRAZE == g]),
										to = max(loyn$LOGAREA[loyn$FGRAZE == g]),
														length = 20)
	dat4pred <- data.frame(FGRAZE = g, LOGAREA = LOGAREA.seq)
	predicted <- predict(birds.inter.1, newdata = dat4pred)
	lines(dat4pred$LOGAREA, predicted, col = as.numeric(g), lwd = 2)
}
legend("topleft", 
 legend = paste("Graze = ", 5:1), 
 col = c(5:1), bty= "n",
 lty = c(1, 1, 1), 
 lwd = c(1, 1, 1))


## ----Q9c, eval=TRUE, echo=SOLUTIONS, results=SOLUTIONS, collapse=FALSE--------------------------------------------------------------
# install.packages('ggplot2', dep = TRUE)
library(ggplot2)

ggplot(loyn, aes(x = LOGAREA, y = ABUND, colour = FGRAZE) ) +
     geom_point() +
     geom_smooth(method = "lm", se = FALSE)

