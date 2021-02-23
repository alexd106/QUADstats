## ----Q2, eval=TRUE, echo=TRUE, collapse=TRUE----------------------------------------------------------------------------
loyn <- read.table("./data/loyn.txt", header = TRUE, 
                   stringsAsFactors = TRUE)
str(loyn)

# 56 observations and 8 variables (from str())


## ----Q3, eval=TRUE, echo=TRUE, collapse=TRUE----------------------------------------------------------------------------
# let's first log10 transform the AREA variable due to the couple 
# of unusually large forest area values (check you data exploration)
# to remind yourself

loyn$LOGAREA <- log10(loyn$AREA)

# now fit the linear model

loyn.lm <- lm(ABUND ~ LOGAREA, data = loyn)


## ----Q4, eval=TRUE, echo=TRUE, collapse=TRUE----------------------------------------------------------------------------
# ANOVA table
anova(loyn.lm)

# The null hypothesis is that the slope of the relationship
# between LOGAREA and ABUND = 0
# i.e. there is no relationship

# The p value is very small (7.178e-11) therefore we 
# reject this null hypothesis (i.e. the slope is different
# from 0)


## ----Q5, eval=TRUE, echo=TRUE, collapse=TRUE----------------------------------------------------------------------------
summary(loyn.lm)

# the estimate of the intercept = 10.4
# the estimate of the slope = 9.78

# word equation
# ABUND = 10.40 + 9.78 * LOGAREA


## ----Q6, eval=TRUE, echo=TRUE, collapse=TRUE----------------------------------------------------------------------------

# the null hypothesis for the intercept is that the intercept = 0
# the p value is very small (certainly less than the (not so magic) 0.05)
# therefore we reject this null hypothesis and conclude that the intercept
# is different from zero.

# the null hypothesis for the slope is that the slope = 0
# the p value is very small (7.18e-11)
# therefore we reject this null hypothesis and conclude that the slope
# is different from zero (i.e. there is a significant relationship between
# LOGAREA and ABUND).


## ----Q7, eval=TRUE, echo=TRUE, collapse=TRUE----------------------------------------------------------------------------
summary(loyn.lm)

# The multiple R-squared value is 0.548 and therefore 54.8% of
# the variation in ABUND is explained by LOGAREA


## ----Q8, eval=TRUE, echo=TRUE, collapse=TRUE----------------------------------------------------------------------------
# first split the plotting device into 2 rows and 2 columns
par(mfrow = c(2,2))

# now create the residuals plots
plot(loyn.lm)

# The normal Q-Q plot is used to assess normality of the residuals (top right).
# If perfectly normal then all residuals would lie exactly on the 1:1 line.
# In reality this is never going to happen so we are looking for obvious large 
# departures. In this plot they don't look too bad, although the lower quantiles
# do seem to deviate. All in all I would be cautiously optimistic.

# To check homogeneity of variance assumption we look at both the Residuals vs Fitted (top left)
# and the Scale-Location plots (bottom left). 
# There doesn't appear to be any obvious patterns of the residuals in these plots, 
# although you can see the two residuals associated with the two large forest patch areas 
# are below the zero line (negative residuals) on the right hand side of these plots
# despite the log transformation. Perhaps something to bear in mind as we progress. 
# But in short, the homogeneity of variance assumption looks ok.

# From the Residuals vs Leverage plot (bottom right) you can see that there are no residuals 
# with a Cooks distance greater than 1. In fact they are all well below 0.5. If you want to 
# produce a plot of just Cooks distance (perhaps this is clearer)
par(mfrow = c(1,1))
plot(loyn.lm, which = 4)

# Going back to the Residuals vs Leverage plot (bottom right) you can see 
# three residuals that re somewhat unusual in terms of their leverage as 
# they stick out a bit to the right compared to the rest of the residuals. 
# Two of these residuals are our two large forest patch AREAs again. However,
# leverage still seems to be on the low side, so at least on the scale of the
# log transformation things look ok.


## ----Q9, eval=TRUE, echo=TRUE, collapse=TRUE----------------------------------------------------------------------------
# to predict bird abundance if AREA == 100

# if you log base 10 transformed the AREA variable
10.4 + (9.78 * log10(100))

# if you used the natural log (i.e. log()) then you would use log()
# not log10()


## ----Q10, eval=TRUE, echo=TRUE, collapse=TRUE---------------------------------------------------------------------------
# need to create a dataframe object with a column of LOGAREA values to predict from.
# note you need to call the column the same as used in the model
my.data <- data.frame(LOGAREA = seq(from = min(loyn$LOGAREA),
                                  to = max(loyn$LOGAREA),
                                  length = 50))

# use predict function to calculate predicted values of abundance based on the 
# new LOGAREA values in the data frame my.data (use the newdata argument)
pred.vals <- predict(loyn.lm, newdata = my.data)


## ----Q11, eval=TRUE, echo=TRUE, collapse=TRUE---------------------------------------------------------------------------
# plot the lines on the plot. The x values are the new LOGAREA values from the my.data
# dataframe, the predicted values are from pred.vals 
plot(loyn$LOGAREA, loyn$ABUND, xlab = "Log10 Patch Area", ylab = "Bird Abundance")

lines(my.data$LOGAREA, pred.vals, lty = 1,col = 2)


## ----Q12, eval=TRUE, echo=TRUE, collapse=TRUE---------------------------------------------------------------------------
pred.vals.se <- predict(loyn.lm, newdata = my.data, se.fit = TRUE) # note the use of the se.fit argument

# check out the structure of pred.vals.se
# you can see we now have 4 vectors in this object
# $fit = fitted values
# $se.fit = standard error of fitted values
# $df = degrees of freedom
# $residual.scale = residual standard error

# so we will need to access our fitted values and standard errors using 
# pred.val.se$fit and pred.vals.se$se.fit respectively
str(pred.vals.se)

# now create the plot
plot(x = loyn$LOGAREA, y = loyn$ABUND,
     xlab = "Log10 Patch Area",
     ylab = "Bird abundance")

# add the fitted values as before but now we need to use 
# pred.vals.se$fit
lines(my.data$LOGAREA, pred.vals.se$fit, lty = 1,col = 1)

# add the upper 95% confidence interval
lines(my.data$LOGAREA, pred.vals.se$fit + (1.96 * pred.vals.se$se.fit), lty = 2, col = 2)

# add the lower 95% confidence interval
lines(my.data$LOGAREA, pred.vals.se$fit - (1.96 * pred.vals.se$se.fit), lty = 2, col = 2)


## ----Q13, eval=TRUE, echo=TRUE, collapse=TRUE---------------------------------------------------------------------------
# back transformed LOGAREA and 95% confidence intervals

# re-plot but this time use the original untransformed AREA variable
plot(x = loyn$AREA, y = loyn$ABUND, ylim = c(0,55),
     xlab = "Patch Area",
     ylab = "Bird abundance")

# now add the fitted lines and upper and lower 95% confidence intervals
# remember we only log10 transformed the LOGAREA variable so this is the only
# variable that we need to back-transform

lines(10^(my.data$LOGAREA), pred.vals.se$fit, lty = 1,col = 1)
lines(10^(my.data$LOGAREA), pred.vals.se$fit + (1.96 * pred.vals.se$se.fit), lty = 2, col = "red")
lines(10^(my.data$LOGAREA), pred.vals.se$fit - (1.96 * pred.vals.se$se.fit), lty = 2, col = "red")

# the model doesn't look too great now! Technically, if we transform variables in our model
# then we should only really interpret the model on the scale of the transformation.

# If we interpret on the back transformed scale, you can see the model doesn't really fit the 
# data very well, especially for the two large forest patch AREAs and also those patches with
# high bird abundance.

