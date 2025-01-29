## ----Q2, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE------------------------------------------------------------------------------------
# loyn <- read.table("data/loyn.txt", header = TRUE,
#                    stringsAsFactors = TRUE)
# str(loyn)


## ----Q3, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE------------------------------------------------------------------------------------
# # create factor GRAZE as it was originally coded as an integer
# loyn$FGRAZE <- factor(loyn$GRAZE)
# 
# # check this
# class(loyn$FGRAZE)


## ----Q4, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE------------------------------------------------------------------------------------
# boxplot(ABUND ~ FGRAZE, xlab = "Grazing level", ylab = "Bird abundance", data = loyn)
# 
# # mean bird abundance for each level of FGRAZE
# tapply(loyn$ABUND, loyn$FGRAZE, mean, na.rm = TRUE)
# 
# # it looks from this plot and the table of means that the bird abundance is lowest for FGRAZE level 5 and
# # highest for level 1. The bird abundance for levels 2, 3 and 4 all look similar.
# # so in terms of differences in ABUND between groups we might expect FGRAZE level 5 to be different from
# # the other grazing intensity group and possibly FGRAZE level 1 to be different from graze level 2,3 and 4
# # but this is not particularly clear. We might also expect there to be no differences between grazing
# # levels 2,3 and 4.


## ----Q5, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE------------------------------------------------------------------------------------
# birds_lm <- lm(ABUND ~ FGRAZE, data = loyn)


## ----Q6, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE------------------------------------------------------------------------------------
# anova(birds_lm)
# 
# # null hypothesis : There is no difference in the mean bird abundance between the
# # five levels of grazing.
# # the p value is very small therefore reject this null hypothesis. In other words
# # there is a difference in the mean bird abundance between grazing intensity levels.
# 
# # for a report you might write something like:
# # there was a significant difference in the mean abundance of birds between the five levels
# # of grazing intensity (F_4,62 = 14.98, p < 0.0001)


## ----Q7, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE------------------------------------------------------------------------------------
# summary(birds_lm)
# 
# # Here the intercept (baseline) is the mean abundance of birds for FGRAZE level 1.
# # the null hypothesis for the intercept is that the intercept = 0.
# # As the p value (p < 2e-16) is very small we reject this null hypothesis and conclude that the
# # intercept is significantly different from 0. However, from a biological perspective this
# # is not a particularly informative hypothesis to test.
# 
# # the remaining estimates are differences (contrasts) between each level and the
# # baseline. For example the FGRAZE2 estimate is - 9.2 and therefore there are 9.2 fewer
# # birds on average in graze level 2 compared to graze level 1. This difference is
# # significantly different from zero (p = 0.003).
# 
# # The difference between graze level 3 (FGRAZE3) and graze level 1 (intercept) is
# # -8.45 (8.45 fewer birds in graze 3 compared to graze 1). This difference is significantly
# # different from 0 (p = 0.003) and therefore the mean abundance of birds in graze level 1 is
# # significantly different from graze level 1.
# 
# # The difference between graze level 4 (FGRAZE4) and graze level 1 (intercept) is
# # -9.66 (9.66 fewer birds in graze 4 compared to graze 1). This difference is significantly
# # different from 0 (p = 0.001) and therefore the mean abundance of birds in graze level 1 is
# # significantly different from graze level 4.
# 
# # The difference between graze level 5 (FGRAZE5) and graze level 1 (intercept) is
# # -22.33 (22.33 fewer birds in graze 5 compared to graze 1). This difference is significantly
# # different from 0 (p = 1.64e-10) and therefore the mean abundance of birds in graze level 1 is
# # significantly different from graze level 5.


## ----Q8, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE------------------------------------------------------------------------------------
# # Set FGRAZE level 2 to be the intercept
# 
# loyn$FGRAZE <- relevel(loyn$FGRAZE, ref = "2")
# birds_lm2 <- lm(ABUND ~ FGRAZE, data = loyn)
# summary(birds_lm2)
# 
# # The intercept is now FGRAZE level 2, we can now compare between levels '2 and 3', '2 and 4', and '2 and 5'
# # Also note that the rest of the model output (R^2, F, DF etc) is the same as the previous model (i.e. its
# # the same model we have just changed the intercept and therefore the contrasts).
# 
# loyn$FGRAZE <- relevel(loyn$FGRAZE, ref = "3")
# birds_lm3 <- lm(ABUND ~ FGRAZE, data = loyn)
# summary(birds_lm3)
# 
# # The intercept is now FGRAZE level 3, we can now compare between levels '3 and 4', 'and 3 and 5'
# 
# loyn$FGRAZE <- relevel(loyn$FGRAZE, ref = "4")
# birds_lm4 <- lm(ABUND ~ FGRAZE, data = loyn)
# summary(birds_lm4)
# 
# # The intercept is now FGRAZE level 4, we can now compare between levels '4 and 5'


## ----Q9, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE------------------------------------------------------------------------------------
# # The multiple R-squared value is 0.491 and therefore 49.1% of
# # the variation in ABUND is explained by FGRAZE


## ----Q10, eval=SOLUTIONS, echo=SOLUTIONS, collapse=TRUE-----------------------------------------------------------------------------------
# # first split the plotting device into 2 rows and 2 columns
# par(mfrow = c(2,2))
# 
# # now create the residuals plots
# plot(birds_lm)
# 
# # To test the normality of residuals assumption we use the Normal Q-Q plot. Although the majority of the residuals
# # lie along the 1:1 line there are five residuals which are all below the line resulting in reasonably substantial
# # negative residuals. This suggest that the model does not fit these observation very well.
# 
# # Looking at the homogeneity of variance assumption (Residuals vs Fitted and Scale-Location plot) you can see the
# # five columns of residuals corresponding to the fitted values for the five grazing levels. Again, things don't look
# # great. The spread for the lower fitted values (left side of the plot) is much narrower when compared to the other groups.
# # This suggests that the homogeneity of variance assumption is not met (i.e. the variances are not the same). The same cluster
# # of negative residuals we spotted in the Normal Q-Q plot also appears in the Residuals vs Fitted plot suggesting that it is
# # these residuals that are responsible.
# 
# # The only real good news is that there doesn't appear to be any influential or unusual residuals as indicated in the
# # Residuals vs Leverage plot.
# 
# # So what to do? You could go back and check the original field notebook data to see if a
# # transcribing mistake has been made (seems unlikely and you dont have this luxury anyway).
# # You could also try applying a transformation (log or square root) on the ABUND variable, refit the model and
# # see if this improves things.
# # for example
# 
# loyn$ABUND.SQRT <- sqrt(loyn$ABUND)
# birds_lm_sqrt <- lm(ABUND.SQRT ~ FGRAZE, data = loyn)
# par(mfrow = c(2,2))
# plot(birds_lm_sqrt)
# 
# # Sadly this doesn't seemed to have improved things!
# 
# # Or finally, you can relax the assumption of equal variance and estimate a separate variance for each group using
# # generalised least squares. This is not something we will do on this course but will cover in a more advanced statistics course!


## ----Q11a, eval=SOLUTIONS, echo=SOLUTIONS, collapse=FALSE---------------------------------------------------------------------------------
# # Using the gplots package, you may need to install this package first
# # install.packages('gplots')
# 
# loyn$FGRAZE <- relevel(loyn$FGRAZE, ref = "1")
# library(gplots)
# plotmeans(ABUND ~ FGRAZE, xlab = "grazing level",
#   ylab = "bird abundance", data = loyn, connect = FALSE)


## ----Q11b, eval=SOLUTIONS, echo=SOLUTIONS, collapse=FALSE---------------------------------------------------------------------------------
# # Using the effects package, you may need to install this package first
# # install.packages('effects')
# 
# library(effects)
# loyn_effects <- allEffects(birds_lm)
# plot(loyn_effects,"FGRAZE", lty = 0)


## ----Q11c, eval=SOLUTIONS, echo=SOLUTIONS, collapse=FALSE---------------------------------------------------------------------------------
# # and finally using old faithful the predict function and base R graphics
# # with the segments function
# 
# my_data <- data.frame(FGRAZE = c("1", "2", "3", "4", "5"))
# pred_vals <- predict(birds_lm, newdata = my_data, se.fit = TRUE)
# 
# # now plot these values
# 
# plot(1:5, seq(0, 50, length=5), type = "n", xlab = "Graze intensity", ylab = "Bird Abundance")
# points(1:5, pred_vals$fit)
# segments(1:5, pred_vals$fit, 1:5, pred_vals$fit - 1.96 * pred_vals$se.fit)
# segments(1:5, pred_vals$fit, 1:5, pred_vals$fit + 1.96 * pred_vals$se.fit)


## ----Q11d, eval=SOLUTIONS, echo=SOLUTIONS, collapse=FALSE---------------------------------------------------------------------------------
# # using old faithful the predict function and base R graphics
# # with the arrows function
# 
# my_data <- data.frame(FGRAZE = c("1", "2", "3", "4", "5"))
# pred_vals <- predict(birds_lm, newdata = my_data, se.fit = TRUE)
# 
# # now plot these values
# 
# plot(1:5, seq(0, 50, length=5), type = "n", xlab = "Graze intensity level", ylab = "Bird Abundance")
# arrows(1:5, pred_vals$fit, 1:5, pred_vals$fit - 1.96 * pred_vals$se.fit,
#     angle = 90, code = 2, length = 0.05, col = "blue")
# arrows(1:5, pred_vals$fit, 1:5, pred_vals$fit + 1.96 * pred_vals$se.fit,
#        angle = 90, code = 2, length = 0.05, col = "blue")
# points(1:5, pred_vals$fit, pch = 16)


## ----Q11e, eval=SOLUTIONS, echo=SOLUTIONS, collapse=FALSE---------------------------------------------------------------------------------
# # or using the ggplot2 package
# library(ggplot2) # make the functions in ggplot2 available
# 
# 
# # This plot will plot the means for each level of FGRAZE
# # and also the 95% confidence intervals
# 
# ggplot(loyn, aes(x = FGRAZE, y = ABUND)) +
#   stat_summary(fun = mean, geom = "point", color = "firebrick",
#         size = 3, position=position_nudge(x = 0.15)) +
#   stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
#         width = 0.1, position=position_nudge(x = 0.15))
# 
# 
# # and as an added bonus, if you wanted to also plot the
# # raw data along with the means for FGRAZE
# 
# ggplot(loyn, aes(x = FGRAZE, y = ABUND)) +
#   geom_point(color = "firebrick", size = 3, alpha = 0.6) +
#   stat_summary(fun = mean, geom = "point", color = "firebrick",
#         size = 3, position=position_nudge(x = 0.15)) +
#   stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
#         width = 0.1, position=position_nudge(x = 0.15))
# 
# 

