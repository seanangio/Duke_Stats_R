---
title: "Peer Assessment II"
output:
  html_document: 
    author: Sean Angiolillo
    date: 26 Feb 2018
    keep_md: yes
    pandoc_args: --number-sections
---

# Background



As a statistical consultant working for a real estate investment firm, your task is to develop a model to predict the selling price of a given home in Ames, Iowa. Your employer hopes to use this information to help assess whether the asking price of a house is higher or lower than the true value of the house. If the home is undervalued, it may be a good investment for the firm.

# Training Data and relevant packages

In order to better assess the quality of the model you will produce, the data have been randomly divided into three separate pieces: a training data set, a testing data set, and a validation data set. For now we will load the training data set, the others will be loaded and used later.


```r
load("ames_train.Rdata")
```

Use the code block below to load any necessary packages


```r
library(statsr)
library(dplyr)
library(BAS)
library(caret)
library(ggplot2)
library(GGally)
library(gridExtra)
library(modelr)
library(forcats)
library(knitr)
```

## Part 1 - Exploratory Data Analysis (EDA)

When you first get your data, it's very tempting to immediately begin fitting models and assessing how they perform.  However, before you begin modeling, it's absolutely essential to explore the structure of the data and the relationships between the variables in the data set.

Do a detailed EDA of the ames_train data set, to learn about the structure of the data and the relationships between the variables in the data set (refer to Introduction to Probability and Data, Week 2, for a reminder about EDA if needed). Your EDA should involve creating and reviewing many plots/graphs and considering the patterns and relationships you see. 

After you have explored completely, submit the three graphs/plots that you found most informative during your EDA process, and briefly explain what you learned from each (why you found each informative).

* * *

Before EDA, there are some important cleaning steps needed. One important realization for this dataset is that many values that should actually be "None" are coded as missing (`NA`). To take one example, almost all values in `Pool.QC` are `NA`. As explained in the [codebook]("https://ww2.amstat.org/publications/jse/v19n3/decock/datadocumentation.txt"), this is because almost no properties have a pool; it's not unknown whether a pool is present or not. This pattern is repeated for many amenities like fireplaces, basements, and garages etc. After recoding these missing values, there's very little data actually missing.

Secondly, I filtered to only keep normal sale conditions and types. Our model excludes consideration of unsual sales. This helps ensure reliability amongst observations.

From previous assignments, we saw it was advantageous to log transform many numeric variables, particularly those measured in square feet. Reasons for this are again explained in the end of the report concerning transformations. 

It also makes sense to remove variables with very little variance that would be poor predictors. Loops and functions would clean this code up but it works for now.


```r
# make factor variables where needed
ames_train$MS.SubClass <- factor(ames_train$MS.SubClass)
ames_train$Mo.Sold <- factor(ames_train$Mo.Sold)

# recode missing values that should be none instead of NA
ames_train$Bsmt.Qual <- fct_explicit_na(ames_train$Bsmt.Qual, na_level = "None")
ames_train$Bsmt.Cond <- fct_explicit_na(ames_train$Bsmt.Cond, na_level = "None")
ames_train$Bsmt.Exposure <- fct_explicit_na(ames_train$Bsmt.Exposure, na_level = "None")
ames_train$BsmtFin.Type.1 <- fct_explicit_na(ames_train$BsmtFin.Type.1, na_level = "None")
ames_train$BsmtFin.Type.2 <- fct_explicit_na(ames_train$BsmtFin.Type.2, na_level = "None")
ames_train$Fireplace.Qu <- fct_explicit_na(ames_train$Fireplace.Qu, na_level = "None")
ames_train$Garage.Type <- fct_explicit_na(ames_train$Garage.Type, na_level = "None")
ames_train$Garage.Finish <- fct_explicit_na(ames_train$Garage.Finish, na_level = "None")
ames_train$Pool.QC <- fct_explicit_na(ames_train$Pool.QC, na_level = "None")
ames_train$Fence <- fct_explicit_na(ames_train$Fence, na_level = "None")
ames_train$Misc.Feature <- fct_explicit_na(ames_train$Misc.Feature, na_level = "None")
ames_train$Garage.Qual <- fct_explicit_na(ames_train$Garage.Qual, na_level = "None")
ames_train$Garage.Cond <- fct_explicit_na(ames_train$Garage.Cond, na_level = "None")
ames_train$Alley <- fct_explicit_na(ames_train$Alley, na_level = "None")

ames_train <- ames_train %>%
    # keep only normal sale conditions and conventional warranty deed sale types
    filter(Sale.Condition == "Normal" & Sale.Type == "WD ") %>%
    # then remove those columns which now only have one level or missing values
    dplyr::select(-c(Sale.Condition, Sale.Type, Lot.Frontage, Garage.Yr.Blt)) %>%
    # remove a very small number of observations missing this variable
    filter(!is.na(Mas.Vnr.Area))

# log transformations of most numeric variables
ames_train <- ames_train %>%
    mutate(larea = log(area),
           lprice = log(price),
           lLot.Area = log(Lot.Area),
           lMas.Vnr.Area = log(Mas.Vnr.Area + 1),
           lBsmtFin.SF.1 = log(BsmtFin.SF.1 + 1),
           lBsmtFin.SF.2 = log(BsmtFin.SF.2 + 1),
           lBsmt.Unf.SF = log(Bsmt.Unf.SF + 1),
           lTotal.Bsmt.SF = log(Total.Bsmt.SF + 1),
           lX1st.Flr.SF = log(X1st.Flr.SF),
           lX2nd.Flr.SF = log(X2nd.Flr.SF + 1),
           lLow.Qual.Fin.SF = log(Low.Qual.Fin.SF + 1),
           lGarage.Area = log(Garage.Area + 1),
           lWood.Deck.SF = log(Wood.Deck.SF + 1),
           lOpen.Porch.SF = log(Open.Porch.SF + 1),
           lEnclosed.Porch = log(Enclosed.Porch + 1),
           lX3Ssn.Porch = log(X3Ssn.Porch + 1),
           lScreen.Porch = log(Screen.Porch + 1),
           lPool.Area = log(Pool.Area + 1),
           lMisc.Val = log(Misc.Val + 1)) %>%
    dplyr::select(-c(area, price, Lot.Area, Mas.Vnr.Area, BsmtFin.SF.1, BsmtFin.SF.2,
                     Bsmt.Unf.SF, Total.Bsmt.SF, X1st.Flr.SF, X2nd.Flr.SF,
                     Low.Qual.Fin.SF, Garage.Area, Wood.Deck.SF, Open.Porch.SF,
                     Enclosed.Porch, X3Ssn.Porch, Screen.Porch, Pool.Area, Misc.Val))

# drop unused levels for neighborhood
ames_train$Neighborhood <- droplevels(ames_train$Neighborhood)

# remove near zero variance variables (poor predictors)
ames_train <- ames_train %>%
    dplyr::select(-one_of(nearZeroVar(ames_train, names = TRUE)))
```

It's helpful to spell out some of our expectations to assess what would be surprising or unsurprising during EDA. I expect the predictors to have fairly intuitive relationships with prices as we are dealing with well-known entities. For example, we would expect the following to be associated with higher property prices: larger area, more rooms, newer construction, better conditions, certain neighborhoods, and the presence of certain amenities (like central air or fireplaces for instance). We'd also expect many of these predictors to be correlated with each other. For instance, properties with larger areas will likely also have more bedrooms. So a challenge will be determining which predictors are actually important and which happen to coincide with those predictors.

### Plot 1

The scatterplot matrix below is a bit crowded but helps show a number of important predictor relationships with `lprice`. As expected from intuition, predictors like `Overall.Qual`, `larea` and `lX1st.Flr.SF` have strong positive correlations with `lprice`. An advantage of the matrix is that it also depicts relationships amongst predictors. Collinearity between predictors will likely be an issue because many variables, such as `larea`, `lX1st.Flr.SF` and `lLot.Area`, communicate the same fundamental information.


```r
select <- c("lprice","Overall.Qual","larea","lX1st.Flr.SF","lLot.Area", "Overall.Cond", "TotRms.AbvGrd")
ggpairs(ames_train[,names(ames_train) %in% select], aes(alpha = 0.1))
```

![](Final_peer_files/figure-html/creategraphs1-1.png)<!-- -->

### Plot 2

In the previous assignment, we saw location was important as evidenced by a plot of median prices grouped by neighborhood. A hidden layer to location is zoning, and the plot below shows property prices mapped by area and colored by zone. Its particularly clear how `RL` (residential low density) zones tend to have higher prices than `RM` (residential medium density) zones for any given area. The log transformations of price and area are less readily interpetable, but at this point we are only interested in seeing the relationship.


```r
ggplot(ames_train, aes(x = larea, y = lprice, color = MS.Zoning)) + 
    geom_point(alpha = 0.5) +
    labs(title = "Zoning Classification by Price and Area", x = "log(area)", y = "log(price)")
```

![](Final_peer_files/figure-html/creategraphs2-1.png)<!-- -->

### Plot 3

In addition to key factors like area, number of rooms, overall condition, and neighborhood, it appears certain amenities can also give a boost to a property's value. Having a kitchen or heating system in excellent quality tends to be associated with higher prices. Not having central air is a negative factor. Having more full baths tends to be associated with higher prices. Although not shown, other home elements like roof style or masonry veneer style seemed to have much weaker relationships with prices. I don't think a realtor would be terribly surprised with any of these revelations, but it is good to confirm our intuition.


```r
p1 <- ggplot(ames_train, aes(x = larea, y = lprice, color = Kitchen.Qual)) + 
    geom_point(alpha = 0.5)
p2 <- ggplot(ames_train, aes(x = larea, y = lprice, color = Heating.QC)) + 
    geom_point(alpha = 0.5)
p3 <- ggplot(ames_train, aes(x = larea, y = lprice, color = Central.Air)) + 
    geom_point(alpha = 0.5)
p4 <- ggplot(ames_train, aes(x = larea, y = lprice, color = factor(Full.Bath))) +
    geom_point(alpha = 0.5)
grid.arrange(p1, p2, p3, p4, ncol = 2)
```

![](Final_peer_files/figure-html/creategraphs3-1.png)<!-- -->

* * *

## Part 2 - Development and assessment of an initial model, following a semi-guided process of analysis

### Section 2.1 An Initial Model
In building a model, it is often useful to start by creating a simple, intuitive initial model based on the results of the exploratory data analysis. (Note: The goal at this stage is **not** to identify the "best" possible model but rather to choose a reasonable and understandable starting point. Later you will expand and revise this model to create your final model.

Based on your EDA, select *at most* 10 predictor variables from “ames_train” and create a linear model for `price` (or a transformed version of price) using those variables. Provide the *R code* and the *summary output table* for your model, a *brief justification* for the variables you have chosen, and a *brief discussion* of the model results in context (focused on the variables that appear to be important predictors and how they relate to sales price).

* * *

Based purely on intuition and our exploratory plots above, we could construct the following initial model. In cases where predictors would seem to communicate very similar information (like `larea`, `lLot.Area` and `X1st.Fl.SF`) I chose only one. My goal in picking the rest was to choose variables that each communicate something different that could influence the price of a house.


```r
# minimal model
m0 <- lm(lprice ~ Overall.Qual + larea, data = ames_train)

# initial model
m1 <- lm(lprice ~ Overall.Qual + larea + MS.Zoning + Neighborhood + Year.Built + 
             Central.Air + Kitchen.Qual + Heating.QC, data = ames_train)
summary(m1)
```

```
## 
## Call:
## lm(formula = lprice ~ Overall.Qual + larea + MS.Zoning + Neighborhood + 
##     Year.Built + Central.Air + Kitchen.Qual + Heating.QC, data = ames_train)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.62879 -0.07101  0.00000  0.07651  0.59143 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          3.6499556  0.8048033   4.535 6.70e-06 ***
## Overall.Qual         0.0763858  0.0064783  11.791  < 2e-16 ***
## larea                0.4564609  0.0192904  23.663  < 2e-16 ***
## MS.ZoningFV          0.1009596  0.0884225   1.142 0.253907    
## MS.ZoningI (all)    -0.0568383  0.1456403  -0.390 0.696451    
## MS.ZoningRH          0.1243894  0.0915555   1.359 0.174674    
## MS.ZoningRL          0.2466941  0.0740103   3.333 0.000901 ***
## MS.ZoningRM          0.1468026  0.0699983   2.097 0.036307 *  
## NeighborhoodBlueste -0.0366832  0.0927663  -0.395 0.692633    
## NeighborhoodBrDale  -0.1910704  0.0745281  -2.564 0.010549 *  
## NeighborhoodBrkSide  0.0662172  0.0624093   1.061 0.289024    
## NeighborhoodClearCr  0.2130708  0.0655575   3.250 0.001205 ** 
## NeighborhoodCollgCr  0.0365475  0.0512657   0.713 0.476127    
## NeighborhoodCrawfor  0.1715423  0.0597135   2.873 0.004184 ** 
## NeighborhoodEdwards -0.0305480  0.0557319  -0.548 0.583769    
## NeighborhoodGilbert  0.0017718  0.0542514   0.033 0.973955    
## NeighborhoodGreens   0.1282069  0.0824293   1.555 0.120283    
## NeighborhoodGrnHill  0.4871344  0.1056986   4.609 4.76e-06 ***
## NeighborhoodIDOTRR  -0.0037950  0.0680472  -0.056 0.955540    
## NeighborhoodMeadowV -0.1610161  0.0662323  -2.431 0.015287 *  
## NeighborhoodMitchel  0.1045046  0.0542908   1.925 0.054619 .  
## NeighborhoodNAmes    0.0630325  0.0535291   1.178 0.239355    
## NeighborhoodNoRidge  0.1488400  0.0550569   2.703 0.007019 ** 
## NeighborhoodNPkVill -0.0954081  0.0820895  -1.162 0.245506    
## NeighborhoodNridgHt  0.1358347  0.0549862   2.470 0.013720 *  
## NeighborhoodNWAmes   0.0546193  0.0553060   0.988 0.323674    
## NeighborhoodOldTown  0.0337861  0.0638608   0.529 0.596921    
## NeighborhoodSawyer   0.0546383  0.0548551   0.996 0.319548    
## NeighborhoodSawyerW -0.0319134  0.0538089  -0.593 0.553301    
## NeighborhoodSomerst  0.1488143  0.0650717   2.287 0.022478 *  
## NeighborhoodStoneBr  0.0899351  0.0624992   1.439 0.150574    
## NeighborhoodSWISU   -0.0558541  0.0692210  -0.807 0.419983    
## NeighborhoodTimber   0.1509365  0.0581794   2.594 0.009663 ** 
## NeighborhoodVeenker  0.1711180  0.0673689   2.540 0.011285 *  
## Year.Built           0.0022019  0.0003874   5.684 1.88e-08 ***
## Central.AirY         0.1755122  0.0253853   6.914 1.01e-11 ***
## Kitchen.QualFa      -0.2382761  0.0434406  -5.485 5.65e-08 ***
## Kitchen.QualGd      -0.1549360  0.0255380  -6.067 2.07e-09 ***
## Kitchen.QualTA      -0.2183243  0.0282859  -7.718 3.76e-14 ***
## Heating.QCFa        -0.0803498  0.0342469  -2.346 0.019226 *  
## Heating.QCGd        -0.0078989  0.0143791  -0.549 0.582942    
## Heating.QCPo         0.0902616  0.1345649   0.671 0.502576    
## Heating.QCTA        -0.0143838  0.0136849  -1.051 0.293566    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1286 on 750 degrees of freedom
## Multiple R-squared:  0.8923,	Adjusted R-squared:  0.8863 
## F-statistic: 147.9 on 42 and 750 DF,  p-value: < 2.2e-16
```

This initial model appears to have done fairly well. With eight predictors, we have obtained an adjusted R-squared of 0.886. We have significant p-values for all predictors. As we'd expect `larea` and `Overall.Qual` have positive coefficients. Some neighborhoods have positive coefficients, while others are negative. Most zones have positive coefficients, but an industrial zone is negative. Having central air is a positive. Not having an excellent kitchen is a negative. It's also interesting to note that a bare bones model of only two predictors (`Overall.Qual` and `larea`) accomplishes a great deal. It yields an adjusted R-squared of 0.771.


* * *

### Section 2.2 Model Selection

Now either using `BAS` another stepwise selection procedure choose the "best" model you can, using your initial model as your starting point. Try at least two different model selection methods and compare their results. Do they both arrive at the same model or do they disagree? What do you think this means?

* * *

If we choose our model by AIC using a stepwise algorithm, we get a model with 34 predictor variables if set to backward or 30 if set to forward. Setting the direction to "both" also yields the same results as the forward model.


```r
# backward AIC
m_aic_bw <- step(lm(lprice ~ ., data = ames_train), direction = 'backward', trace = 0)

# forward AIC
null <- lm(lprice ~ 1, data = ames_train)
full <- lm(lprice ~ ., data = ames_train)
m_aic_fw <- step(null, scope = list(lower = null, upper = full), direction = "forward", trace = 0)

# both direction AIC
m_aic_both <- step(lm(lprice ~ ., data = ames_train), direction = 'both', trace = 0)
identical(m_aic_bw, m_aic_both)
```

```
## [1] TRUE
```

```r
# final model formula for backward, forward and both directions
rm(null, m_aic_both)
m_aic_bw$call
```

```
## lm(formula = lprice ~ MS.Zoning + Lot.Shape + Lot.Config + Neighborhood + 
##     Condition.1 + Bldg.Type + House.Style + Overall.Qual + Overall.Cond + 
##     Year.Built + Year.Remod.Add + Roof.Style + Exterior.1st + 
##     Exter.Qual + Exter.Cond + Foundation + Bsmt.Qual + Bsmt.Exposure + 
##     BsmtFin.Type.1 + Central.Air + Bsmt.Full.Bath + Kitchen.Qual + 
##     Fireplaces + Garage.Finish + Garage.Cars + Paved.Drive + 
##     Mo.Sold + larea + lLot.Area + lBsmtFin.SF.1 + lBsmt.Unf.SF + 
##     lTotal.Bsmt.SF + lWood.Deck.SF + lOpen.Porch.SF, data = ames_train)
```

```r
m_aic_fw$call
```

```
## lm(formula = lprice ~ Overall.Qual + larea + Neighborhood + BsmtFin.Type.1 + 
##     MS.SubClass + Overall.Cond + Year.Built + lLot.Area + Bsmt.Full.Bath + 
##     Kitchen.Qual + MS.Zoning + Garage.Cars + lTotal.Bsmt.SF + 
##     Exter.Cond + Fireplaces + Condition.1 + Exterior.1st + Bsmt.Exposure + 
##     Bsmt.Qual + Central.Air + Foundation + Year.Remod.Add + lBsmtFin.SF.1 + 
##     lBsmtFin.SF.2 + Exter.Qual + Mo.Sold + lOpen.Porch.SF + lWood.Deck.SF + 
##     lGarage.Area + Lot.Config, data = ames_train)
```

If we change our model selection criteria from AIC to BIC in a stepwise algorithm, we get a model with 15 predictors (more than our initial model but far fewer than the AIC models). This is because we have set `k` equal to the log of the number of observations instead of the default 2 as is done when using AIC. The larger penalty is intended to discourage overfitting, and so will reduce the number of selected predictors.


```r
# change selection criteria to BIC in same stepwise algorithm
m_bic <- step(full, k = log(nrow(ames_train)), trace = 0)
rm(full)
m_bic$call
```

```
## lm(formula = lprice ~ MS.Zoning + Overall.Qual + Overall.Cond + 
##     Year.Built + Year.Remod.Add + Bsmt.Exposure + Central.Air + 
##     Bsmt.Full.Bath + Kitchen.Qual + Fireplaces + Garage.Cars + 
##     larea + lLot.Area + lBsmtFin.SF.1 + lTotal.Bsmt.SF, data = ames_train)
```

We can also try a Bayesian approach using the `BAS` package.


```r
set.seed(417)
m_bas <- bas.lm(lprice ~ ., data = ames_train, prior = "BIC", 
                 modelprior = uniform(), method = "MCMC")

pred.tr.HPM <- predict(m_bas, estimator = "HPM")$fit
```

Below I'll need to calculate RMSE so I'll add predictions to the dataframe for each model.


```r
ames_train <- ames_train %>%
    add_predictions(m0, var = "pred_m0") %>%
    add_predictions(m1, var = "pred_m1") %>%
    add_predictions(m_bic, var = "pred_bic") %>%
    add_predictions(m_aic_bw, var = "pred_aic_fw") %>%
    add_predictions(m_aic_fw, var = "pred_aic_bw") %>%
    mutate(pred.tr.HPM)
```

```
## Warning in predict.lm(model, data): prediction from a rank-deficient fit
## may be misleading

## Warning in predict.lm(model, data): prediction from a rank-deficient fit
## may be misleading
```

```r
# create rmse function for use in table below
rmse <- function(m, o){
  sqrt(mean((m - o)^2))
}
```

We get rank-deficient fit warnings for the predictions of both AIC models. It appears to be related to the `Bsmt.Qual` variable, but its distribution seems reasonable so I'll return to this problem only if we end up pursuing one of those models.

We can collect key summary statistics to quickly compare these models.


```r
# collect summary stats of each model
model_comp <- data.frame(
    model = c("basic", "initial", "BIC", "Forward AIC", "Backward AIC", "HPM"),
    n_predictors = c(2, 8, 15, 30, 34, NA),
    train_adj.r.sq = c(
        summary(m0)$adj.r.squared,
        summary(m1)$adj.r.squared,
        summary(m_bic)$adj.r.squared,
        summary(m_aic_fw)$adj.r.squared,
        summary(m_aic_bw)$adj.r.squared,
        NA),
    train_RSE = c(
        summary(m0)$sigma,
        summary(m1)$sigma,
        summary(m_bic)$sigma,
        summary(m_aic_fw)$sigma,
        summary(m_aic_bw)$sigma,
        NA),
    train_RMSE_usd = c(
        rmse(exp(ames_train$pred_m0), exp(ames_train$lprice)),
        rmse(exp(ames_train$pred_m1), exp(ames_train$lprice)),
        rmse(exp(ames_train$pred_bic), exp(ames_train$lprice)),
        rmse(exp(ames_train$pred_aic_fw), exp(ames_train$lprice)),
        rmse(exp(ames_train$pred_aic_bw), exp(ames_train$lprice)),
        rmse(exp(ames_train$pred.tr.HPM), exp(ames_train$lprice))
        ))

kable(model_comp)
```



model           n_predictors   train_adj.r.sq   train_RSE   train_RMSE_usd
-------------  -------------  ---------------  ----------  ---------------
basic                      2        0.7708395   0.1825546         33251.82
initial                    8        0.8862698   0.1286059         25639.84
BIC                       15        0.9306295   0.1004408         19245.01
Forward AIC               30        0.9551957   0.0807203         13544.80
Backward AIC              34        0.9556915   0.0802724         13881.31
HPM                       NA               NA          NA        392765.78


Our initial hand-built model had 8 predictor variables. The BIC step model chose 15. The AIC step models retained 30 and 34 predictors. As the number of predictor variables increase, we see two trends that suggest a better model: increasing adjusted R-squared and decreasing residual standard error. 

While this might suggest to us that we should pick the model with the most predictors, two reasons would caution us against this. The first is interpretability, a requirement from our clients. A model with 35 predictors is much more complicated to interpret than one with only a few variables. 

The most important reason though is overfitting. A model too finely attuned to minimize the error from training data will not generalize well to new unseen data. In minimizing the bias, we have also fit the variance or noise. This is referred to as the bias-variance tradeoff. Our basic model with only `Overall.Cond` and `larea` is highly interpretable but is likely underfit. There's still a large amount of bias remaining in the data for which the model has not accounted. On the other hand, both AIC models may be overfit. We'll be able to test this out when we examine the models on training data.

We can also see the decreasing gains in adjusted R-squared or reductions in residual standard error as we add more predictors to the model. The adjusted R-squared of the backward AIC model is 0.956. An extra 19 variables raised adjusted R-squared by only 0.025 points!

The `BAS` model had by far the highest RMSE of any model so we'll exclude it going forward. Something isn't right there.

```r
rm(m_bas, pred.tr.HPM)
model_comp <- head(model_comp, -1)
```


* * *

### Section 2.3 Initial Model Residuals
One way to assess the performance of a model is to examine the model's residuals. In the space below, create a residual plot for your preferred model from above and use it to assess whether your model appears to fit the data well. Comment on any interesting structure in the residual plot (trend, outliers, etc.) and briefly discuss potential implications it may have for your model and inference / prediction you might produce.

* * *

We can plot and interpret all of the standard residuals plots. I'll choose the BIC step model. But the residuals for the other models actually did look fairly similar.


```r
# four standard residual plots
par(mfrow = c(2,2))
plot(m_bic)
```

```
## Warning: not plotting observations with leverage one:
##   450, 642

## Warning: not plotting observations with leverage one:
##   450, 642
```

![](Final_peer_files/figure-html/model_resid-1.png)<!-- -->

When executing the residual plots, we do get a message that two points with leverage one have not been plotted. It appears that these points have exactly correct predictions (for at least one model), which I believe is creating the problem. Nothing else seems unusual about them though so I will continue through each of the four residual plots.


PID          lprice     pred_m1    pred_bic 
-----------  ---------  ---------  ---------
1007100110   11.54248   11.54248   11.54248 
910201020    11.37366   11.37366   11.41200 

The residuals vs. fitted values plot is used to explore patterns in the residuals. Particularly we are interested in seeing if there are any non-linear patterns that would suggest our model is poorly fitting the data. Fortunately, our plot shows random scatter around 0, suggesting that our model meets the condition of linearity. We have three points (582, 535, 86) that might be outliers that deserve more attention.

A normal Q-Q plot tests if the residuals follow a normal distribution. Most points line up quite well. The same three points are highlighted however.

The scale-location plot tests the assumptin of equal variance. Like in the residuals vs fitted plot, we hope to see random scatter around y = 0. We largely satisfy this condition, suggesting that we largely do have constant variance across the range of predicted values. The same three points are noteworthy.

Lastly, the residuals vs. leverage plots helps us to find potentially influential cases. 582 is highlighted as being influential against a regression line.

I'd say our residual plots suggest overall that our model is fairly sound. If we examine the three potential outliers identified in our residual plots, nothing particularly jumps out. Perhaps these were just bad deals where the price did not meet the expected value. Without a real case to exclude them however, I am going to retain them.


       PID  MS.SubClass   MS.Zoning   Lot.Shape   Lot.Config   Neighborhood   Condition.1   Bldg.Type   House.Style    Overall.Qual   Overall.Cond   Year.Built   Year.Remod.Add  Roof.Style   Exterior.1st   Exterior.2nd   Mas.Vnr.Type   Exter.Qual   Exter.Cond   Foundation   Bsmt.Qual   Bsmt.Exposure   BsmtFin.Type.1   Heating.QC   Central.Air   Electrical    Bsmt.Full.Bath   Bsmt.Half.Bath   Full.Bath   Half.Bath   Bedroom.AbvGr  Kitchen.Qual    TotRms.AbvGrd   Fireplaces  Fireplace.Qu   Garage.Type   Garage.Finish    Garage.Cars  Paved.Drive   Fence   Mo.Sold    Yr.Sold      larea     lprice   lLot.Area   lMas.Vnr.Area   lBsmtFin.SF.1   lBsmtFin.SF.2   lBsmt.Unf.SF   lTotal.Bsmt.SF   lX1st.Flr.SF   lX2nd.Flr.SF   lGarage.Area   lWood.Deck.SF   lOpen.Porch.SF   lEnclosed.Porch    pred_m0    pred_m1   pred_bic   pred_aic_fw   pred_aic_bw   pred.tr.HPM
----------  ------------  ----------  ----------  -----------  -------------  ------------  ----------  ------------  -------------  -------------  -----------  ---------------  -----------  -------------  -------------  -------------  -----------  -----------  -----------  ----------  --------------  ---------------  -----------  ------------  -----------  ---------------  ---------------  ----------  ----------  --------------  -------------  --------------  -----------  -------------  ------------  --------------  ------------  ------------  ------  --------  --------  ---------  ---------  ----------  --------------  --------------  --------------  -------------  ---------------  -------------  -------------  -------------  --------------  ---------------  ----------------  ---------  ---------  ---------  ------------  ------------  ------------
 916252170  120           RM          IR1         Inside       GrnHill        Norm          TwnhsE      1Story                    7              5         1986             1986  Gable        BrkFace        Wd Sdng        None           Gd           TA           CBlock       None        None            None             Gd           Y             SBrkr                      0                0           2           0               2  Gd                          5            0  None           Attchd        RFn                        1  Y             None    11            2006   7.166266   12.34583    9.016634        0.000000        0.000000        0.000000       0.000000         0.000000       7.166266       0.000000       5.746203               0         0.000000          0.000000   12.13150   12.47534   11.81651      12.31114      12.31124      11.85922
 916253320  120           RM          IR1         Inside       GrnHill        Norm          TwnhsE      1Story                    7              5         1998             1998  Gable        Wd Sdng        Wd Sdng        BrkFace        Gd           TA           PConc        Gd          No              GLQ              Ex           Y             SBrkr                      1                0           1           1               1  Gd                          5            1  TA             Attchd        Fin                        1  Y             None    9             2007   7.314553   12.70685    9.186355        5.480639        7.005789        4.290459       5.796058         7.315218       7.314553       0.000000       5.659482               0         0.000000          0.000000   12.20128   12.57735   12.22199      12.74154      12.74144      11.81312
 911102170  70            C (all)     Reg         Inside       IDOTRR         Feedr         1Fam        2Story                    4              4         1920             1950  Gambrel      BrkFace        BrkFace        None           TA           Fa           BrkTil       TA          No              Unf              TA           N             SBrkr                      0                0           1           0               3  TA                          6            0  None           Detchd        Unf                        1  N             MnPrv   7             2008   7.183112   10.59663    9.047821        0.000000        0.000000        0.000000       6.476972         6.476972       6.475433       6.505784       5.525453               0         4.007333          5.153292   11.65427   11.22543   11.09793      10.93587      10.96619      11.94822

* * *

### Section 2.4 Initial Model RMSE

You can calculate it directly based on the model output. Be specific about the units of your RMSE (depending on whether you transformed your response variable). The value you report will be more meaningful if it is in the original units (dollars).

* * *

We can recall the RMSE calculations from the table above. Converting log prices and predictions back into dollar formats makes our root mean squared error much more readily interpretable. Now it's easier to much more tempting to see the value of the AIC models. We can calculate the root mean square error of our initial model to be \$25,639.84 compared to \$13,544.8 in the forward AIC model. A typical difference of \$12,095 would certainly be of interest to our client. The forward AIC model is clearly better, but not if it is overfit!


model          train_RMSE_usd 
-------------  ---------------
basic          33251.82       
initial        25639.84       
BIC            19245.01       
Forward AIC    13544.80       
Backward AIC   13881.31       


* * *

### Section 2.5 Overfitting 

The process of building a model generally involves starting with an initial model (as you have done above), identifying its shortcomings, and adapting the model accordingly. This process may be repeated several times until the model fits the data reasonably well. However, the model may do well on training data but perform poorly out-of-sample (meaning, on a dataset other than the original training data) because the model is overly-tuned to specifically fit the training data. This is called “overfitting.” To determine whether overfitting is occurring on a model, compare the performance of a model on both in-sample and out-of-sample data sets. To look at performance of your initial model on out-of-sample data, you will use the data set `ames_test`.


```r
load("ames_test.Rdata")
```

Use your model from above to generate predictions for the housing prices in the test data set.  Are the predictions significantly more accurate (compared to the actual sales prices) for the training data than the test data?  Why or why not? Briefly explain how you determined that (what steps or processes did you use)?

* * *

First, I need to perform the same data manipulations and cleaning as I did to the training dataset. Because `nearZeroVar` won't necessarily return the same columns, I have to explicitly make sure the selected columns for the testing set match the training set. There also a few cases in the testing set with a factor variable not previously seen in the training data. I'll remove these few points.


```r
# make factor variables where needed
ames_test$MS.SubClass <- factor(ames_test$MS.SubClass)
ames_test$Mo.Sold <- factor(ames_test$Mo.Sold)

# recode missing values that should be none
ames_test$Bsmt.Qual <- fct_explicit_na(ames_test$Bsmt.Qual, na_level = "None")
ames_test$Bsmt.Cond <- fct_explicit_na(ames_test$Bsmt.Cond, na_level = "None")
ames_test$Bsmt.Exposure <- fct_explicit_na(ames_test$Bsmt.Exposure, na_level = "None")
ames_test$BsmtFin.Type.1 <- fct_explicit_na(ames_test$BsmtFin.Type.1, na_level = "None")
ames_test$BsmtFin.Type.2 <- fct_explicit_na(ames_test$BsmtFin.Type.2, na_level = "None")
ames_test$Fireplace.Qu <- fct_explicit_na(ames_test$Fireplace.Qu, na_level = "None")
ames_test$Garage.Type <- fct_explicit_na(ames_test$Garage.Type, na_level = "None")
ames_test$Garage.Finish <- fct_explicit_na(ames_test$Garage.Finish, na_level = "None")
ames_test$Pool.QC <- fct_explicit_na(ames_test$Pool.QC, na_level = "None")
ames_test$Fence <- fct_explicit_na(ames_test$Fence, na_level = "None")
ames_test$Misc.Feature <- fct_explicit_na(ames_test$Misc.Feature, na_level = "None")
ames_test$Garage.Qual <- fct_explicit_na(ames_test$Garage.Qual, na_level = "None")
ames_test$Garage.Cond <- fct_explicit_na(ames_test$Garage.Cond, na_level = "None")
ames_test$Alley <- fct_explicit_na(ames_test$Alley, na_level = "None")

ames_test <- ames_test %>%
    # keep only normal sale conditions and conventional warranty deed sale types
    filter(Sale.Condition == "Normal" & Sale.Type == "WD ") %>%
    # then remove those columns which now only have one level or missing values 
    dplyr::select(-c(Sale.Condition, Sale.Type, Lot.Frontage, Garage.Yr.Blt)) %>%
    # remove a very small number of observations missing this variable
    filter(!is.na(Mas.Vnr.Area))

# log transformations of most numeric variables
ames_test <- ames_test %>%
    mutate(larea = log(area),
           lprice = log(price),
           lLot.Area = log(Lot.Area),
           lMas.Vnr.Area = log(Mas.Vnr.Area + 1),
           lBsmtFin.SF.1 = log(BsmtFin.SF.1 + 1),
           lBsmtFin.SF.2 = log(BsmtFin.SF.2 + 1),
           lBsmt.Unf.SF = log(Bsmt.Unf.SF + 1),
           lTotal.Bsmt.SF = log(Total.Bsmt.SF + 1),
           lX1st.Flr.SF = log(X1st.Flr.SF),
           lX2nd.Flr.SF = log(X2nd.Flr.SF + 1),
           lLow.Qual.Fin.SF = log(Low.Qual.Fin.SF + 1),
           lGarage.Area = log(Garage.Area + 1),
           lWood.Deck.SF = log(Wood.Deck.SF + 1),
           lOpen.Porch.SF = log(Open.Porch.SF + 1),
           lEnclosed.Porch = log(Enclosed.Porch + 1),
           lX3Ssn.Porch = log(X3Ssn.Porch + 1),
           lScreen.Porch = log(Screen.Porch + 1),
           lPool.Area = log(Pool.Area + 1),
           lMisc.Val = log(Misc.Val + 1)) %>%
    dplyr::select(-c(area, price, Lot.Area, Mas.Vnr.Area, BsmtFin.SF.1, BsmtFin.SF.2,
                     Bsmt.Unf.SF, Total.Bsmt.SF, X1st.Flr.SF, X2nd.Flr.SF,
                     Low.Qual.Fin.SF, Garage.Area, Wood.Deck.SF, Open.Porch.SF,
                     Enclosed.Porch, X3Ssn.Porch, Screen.Porch, Pool.Area, Misc.Val))

# filter first
ames_test <- ames_test %>%
    # remove a few entries that is present in test but not training
    filter(!Neighborhood == "Landmrk") %>%
    filter(!Foundation == "Wood") %>%
    filter(!Exterior.1st == "AsphShn") %>%
    filter(!Exterior.2nd == "AsphShn") %>%
    filter(!Exterior.2nd == "Stone") %>%
    filter(!House.Style == "2.5Fin") %>%
    filter(!Exter.Cond == "Po") %>%
    filter(!Roof.Style == "Shed")

# then keep only columns that remain in ames_test (after removing predictions)
ames_test <- ames_test[head(names(ames_train), -6)]
```

Now I can generate model predictions for the test data and calculate respective RMSE. Then we can compare RMSE of each model on training and testing data.


```r
# add model predictions
ames_test <- ames_test %>%
    add_predictions(m0, var = "pred_m0") %>%
    add_predictions(m1, var = "pred_m1") %>%
    add_predictions(m_bic, var = "pred_bic") %>%
    add_predictions(m_aic_fw, var = "pred_aic_fw") %>%
    add_predictions(m_aic_bw, var = "pred_aic_bw")
```

```
## Warning in predict.lm(model, data): prediction from a rank-deficient fit
## may be misleading

## Warning in predict.lm(model, data): prediction from a rank-deficient fit
## may be misleading
```

```r
# create test_RMSE_usd vector
test_RMSE_usd <- c(rmse(exp(ames_test$pred_m0), exp(ames_test$lprice)),
                   rmse(exp(ames_test$pred_m1), exp(ames_test$lprice)),
                   rmse(exp(ames_test$pred_bic), exp(ames_test$lprice)),
                   rmse(exp(ames_test$pred_aic_fw), exp(ames_test$lprice)),
                   rmse(exp(ames_test$pred_aic_bw), exp(ames_test$lprice)))

model_comp <- model_comp %>%
    mutate(test_RMSE_usd = test_RMSE_usd,
           per_inc = (test_RMSE_usd - train_RMSE_usd) / train_RMSE_usd)
kable(model_comp)
```



model           n_predictors   train_adj.r.sq   train_RSE   train_RMSE_usd   test_RMSE_usd      per_inc
-------------  -------------  ---------------  ----------  ---------------  --------------  -----------
basic                      2        0.7708395   0.1825546         33251.82        32666.16   -0.0176131
initial                    8        0.8862698   0.1286059         25639.84        27205.56    0.0610659
BIC                       15        0.9306295   0.1004408         19245.01        21401.03    0.1120302
Forward AIC               30        0.9551957   0.0807203         13544.80        19863.09    0.4664737
Backward AIC              34        0.9556915   0.0802724         13881.31        20301.50    0.4625058

With the exception of our very basic model, we see that the RMSE of each of the three models has increased in value when comparing RMSE on the testing set to that on the training set. This isn't entirely surprising. Because it is unseen data, it's more likely that the RMSE will increase using a model trained on a different dataset.

However, what is interesting is the size of this increase varies greatly amongst models. The first two small models saw very modest (and even a negative) increase in RMSE when applied to the unseen testing data. The AIC step models saw very large increases in RMSE on the test set. This suggests our AIC models are likely overfit and our first two models are underfit. If we selected an AIC model, it would not be able to give us a reliable error measurement on unseen data (which would be its purpose). The first two models though would have too large of a typical error to be useful. We can probably do better.

The BIC model stands somewhere between these two possibilities. Its RMSE on the testing set is still higher than the forward AIC model. I'll try to improve it with some tweaks in the next section.

* * *

**Note to the learner:** If in real-life practice this out-of-sample analysis shows evidence that the training data fits your model a lot better than the test data, it is probably a good idea to go back and revise the model (usually by simplifying the model) to reduce this overfitting. For simplicity, we do not ask you to do this on the assignment, however.

## Part 3 Development of a Final Model

Now that you have developed an initial model to use as a baseline, create a final model with *at most* 20 variables to predict housing prices in Ames, IA, selecting from the full array of variables in the dataset and using any of the tools that we introduced in this specialization.  

Carefully document the process that you used to come up with your final model, so that you can answer the questions below.

### Section 3.1 Final Model

Provide the summary table for your model.

* * *

To choose a final model, I compared the formulas of the BIC and forward AIC models to look for predictors I could add to the BIC model. I iteratively tested including predictors in the forward AIC model not present in the BIC model for its impact on RMSE. I found four predictors that had this desired effect: `Neighborhood`, `Lot.Config`, `Exter.Qual` and `Condition.1`. Now, as seen in the last row of the table below, the modified BIC model with 19 predictors has the lowest RMSE on the training set-- lower than the much larger AIC step models.


```r
# remove previous predictions from dataframes
ames_tr <- ames_train[head(names(ames_train), -6)]
ames_te <- ames_test[head(names(ames_test), -5)]

# create model
m_bic2 <- lm(lprice ~ MS.Zoning + Overall.Qual + Overall.Cond + Year.Built +
               Year.Remod.Add + Bsmt.Exposure + Central.Air + Bsmt.Full.Bath +
               Kitchen.Qual + Fireplaces + Garage.Cars + larea + lLot.Area +
               lBsmtFin.SF.1 + lTotal.Bsmt.SF + Neighborhood + Lot.Config + Exter.Qual +
               Condition.1, data = ames_tr)

# add predictions to dataframe
ames_tr <- ames_tr %>% add_predictions(m_bic2, var = "pred_bic2")
ames_te <- ames_te %>% add_predictions(m_bic2, var = "pred_bic2")

# create summary metrics
bic2_df <- data.frame(
    model = "BIC2",
    n_predictors = 19,
    train_adj.r.sq = summary(m_bic2)$adj.r.squared,
    train_RSE = summary(m_bic2)$sigma,
    train_RMSE_usd = rmse(exp(ames_tr$pred_bic2), exp(ames_tr$lprice)),
    test_RMSE_usd = rmse(exp(ames_te$pred_bic2), exp(ames_te$lprice)),
    per_inc = (rmse(exp(ames_te$pred_bic2), exp(ames_te$lprice)) - rmse(exp(ames_tr$pred_bic2), exp(ames_tr$lprice))) / rmse(exp(ames_tr$pred_bic2), exp(ames_tr$lprice))
)

# add bic2 row to df
model_comp <- rbind(model_comp, bic2_df)
kable(model_comp)
```



model           n_predictors   train_adj.r.sq   train_RSE   train_RMSE_usd   test_RMSE_usd      per_inc
-------------  -------------  ---------------  ----------  ---------------  --------------  -----------
basic                      2        0.7708395   0.1825546         33251.82        32666.16   -0.0176131
initial                    8        0.8862698   0.1286059         25639.84        27205.56    0.0610659
BIC                       15        0.9306295   0.1004408         19245.01        21401.03    0.1120302
Forward AIC               30        0.9551957   0.0807203         13544.80        19863.09    0.4664737
Backward AIC              34        0.9556915   0.0802724         13881.31        20301.50    0.4625058
BIC2                      19        0.9444532   0.0898778         16447.75        19184.07    0.1663646

* * *

### Section 3.2 Transformation

Did you decide to transform any variables?  Why or why not? Explain in a few sentences.

* * *

Before beginning analysis here, I log transformed many numeric variables (see code) such as `price`, `area`, and `lLot.Area`. I discussed in the previous assignment why this made sense. These were strictly-positive, highly skewed variables stretched across orders of magnitude and so good candidates for a log transformation, which seeks to make the data more linear, thereby better conforming to the assumptions of our linear model. It's important to do this transformation because the effects of these variables are likely not linear. For example, an increase in area of 100 sq ft for a small property likely has a much greater impact on price than that same unit increase in area for a very large property. 

* * *

### Section 3.3 Variable Interaction

Did you decide to include any variable interactions? Why or why not? Explain in a few sentences.

* * *

An interaction occurs when an independent variable has a different
effect on the outcome depending on the values of another independent variable. They can make interpretation tricky, especially in a model of this size. However given that our primary aim is prediction, I tested out the effect on RMSE of several possible interactions. I found two that three yield slight improvements over the previous model: an interaction between `Overall.Cond` and `Year.Built` and `Year.Remod.Add`; one between `larea` and `lLot.Area`; and one between `lBsmtFin.SF.1` and `lTotal.Bsmt.SF`. There is some intuition as to why interactions between these predictors would be reasonable. The year in which a property was built (or remodeled) might reasonably have an impact on its condition. Area and lot area seem fundamentally tied, as with finished basement surface area and total basement surface area.


```r
# remove previous predictions from dataframes
ames_tri <- ames_train[head(names(ames_train), -6)]
ames_tei <- ames_test[head(names(ames_test), -5)]

# create model with interactions
m_bic2i <- lm(lprice ~ MS.Zoning + Overall.Qual + Overall.Cond * Year.Built *
               Year.Remod.Add + Bsmt.Exposure + Central.Air + Bsmt.Full.Bath +
               Kitchen.Qual + Fireplaces + Garage.Cars + larea * lLot.Area +
               lBsmtFin.SF.1 * lTotal.Bsmt.SF + Neighborhood + Lot.Config + Exter.Qual +
               Condition.1, data = ames_tri)

# add predictions to dataframe
ames_tri <- ames_tri %>% add_predictions(m_bic2i, var = "pred_bic2i")
ames_tei <- ames_tei %>% add_predictions(m_bic2i, var = "pred_bic2i")

# create summary metrics
bic2i_df <- data.frame(
    model = "BIC2i",
    n_predictors = 19,
    train_adj.r.sq = summary(m_bic2i)$adj.r.squared,
    train_RSE = summary(m_bic2i)$sigma,
    train_RMSE_usd = rmse(exp(ames_tri$pred_bic2i), exp(ames_tri$lprice)),
    test_RMSE_usd = rmse(exp(ames_tei$pred_bic2i), exp(ames_tei$lprice)),
    per_inc = (rmse(exp(ames_tei$pred_bic2i), exp(ames_tei$lprice)) - rmse(exp(ames_tri$pred_bic2i), exp(ames_tri$lprice))) / rmse(exp(ames_tri$pred_bic2i), exp(ames_tri$lprice))
)

# add bic2 row to df
model_comp <- rbind(model_comp, bic2i_df)
kable(model_comp)
```



model           n_predictors   train_adj.r.sq   train_RSE   train_RMSE_usd   test_RMSE_usd      per_inc
-------------  -------------  ---------------  ----------  ---------------  --------------  -----------
basic                      2        0.7708395   0.1825546         33251.82        32666.16   -0.0176131
initial                    8        0.8862698   0.1286059         25639.84        27205.56    0.0610659
BIC                       15        0.9306295   0.1004408         19245.01        21401.03    0.1120302
Forward AIC               30        0.9551957   0.0807203         13544.80        19863.09    0.4664737
Backward AIC              34        0.9556915   0.0802724         13881.31        20301.50    0.4625058
BIC2                      19        0.9444532   0.0898778         16447.75        19184.07    0.1663646
BIC2i                     19        0.9473796   0.0874783         15781.89        18026.22    0.1422092

We can see from this table that the RMSE on the testing set of the interaction model is \$1,158 less than the previous BIC model without these interactions. 


* * *

### Section 3.4 Variable Selection

What method did you use to select the variables you included? Why did you select the method you used? Explain in a few sentences.

* * *

AFter EDA I used my intuition to build a basic model. I then used stepwise algorithms to construct models using AIC and BIC as the selection criteira. I also attempted to extract the best model from a Bayesian Model Averaging exercise. Determining that the AIC step models had too many predictors and too large of an increase in RMSE on the test set, I focused on improving the BIC model. I added a few predictors included in the AIC models and examined the impact on the RMSE. Lastly I tweaked the model with a few interactions in a trial and error fashion, again examining impact on RMSE, until I reached the final call below


```r
m_bic2i$call
```

```
## lm(formula = lprice ~ MS.Zoning + Overall.Qual + Overall.Cond * 
##     Year.Built * Year.Remod.Add + Bsmt.Exposure + Central.Air + 
##     Bsmt.Full.Bath + Kitchen.Qual + Fireplaces + Garage.Cars + 
##     larea * lLot.Area + lBsmtFin.SF.1 * lTotal.Bsmt.SF + Neighborhood + 
##     Lot.Config + Exter.Qual + Condition.1, data = ames_tri)
```

* * *

### Section 3.5 Model Testing

How did testing the model on out-of-sample data affect whether or how you changed your model? Explain in a few sentences.

* * *

Testing the model on out-of-sample data played a key role in model selection. Initially a 34-predictor model arrived at through a backard stepwise algorithm with AIC as the selection criteria had the highest adjusted R-squared and the lowest RMSE. However, once we tried our models on the out-of-sample testing data, this was no longer true. This AIC model was overfit to the training data. It did not generalize as well to new data as the models with a smaller number of predictors.


* * *

## Part 4 Final Model Assessment

### Section 4.1 Final Model Residual

For your final model, create and briefly interpret an informative plot of the residuals.

* * *

We can return to the standard residual plots with the final model. The residuals vs. fitted values plot maintains random scatter around 0 with no visible trend. The normal Q-Q plot is very similar as we saw previous with only a few values not conforming to our expectation of normality. The scale-location testing the assumptin of equal variance shows a very slight "U" shaped dip but it seems to be largely satisfied. Point 582 is still an outlier but seems to be less influential than the original model.


```r
par(mfrow = c(2,2))
plot(m_bic2i)
```

```
## Warning: not plotting observations with leverage one:
##   450, 642

## Warning: not plotting observations with leverage one:
##   450, 642
```

![](Final_peer_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


* * *

### Section 4.2 Final Model RMSE

For your final model, calculate and briefly comment on the RMSE.

* * *

Of all models explored, the BIC interaction model had the lowest RMSE on the testing data. This metric suggests that the BIC interaction model has the smallest typical error in predicting price of an unseen property.


model   train_RMSE_usd   test_RMSE_usd 
------  ---------------  --------------
BIC2i   15781.89         18026.22      


* * *

### Section 4.3 Final Model Evaluation

What are some strengths and weaknesses of your model?

* * *

The strength of the model is that is provides a simple formula for generating a credible range of expected values for predicting the price of a given property. It is simple in the sense that all of the parameters in the model can be easily entered into the formula. The lack of discernible trend in the residuals plot and RMSE values on the testing and validation sets suggest the model is effective in producing a prediction with a quantifiable level of uncertainty.

The weakness of the model is the magnitude of the prediction interval itself. The RMSE, representing a typical prediction error, of $18,000 still leaves room for improvement. Moreover, while the model is certainly not a black box (we can see each variable included), it's not entirely obvious why some variables were retained and others were left out. For instance, 5 of the 19 predictors were related to basements. This seems like it would place greater emphasis on basements than would be reasonable.


* * *

### Section 4.4 Final Model Validation

Testing your final model on a separate, validation data set is a great way to determine how your model will perform in real-life practice. 

You will use the “ames_validation” dataset to do some additional assessment of your final model. Discuss your findings, be sure to mention:
* What is the RMSE of your final model when applied to the validation data?  
* How does this value compare to that of the training data and/or testing data?
* What percentage of the 95% predictive confidence (or credible) intervals contain the true price of the house in the validation data set?  
* From this result, does your final model properly reflect uncertainty?


```r
load("ames_validation.Rdata")
```

* * *

As I did for the testing data, I need to perform the same cleaning operations on the validation data so all columns and filtering matches the training data.


```r
# make factor variables where needed
ames_validation$MS.SubClass <- factor(ames_validation$MS.SubClass)
ames_validation$Mo.Sold <- factor(ames_validation$Mo.Sold)

# recode missing values that should be none
ames_validation$Bsmt.Qual <- fct_explicit_na(ames_validation$Bsmt.Qual, na_level = "None")
ames_validation$Bsmt.Cond <- fct_explicit_na(ames_validation$Bsmt.Cond, na_level = "None")
ames_validation$Bsmt.Exposure <- fct_explicit_na(ames_validation$Bsmt.Exposure, na_level = "None")
ames_validation$BsmtFin.Type.1 <- fct_explicit_na(ames_validation$BsmtFin.Type.1, na_level = "None")
ames_validation$BsmtFin.Type.2 <- fct_explicit_na(ames_validation$BsmtFin.Type.2, na_level = "None")
ames_validation$Fireplace.Qu <- fct_explicit_na(ames_validation$Fireplace.Qu, na_level = "None")
ames_validation$Garage.Type <- fct_explicit_na(ames_validation$Garage.Type, na_level = "None")
ames_validation$Garage.Finish <- fct_explicit_na(ames_validation$Garage.Finish, na_level = "None")
ames_validation$Pool.QC <- fct_explicit_na(ames_validation$Pool.QC, na_level = "None")
ames_validation$Fence <- fct_explicit_na(ames_validation$Fence, na_level = "None")
ames_validation$Misc.Feature <- fct_explicit_na(ames_validation$Misc.Feature, na_level = "None")
ames_validation$Garage.Qual <- fct_explicit_na(ames_validation$Garage.Qual, na_level = "None")
ames_validation$Garage.Cond <- fct_explicit_na(ames_validation$Garage.Cond, na_level = "None")
ames_validation$Alley <- fct_explicit_na(ames_validation$Alley, na_level = "None")

ames_validation <- ames_validation %>%
    # keep only normal sale conditions and conventional warranty deed sale types
    filter(Sale.Condition == "Normal" & Sale.Type == "WD ") %>%
    # then remove those columns which now only have one level or missing values 
    dplyr::select(-c(Sale.Condition, Sale.Type, Lot.Frontage, Garage.Yr.Blt)) %>%
    # remove a very small number of observations missing this variable
    filter(!is.na(Mas.Vnr.Area)) %>%
    filter(!is.na(Bsmt.Full.Bath)) %>%
    # needed for validation set
    filter(!MS.Zoning == "A (agr)")

# log transformations of most numeric variables
ames_validation <- ames_validation %>%
    mutate(larea = log(area),
           lprice = log(price),
           lLot.Area = log(Lot.Area),
           lMas.Vnr.Area = log(Mas.Vnr.Area + 1),
           lBsmtFin.SF.1 = log(BsmtFin.SF.1 + 1),
           lBsmtFin.SF.2 = log(BsmtFin.SF.2 + 1),
           lBsmt.Unf.SF = log(Bsmt.Unf.SF + 1),
           lTotal.Bsmt.SF = log(Total.Bsmt.SF + 1),
           lX1st.Flr.SF = log(X1st.Flr.SF),
           lX2nd.Flr.SF = log(X2nd.Flr.SF + 1),
           lLow.Qual.Fin.SF = log(Low.Qual.Fin.SF + 1),
           lGarage.Area = log(Garage.Area + 1),
           lWood.Deck.SF = log(Wood.Deck.SF + 1),
           lOpen.Porch.SF = log(Open.Porch.SF + 1),
           lEnclosed.Porch = log(Enclosed.Porch + 1),
           lX3Ssn.Porch = log(X3Ssn.Porch + 1),
           lScreen.Porch = log(Screen.Porch + 1),
           lPool.Area = log(Pool.Area + 1),
           lMisc.Val = log(Misc.Val + 1)) %>%
    dplyr::select(-c(area, price, Lot.Area, Mas.Vnr.Area, BsmtFin.SF.1, BsmtFin.SF.2,
                     Bsmt.Unf.SF, Total.Bsmt.SF, X1st.Flr.SF, X2nd.Flr.SF,
                     Low.Qual.Fin.SF, Garage.Area, Wood.Deck.SF, Open.Porch.SF,
                     Enclosed.Porch, X3Ssn.Porch, Screen.Porch, Pool.Area, Misc.Val))

# then keep only columns that remain in ames_test (after removing predictions)
ames_validation <- ames_validation[head(names(ames_train), -6)]
```

I can then calculate the final model's RMSE on the validation set. As it is more unseen data, we'd expect it to be similar to the RMSE on the testing set, and it is. In this case, it happens to be slightly lower. The two similar RMSE values on the testing and validation sets should give us some confidence in the ability of the model to predict prices of new properties. While the magnitude of the error may be more than we would like, it is good to have some confidence in the reliability and range of the measurement. 


```r
# remove predictions from dataframe
ames_validation <- ames_validation %>% add_predictions(m_bic2i)

# extract last row of table and calculate RMSE on validation set
bic2_inter <- model_comp[7,]
bic2_inter$val_RMSE_usd <-  rmse(exp(ames_validation$pred), exp(ames_validation$lprice))
kable(bic2_inter, row.names = FALSE)
```



model    n_predictors   train_adj.r.sq   train_RSE   train_RMSE_usd   test_RMSE_usd     per_inc   val_RMSE_usd
------  -------------  ---------------  ----------  ---------------  --------------  ----------  -------------
BIC2i              19        0.9473796   0.0874783         15781.89        18026.22   0.1422092       17824.82

As we'd expectm, 95% of the 95% predictive intervals contain the true price of the house in the validation dataset.


```r
ames_val <- ames_validation %>% dplyr::select(-pred)
# Predict prices
pred.bic2i <- predict(m_bic2i, ames_val, interval = "prediction")

# Calculate proportion of observations that fall within prediction intervals
coverage.prob <- mean(ames_val$lprice > pred.bic2i[,"lwr"] &
                            ames_val$lprice < pred.bic2i[,"upr"])
coverage.prob
```

```
## [1] 0.9449036
```

* * *

## Part 5 Conclusion

Provide a brief summary of your results, and a brief discussion of what you have learned about the data and your model. 

* * *

In this exercise, I was able to build a linear model with 19 predictors that can produce predictions of a property price with a typical error of about $18,000. I was surprised that, in addition to neighborhood, some data on school districts was not available. This is important for many home buyers. The frequentist methods I used were effective, but I should return to the problem with a more committed Bayesian effort in the future.

* * *