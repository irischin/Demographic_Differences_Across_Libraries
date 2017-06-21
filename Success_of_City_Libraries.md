What contributes to the success of a library?
================

From our dataset, we can also try to build a model that would allow us to predict a library's success. Success can of course be defined and measured in various ways -- for the current analysis, we'll use total number of ***registered users*** as our metric.

We first import relevant packages and also read in our dataset.

``` r
library(reshape2)
library(dplyr)
library(ggplot2)
library(caret)
library(car)
library(gridExtra)

LIB2012<-read.table("lib_2012_and_demo.txt", header = T, stringsAsFactors = F)
```

Then we can subset our data to include only factors that we think might contribute to the number of registered users for a library annually.

In particular, I included our outcome variable (i.e., `REGBOR`) as well as the following predictor variables:

-   `POPU_UND`: population of legal service area (Again, here I'm using the unduplicated population calculation as in the other analyses. For more information on how this is calculated, please refer to the documentation file made available by the *Institute of Museum and Library Services*.)
-   `BKVOL`: size of print collection
-   `AUDIO_PH`: size of physical, audio collection
-   `VIDEO_PH`: size of physical, video collection
-   `DATABASE`: size of licensed databases
-   `HRS_OPEN`: total number of public service hours
-   `KIDPRO`: number of children oriented programs
-   `YAPRO`: number of young adult oriented programs
-   `TOTPRO`: total number of programs
-   `TOTINCM`: total operating revenue (from federal, state, or other funding sources)
-   `Majority_demo`: majority demographic group of the surrounding area
-   `OBEREG`: region that the library is located in (i.e., New England, Mid East, Great Lakes, etc. For more information on the corresponding code number for each region, please refer to the documentation file)

``` r
LIB2012_sub<-LIB2012 %>%
  filter(C_FSCS=="Y") %>% #we want only the libraries that meet criteria for the FSCS public library definition
  select(REGBOR, 
         POPU_UND,
         BKVOL, AUDIO_PH, VIDEO_PH, DATABASE, HRS_OPEN, KIDPRO, YAPRO, TOTPRO, TOTINCM, Majority_demo, OBEREG)
```

Cleaning and pre-processing
---------------------------

One of the first things we want to do is exclude entries that do not have `REGBOR` information.

``` r
LIB2012_sub<-LIB2012_sub[LIB2012_sub$REGBOR>0,]
```

Because the PLS indicates missing value with negative numbers (typically -1 or -3), we will need to replace those entries with NA.

``` r
LIB2012_sub_narm<-as.data.frame(sapply(LIB2012_sub[,-c(1,12)], function(x) ifelse(x<0, NA, x)))

LIB2012_sub<-data.frame(LIB2012_sub_narm, LIB2012_sub[,c(1,12)])
```

You might also remember that I've included information regarding the total number of children oriented programs, young adult oriented programs, as well as the total number of programs offered at a library. The former two categories are included in the latter category -- so instead of including the total number of programs as a predictor, we should create a new variable that provides information on the number of programs offered at a library that are neither children or young adult oriented (Some examples of such programs might include things like lectures, citizenship classes, or book discussions.).

``` r
LIB2012_sub<-LIB2012_sub %>%
  mutate(OTHER_PRO=TOTPRO-(KIDPRO+YAPRO)) %>%
  select(-TOTPRO)
```

We also need to convert our `Majority_demo` variable from a character class to a factor class. Recall that we only had three libraries in which the majority demographic group of the surrounding community was Asian. Because of the sparsity of such libraries, we will exclude them from the current analysis.

``` r
LIB2012_sub$Majority_demo<-factor(LIB2012_sub$Majority_demo)
levels(LIB2012_sub$Majority_demo)<-c("Hispanic", "Asian", "Black", "White")
LIB2012_sub<-subset(LIB2012_sub, Majority_demo!="Asian")
```

Looking at the `OBEREG` variable, you'll notice that the distributions varies by region. Moreover, there are very few libraries that fall in the outlying areas (i.e., AS, GU, MP, PR, VI; coded as 9).

``` r
table(LIB2012_sub$OBEREG)
```

    ## 
    ##   1   2   3   4   5   6   7   8   9 
    ##  42  51  88  37 110  58  23  75   3

To create a more even distribution of libraries by region, similar/nearby regions were combined into larger categories.

-   New England (CT ME MA NH RI VT) and Mid East (DE DC MD NJ NY PA) were combined into the East category
-   Great Lakes (IL IN MI OH WI) and Plains (IA KS MN MO NE ND SC) were combined into the Midwest category
-   Southeast (AL AR FL GA KY LA MS NC SC TN VA WV) was retained as is
-   Southwest (AZ NM OK TX) and Rocky Mountains (CO ID MT UT WY) were combined into the Southwest\_Mountains category
-   Far West (AK CA HI NV OR WA) was retained as is

Because of the sparsity of libraries that were located in the outlying areas (i.e., 3), we will exclude them from the current analysis.

``` r
LIB2012_sub<-subset(LIB2012_sub, OBEREG!=9)

region<-c()
for (i in 1:length(LIB2012_sub$OBEREG)){
  if (LIB2012_sub$OBEREG[i] < 3) {
    region<-c(region, "East")
  } else if (LIB2012_sub$OBEREG[i] < 5) {
    region<-c(region, "Midwest")
  } else if (LIB2012_sub$OBEREG[i] == 5) {
    region<-c(region, "Southeast") 
  } else if (LIB2012_sub$OBEREG[i] < 8) {
    region<-c(region, "Southwest_Mountains")
  } else region<-c(region, "FarWest")
}

LIB2012_sub$Region<-as.factor(region)
LIB2012_sub<-droplevels(LIB2012_sub)
```

Exploratory Analyses
--------------------

Now we can take a quick look at our final dataset, starting with the distributions of our various variables. <img src="Success_of_City_Libraries_files/figure-markdown_github/histograms of variables-1.png" style="display: block; margin: auto;" />

We can see that the variables are highly skewed. We can implement a log transformation to see if the distribution improves.

<img src="Success_of_City_Libraries_files/figure-markdown_github/log transform-1.png" style="display: block; margin: auto;" />

The distributions generally look better, but some variables such as `HRS_OPEN` and `YAPRO` still look non-normal so that might be something to keep in mind.

Now to actually create the log transformed dataframe. Here I'll be using smoothing and a natural log transformation. A boxplot is also included as another way to display the distributions of the different variables. You'll notice the presence of outliers and that is something we will need to keep in mind when we are building our models.

``` r
LIB2012_sub_lg<-cbind(log(LIB2012_sub[,-c(10, 12, 14)]+1), LIB2012_sub[,c(12, 14)])
rownames(LIB2012_sub_lg)<-seq_len(nrow(LIB2012_sub_lg))
head(LIB2012_sub_lg)
```

    ##   POPU_UND    BKVOL  AUDIO_PH  VIDEO_PH DATABASE HRS_OPEN   KIDPRO
    ## 1 12.60767 13.14198 10.335010 10.986597 4.204693 9.269458 6.569481
    ## 2 11.51636 12.78876  9.923241 10.263083 4.343805 8.871365 6.694562
    ## 3  9.96876 10.85083  7.994632  7.716015 4.262680 7.712891 3.931826
    ## 4 10.88521 11.18100  8.071843  8.070281 4.330733 8.135054 5.894403
    ## 5 10.18407 11.16535  8.280458  8.097426 4.290459 8.160804 4.290459
    ## 6 11.40322 11.22265  8.204672  8.415603 4.304065 7.921173 5.616771
    ##      YAPRO  TOTINCM    REGBOR OTHER_PRO Majority_demo    Region
    ## 1 5.075174 16.11762 12.220114  4.905275         White   FarWest
    ## 2 2.890372 15.57817 10.939391  4.859812         White   FarWest
    ## 3 0.000000 13.02267  9.784253  4.934474         White Southeast
    ## 4 3.091042 14.25277 10.605347  2.302585         White Southeast
    ## 5 2.079442 13.31039  9.755625  2.833213         White Southeast
    ## 6 2.639057 13.94643 10.689806  5.659482         White Southeast

<img src="Success_of_City_Libraries_files/figure-markdown_github/logtransform boxplot-1.png" style="display: block; margin: auto;" />

We can also generate a correlation and scatterplot matrix to examine how our variables relate to each other.

``` r
cor(LIB2012_sub_lg[,-c(12:13)], use="complete.obs")
```

    ##            POPU_UND     BKVOL  AUDIO_PH  VIDEO_PH  DATABASE  HRS_OPEN
    ## POPU_UND  1.0000000 0.8836217 0.7878455 0.7155886 0.2946864 0.8339106
    ## BKVOL     0.8836217 1.0000000 0.8755484 0.7872432 0.3744942 0.8539460
    ## AUDIO_PH  0.7878455 0.8755484 1.0000000 0.8348911 0.3645223 0.7651432
    ## VIDEO_PH  0.7155886 0.7872432 0.8348911 1.0000000 0.3219380 0.7002054
    ## DATABASE  0.2946864 0.3744942 0.3645223 0.3219380 1.0000000 0.3733279
    ## HRS_OPEN  0.8339106 0.8539460 0.7651432 0.7002054 0.3733279 1.0000000
    ## KIDPRO    0.7576639 0.7902615 0.7839802 0.7004720 0.3263174 0.7893627
    ## YAPRO     0.5541014 0.5783012 0.5584998 0.5094965 0.2337491 0.5987048
    ## TOTINCM   0.8546543 0.9192621 0.9095515 0.8213633 0.3631566 0.8276785
    ## REGBOR    0.8976818 0.8594861 0.8020163 0.7255662 0.3020733 0.8041173
    ## OTHER_PRO 0.6195716 0.6741869 0.6664620 0.6015583 0.3037922 0.6765053
    ##              KIDPRO     YAPRO   TOTINCM    REGBOR OTHER_PRO
    ## POPU_UND  0.7576639 0.5541014 0.8546543 0.8976818 0.6195716
    ## BKVOL     0.7902615 0.5783012 0.9192621 0.8594861 0.6741869
    ## AUDIO_PH  0.7839802 0.5584998 0.9095515 0.8020163 0.6664620
    ## VIDEO_PH  0.7004720 0.5094965 0.8213633 0.7255662 0.6015583
    ## DATABASE  0.3263174 0.2337491 0.3631566 0.3020733 0.3037922
    ## HRS_OPEN  0.7893627 0.5987048 0.8276785 0.8041173 0.6765053
    ## KIDPRO    1.0000000 0.6523415 0.8349093 0.7856536 0.7426957
    ## YAPRO     0.6523415 1.0000000 0.6390577 0.5956656 0.6036030
    ## TOTINCM   0.8349093 0.6390577 1.0000000 0.8745158 0.7065302
    ## REGBOR    0.7856536 0.5956656 0.8745158 1.0000000 0.6320991
    ## OTHER_PRO 0.7426957 0.6036030 0.7065302 0.6320991 1.0000000

<img src="Success_of_City_Libraries_files/figure-markdown_github/scatterplot matrix-1.png" style="display: block; margin: auto;" />

From both the correlation matrix and scatterplots, we see that some variables are highly correlated with each other. This is something to keep in mind as we might encounter issues with multicollinearity.

Building the models
-------------------

We'll be using three types of models to predict a library's success as measured by the total number of registered users: (1) linear regression, (2) linear regression with elastic net regularization, and (3) random forests. In addition, we'll be using (10-fold) cross-validation to evaluate how well our model actually predicts the number of registered users.

So that we can ensure that the different models are working with exactly the same data (e.g. training and test sets), we first create a single set of test/training partitions that could be used across the different models.

``` r
set.seed(42)
model_folds<-createFolds(LIB2012_sub_lg$REGBOR, k=10)
```

To deal with missing values, we'll include a `preProcess` function that will use K-Nearest Neighbor imputation for our missing values. With KNN-imputation, `preProcess` will automatically center and scale our predictors.

### Linear Regression Model

``` r
model_lm<-train(REGBOR~., data=LIB2012_sub_lg,
                 method="lm",
                 na.action=na.pass,
                 preProcess=c("knnImpute"),
                 trControl=trainControl(method="cv", number=10, index=model_folds, verboseIter=F))
```

We can now look at the results of our model and the estimated performance metric -- here, the root mean square error (RMSE).

``` r
print(model_lm$results)
```

    ##   intercept      RMSE  Rsquared    RMSESD RsquaredSD
    ## 1      TRUE 0.5291009 0.8085578 0.0257282 0.01496709

``` r
print(summary(model_lm))
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.87070 -0.21263  0.02619  0.26665  1.27709 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               11.036357   0.019537 564.892  < 2e-16 ***
    ## POPU_UND                   0.498047   0.050319   9.898  < 2e-16 ***
    ## BKVOL                      0.092124   0.062436   1.476   0.1408    
    ## AUDIO_PH                   0.003304   0.054527   0.061   0.9517    
    ## VIDEO_PH                  -0.002173   0.037172  -0.058   0.9534    
    ## DATABASE                  -0.021447   0.023799  -0.901   0.3680    
    ## HRS_OPEN                  -0.047348   0.044874  -1.055   0.2919    
    ## KIDPRO                     0.105005   0.041586   2.525   0.0119 *  
    ## YAPRO                      0.053818   0.027974   1.924   0.0550 .  
    ## TOTINCM                    0.443063   0.071582   6.190 1.32e-09 ***
    ## OTHER_PRO                  0.001996   0.031688   0.063   0.9498    
    ## Majority_demoBlack        -0.029311   0.030952  -0.947   0.3441    
    ## Majority_demoWhite        -0.009042   0.029881  -0.303   0.7623    
    ## RegionFarWest              0.128577   0.027184   4.730 2.98e-06 ***
    ## RegionMidwest              0.060572   0.027758   2.182   0.0296 *  
    ## RegionSoutheast            0.197956   0.028034   7.061 6.01e-12 ***
    ## RegionSouthwest_Mountains  0.156150   0.026641   5.861 8.69e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4298 on 467 degrees of freedom
    ## Multiple R-squared:  0.8721, Adjusted R-squared:  0.8677 
    ## F-statistic:   199 on 16 and 467 DF,  p-value: < 2.2e-16

We can also check whether any assumptions of linear regression were violated.

1.**Mean of residuals is zero**:

``` r
mean(residuals(model_lm))
```

    ## [1] 1.88423e-17

2.**Homoscedasticity of residuals**: we see that our residuals show some heteroscedasticity (see the plot of residuals vs. fitted values). However, OLS is somewhat robust to violations of homoscedasticity and given that the model will still give us unbiased coefficient estimates, we won't worry about it too much here. <img src="Success_of_City_Libraries_files/figure-markdown_github/lm homoscedasticity-1.png" style="display: block; margin: auto;" />

3.**Autocorrelation**: we can see that our data does not appear to be autocorrelated

``` r
acf(residuals(model_lm$finalModel), main="Autocorrelation of residuals")
```

<img src="Success_of_City_Libraries_files/figure-markdown_github/lm autocorrelation-1.png" style="display: block; margin: auto;" />

``` r
durbinWatsonTest(model_lm$finalModel)
```

    ##  lag Autocorrelation D-W Statistic p-value
    ##    1      0.06581151      1.867142   0.078
    ##  Alternative hypothesis: rho != 0

4**Multicollinearity**: Using a cut-off of 4 for the variance inflation factor (IVF), we see evidence of multicollinearity. This is not too surprising, given that as mentioned earlier, we found that some variables were highly correlated with each other.

``` r
vif(model_lm$finalModel)
```

    ##                  POPU_UND                     BKVOL 
    ##                  6.619690                 10.191706 
    ##                  AUDIO_PH                  VIDEO_PH 
    ##                  7.773256                  3.612475 
    ##                  DATABASE                  HRS_OPEN 
    ##                  1.480807                  5.264551 
    ##                    KIDPRO                     YAPRO 
    ##                  4.521383                  2.045956 
    ##                   TOTINCM                 OTHER_PRO 
    ##                 13.396491                  2.625310 
    ##        Majority_demoBlack        Majority_demoWhite 
    ##                  2.504746                  2.334453 
    ##             RegionFarWest             RegionMidwest 
    ##                  1.931986                  2.014525 
    ##           RegionSoutheast RegionSouthwest_Mountains 
    ##                  2.054648                  1.855592

This may not be a huge issue if one is not too concerned about the role of the individual predictors. Nonetheless, we can try to address the multicollinearity issue, by including a principle component analysis in our regression.

``` r
model_pca_lm<-train(REGBOR~., data=LIB2012_sub_lg,
                     method="lm",
                     na.action=na.pass,
                     preProcess=c("knnImpute", "pca"),
                     trControl=trainControl(method="cv", number=10, index=model_folds, verboseIter=F))

print(model_pca_lm$results)
```

    ##   intercept      RMSE  Rsquared     RMSESD RsquaredSD
    ## 1      TRUE 0.5218497 0.8108707 0.02123935 0.01217715

``` r
print(summary(model_pca_lm))
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.80576 -0.24488  0.02248  0.30790  1.15882 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 11.036357   0.020836 529.683  < 2e-16 ***
    ## PC1         -0.390520   0.007768 -50.274  < 2e-16 ***
    ## PC2          0.019307   0.015492   1.246 0.213299    
    ## PC3         -0.138164   0.016719  -8.264 1.44e-15 ***
    ## PC4          0.074835   0.018426   4.061 5.71e-05 ***
    ## PC5         -0.066155   0.019436  -3.404 0.000721 ***
    ## PC6          0.114661   0.025048   4.578 6.02e-06 ***
    ## PC7         -0.090188   0.028722  -3.140 0.001795 ** 
    ## PC8          0.200873   0.033153   6.059 2.80e-09 ***
    ## PC9         -0.230772   0.039042  -5.911 6.53e-09 ***
    ## PC10         0.076706   0.041275   1.858 0.063735 .  
    ## PC11         0.117252   0.045380   2.584 0.010073 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4584 on 472 degrees of freedom
    ## Multiple R-squared:  0.853,  Adjusted R-squared:  0.8495 
    ## F-statistic: 248.9 on 11 and 472 DF,  p-value: < 2.2e-16

Comparing the RMSE of the first linear regression model and the second one that included a PCA, we see that the second model doesn't seem to perform any better than our first model.

We can again check our assumptions really quickly:

1.**Mean of residuals is zero**:

``` r
mean(residuals(model_pca_lm))
```

    ## [1] 1.058062e-17

2.**Homoscedasticity of residuals**: <img src="Success_of_City_Libraries_files/figure-markdown_github/lm pca homoscedasticity-1.png" style="display: block; margin: auto;" />

3.**Autocorrelation**:

``` r
acf(residuals(model_pca_lm), main="Autocorrelation of residuals")
```

<img src="Success_of_City_Libraries_files/figure-markdown_github/lm pca autocorrelation-1.png" style="display: block; margin: auto;" />

``` r
durbinWatsonTest(model_pca_lm$finalModel) #notice here, it seems that we have violated autocorrelation assumption
```

    ##  lag Autocorrelation D-W Statistic p-value
    ##    1       0.1089336      1.780763   0.006
    ##  Alternative hypothesis: rho != 0

4.**Multicollinearity** (given that we incorporated a PCA, this should address the multicollinearity issue, but we'll include a test for violations of multicollinearity here anyway):

``` r
vif(model_pca_lm$finalModel)
```

    ##  PC1  PC2  PC3  PC4  PC5  PC6  PC7  PC8  PC9 PC10 PC11 
    ##    1    1    1    1    1    1    1    1    1    1    1

We can of course use other models. In particular, we'll be running two additional types of models: linear regression with elastic net regularization and random forests.

### Linear regression with elastic net regularization

``` r
model_glmnet<-train(REGBOR~., data=LIB2012_sub_lg,
                        method="glmnet",
                        na.action=na.pass,
                        preProcess=c("knnImpute"),
                        trControl=trainControl(method="cv", number=10, index=model_folds, verboseIter=F))

print(model_glmnet$bestTune)
```

    ##   alpha     lambda
    ## 8     1 0.02119413

``` r
print(model_glmnet$results)
```

    ##   alpha      lambda      RMSE  Rsquared     RMSESD  RsquaredSD
    ## 1  0.10 0.002119413 0.5230436 0.8124161 0.02411107 0.012491289
    ## 2  0.10 0.021194132 0.5018741 0.8257297 0.01928329 0.007215012
    ## 3  0.10 0.211941325 0.4900821 0.8348790 0.01944475 0.008240416
    ## 4  0.55 0.002119413 0.5190488 0.8150595 0.02261450 0.010721625
    ## 5  0.55 0.021194132 0.4900059 0.8333438 0.02067668 0.009102992
    ## 6  0.55 0.211941325 0.5075924 0.8358586 0.02199293 0.011206121
    ## 7  1.00 0.002119413 0.5146271 0.8179622 0.02237683 0.009473805
    ## 8  1.00 0.021194132 0.4879481 0.8344949 0.02015517 0.010563190
    ## 9  1.00 0.211941325 0.5352455 0.8318350 0.02768350 0.018160119

We find that under the best tuning parameters (i.e., apha = 1, lambda = 0.0212), our RMSE is 0.488 and our Rsquared is .834.

We can also take a look at our residuals: <img src="Success_of_City_Libraries_files/figure-markdown_github/elastic regression residuals-1.png" style="display: block; margin: auto;" />

### Random Forests

``` r
model_rf<-train(REGBOR~., data=LIB2012_sub_lg,
                    method="ranger",
                    na.action=na.pass,
                    preProcess=c("knnImpute"),
                    trControl=trainControl(method="cv", number=10, index=model_folds,verboseIter=F))

print(model_rf$bestTune)
```

    ##   mtry
    ## 2    9

``` r
print(model_rf$results)
```

    ##   mtry      RMSE  Rsquared     RMSESD RsquaredSD
    ## 1    2 0.6112891 0.7662947 0.05887116 0.05051551
    ## 2    9 0.5814699 0.7733947 0.06600386 0.04851384
    ## 3   16 0.5825800 0.7715085 0.06522896 0.04651122

We can also try to run another model with our own specifications of the mytry tuning grid to see if we get a better model.

``` r
model_rf2<-train(REGBOR~., data=LIB2012_sub_lg,
                method="ranger",
                tuneGrid=data.frame(mtry=c(6:15)),
                na.action=na.pass,
                preProcess=c("knnImpute"),
                trControl=trainControl(method="cv", number=10, index=model_folds,verboseIter=F))

print(model_rf2$bestTune)
```

    ##   mtry
    ## 8   13

``` r
print(model_rf2$results)
```

    ##    mtry      RMSE  Rsquared     RMSESD RsquaredSD
    ## 1     6 0.5833642 0.7738212 0.06332640 0.04758889
    ## 2     7 0.5823812 0.7740255 0.06372907 0.04767817
    ## 3     8 0.5807405 0.7746148 0.06557158 0.04782050
    ## 4     9 0.5802177 0.7747357 0.06417375 0.04644191
    ## 5    10 0.5805173 0.7739240 0.06501547 0.04763401
    ## 6    11 0.5812122 0.7733859 0.06645543 0.04761384
    ## 7    12 0.5822996 0.7726050 0.06517878 0.04698548
    ## 8    13 0.5801976 0.7738412 0.06520725 0.04649220
    ## 9    14 0.5817569 0.7723261 0.06503860 0.04626441
    ## 10   15 0.5829008 0.7710006 0.06493738 0.04620812

To examine the residuals: <img src="Success_of_City_Libraries_files/figure-markdown_github/random forest model 2 residuals-1.png" style="display: block; margin: auto;" />

Comparing models
----------------

We can now compare the different models that we built to determine which has the best performance (as measured by the RMSE).

``` r
model_compare<-resamples(list(Linear_regression=model_lm,
                              Linear_pca_regression=model_pca_lm, 
                              Elastic_regression=model_glmnet, 
                              Random_forest_A=model_rf,
                              Random_forest_B=model_rf2))

summary(model_compare)
```

    ## 
    ## Call:
    ## summary.resamples(object = model_compare)
    ## 
    ## Models: Linear_regression, Linear_pca_regression, Elastic_regression, Random_forest_A, Random_forest_B 
    ## Number of resamples: 10 
    ## 
    ## RMSE 
    ##                         Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
    ## Linear_regression     0.5009  0.5114 0.5201 0.5291  0.5388 0.5809    0
    ## Linear_pca_regression 0.5044  0.5085 0.5157 0.5218  0.5267 0.5744    0
    ## Elastic_regression    0.4458  0.4772 0.4922 0.4879  0.4990 0.5206    0
    ## Random_forest_A       0.4811  0.5421 0.5952 0.5815  0.6119 0.6955    0
    ## Random_forest_B       0.4767  0.5410 0.6011 0.5802  0.6105 0.6886    0
    ## 
    ## Rsquared 
    ##                         Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
    ## Linear_regression     0.7850  0.7953 0.8120 0.8086  0.8200 0.8260    0
    ## Linear_pca_regression 0.7868  0.8050 0.8139 0.8109  0.8201 0.8230    0
    ## Elastic_regression    0.8198  0.8280 0.8329 0.8345  0.8413 0.8544    0
    ## Random_forest_A       0.6956  0.7474 0.7703 0.7734  0.8078 0.8377    0
    ## Random_forest_B       0.7006  0.7483 0.7676 0.7738  0.8071 0.8370    0

Looking across models, we find that the linear regression model with elastic net regularization appears to perform the best -- with the lowest mean RMSE and highest mean Rsquared.

Dealing with outliers
=====================

Recall the presence of outliers as revealed by our boxplots. We can try to re-run our models with the exclusion of some outliers to determine whether this will improve our model performance. In particular, I'll be excluding lower bound outliers in the population of legal service variable (`POPU_UND`). In our analysis, we have focused on libraries located in city. However, a question we might ask is to what extent a library classified as being located in a city necessarily services a city-like population.

Let's look at the `POPU_UND` outliers.

``` r
sort(boxplot.stats(LIB2012_sub_lg$POPU_UND)$out)
```

    ##  [1]  5.262690  6.131226  7.996317  8.018955  8.049746  8.212297  8.632128
    ##  [8]  8.923325  8.987197 14.124780 14.196383 14.196462 14.238165 14.297648
    ## [15] 14.574675 14.610804 14.726648 14.807131 15.043867 15.157147

This tells us that for the lower-bound outliers, their population of legal service is less than 8000 (this was calculated by computing the exponential function of the largest lower-bound outlier, i.e., 8.987197). This is relatively small and may not be representative of other libraries located in cities. Thus, for the following analysis, we'll disregard those smaller libraries and re-implement our models to see if performance improves.

To create new subsetted data that excludes those smaller library outliers:

``` r
LIB2012_sub_lg_nlout<-LIB2012_sub_lg %>%
  filter(POPU_UND >8.987197)
rownames(LIB2012_sub_lg_nlout)<-seq_len(nrow(LIB2012_sub_lg_nlout))
```

We can take a quick look at the correlation and scatterplot matrix of our subsetted dataset:

``` r
cor(LIB2012_sub_lg_nlout[,-c(12:13)], use="complete.obs")
```

    ##            POPU_UND     BKVOL  AUDIO_PH  VIDEO_PH  DATABASE  HRS_OPEN
    ## POPU_UND  1.0000000 0.8620623 0.7470084 0.6696840 0.3074840 0.8552101
    ## BKVOL     0.8620623 1.0000000 0.8544698 0.7561132 0.3870988 0.8613873
    ## AUDIO_PH  0.7470084 0.8544698 1.0000000 0.8097972 0.3768869 0.7640181
    ## VIDEO_PH  0.6696840 0.7561132 0.8097972 1.0000000 0.3227812 0.6876758
    ## DATABASE  0.3074840 0.3870988 0.3768869 0.3227812 1.0000000 0.3709358
    ## HRS_OPEN  0.8552101 0.8613873 0.7640181 0.6876758 0.3709358 1.0000000
    ## KIDPRO    0.7407435 0.7723550 0.7561846 0.6721042 0.3376563 0.7926753
    ## YAPRO     0.5398302 0.5592475 0.5438068 0.4935850 0.2344246 0.5845960
    ## TOTINCM   0.8289014 0.9071620 0.8909567 0.7942281 0.3775147 0.8365954
    ## REGBOR    0.8730943 0.8319112 0.7609013 0.6831430 0.3154155 0.8136903
    ## OTHER_PRO 0.6032104 0.6557553 0.6468786 0.5807999 0.3048922 0.6643921
    ##              KIDPRO     YAPRO   TOTINCM    REGBOR OTHER_PRO
    ## POPU_UND  0.7407435 0.5398302 0.8289014 0.8730943 0.6032104
    ## BKVOL     0.7723550 0.5592475 0.9071620 0.8319112 0.6557553
    ## AUDIO_PH  0.7561846 0.5438068 0.8909567 0.7609013 0.6468786
    ## VIDEO_PH  0.6721042 0.4935850 0.7942281 0.6831430 0.5807999
    ## DATABASE  0.3376563 0.2344246 0.3775147 0.3154155 0.3048922
    ## HRS_OPEN  0.7926753 0.5845960 0.8365954 0.8136903 0.6643921
    ## KIDPRO    1.0000000 0.6435676 0.8152768 0.7611820 0.7267589
    ## YAPRO     0.6435676 1.0000000 0.6345245 0.5814287 0.5828571
    ## TOTINCM   0.8152768 0.6345245 1.0000000 0.8493974 0.6932918
    ## REGBOR    0.7611820 0.5814287 0.8493974 1.0000000 0.6092441
    ## OTHER_PRO 0.7267589 0.5828571 0.6932918 0.6092441 1.0000000

<img src="Success_of_City_Libraries_files/figure-markdown_github/ex_outlier scatterplot matrix-1.png" style="display: block; margin: auto;" />

To create a new partition set so that our new models can be compared later:

``` r
model_folds2<-createFolds(LIB2012_sub_lg_nlout$REGBOR, k=10)
```

### Linear regression

From our correlation matrix, we can tell that some variables are highly correlated with each other. As such, we'll incorporate a PCA into our linear regression model to address potential issues with multicollinearity.

``` r
model_pca_lm_nlout<-train(REGBOR~., data=LIB2012_sub_lg_nlout,
                method="lm",
                na.action=na.pass,
                preProcess=c("knnImpute", "pca"),
                trControl=trainControl(method="cv", number=10, index=model_folds2, verboseIter=F))

print(model_pca_lm_nlout$results)
```

    ##   intercept      RMSE  Rsquared     RMSESD RsquaredSD
    ## 1      TRUE 0.5126996 0.7769611 0.02468489 0.01879871

``` r
print(summary(model_pca_lm_nlout))
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.98015 -0.23646  0.04347  0.28484  1.09162 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 11.107763   0.020660 537.635  < 2e-16 ***
    ## PC1         -0.346669   0.007771 -44.610  < 2e-16 ***
    ## PC2          0.031092   0.015268   2.036  0.04228 *  
    ## PC3         -0.133384   0.016491  -8.088 5.35e-15 ***
    ## PC4          0.076554   0.018237   4.198 3.23e-05 ***
    ## PC5         -0.059221   0.019208  -3.083  0.00217 ** 
    ## PC6          0.102309   0.024685   4.145 4.05e-05 ***
    ## PC7         -0.012886   0.028805  -0.447  0.65482    
    ## PC8          0.161303   0.032497   4.964 9.74e-07 ***
    ## PC9         -0.192529   0.036465  -5.280 1.99e-07 ***
    ## PC10         0.076611   0.040573   1.888  0.05962 .  
    ## PC11         0.111799   0.044702   2.501  0.01273 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4503 on 463 degrees of freedom
    ## Multiple R-squared:  0.8239, Adjusted R-squared:  0.8197 
    ## F-statistic:   197 on 11 and 463 DF,  p-value: < 2.2e-16

<img src="Success_of_City_Libraries_files/figure-markdown_github/pca linear regression residuals-1.png" style="display: block; margin: auto;" />

### Linear regression with elastic net regularization

``` r
model_glmnet_nlout<-train(REGBOR~., data=LIB2012_sub_lg_nlout,
                    method="glmnet",
                    na.action=na.pass,
                    preProcess=c("knnImpute"),
                    trControl=trainControl(method="cv", number=10, index=model_folds2, verboseIter=F))

print(model_glmnet_nlout$bestTune)
```

    ##   alpha    lambda
    ## 3   0.1 0.1849969

``` r
print(model_glmnet_nlout$results)
```

    ##   alpha      lambda      RMSE  Rsquared     RMSESD RsquaredSD
    ## 1  0.10 0.001849969 0.5452049 0.7533995 0.03190618 0.02926168
    ## 2  0.10 0.018499693 0.5203841 0.7723340 0.02368350 0.01978548
    ## 3  0.10 0.184996928 0.4878324 0.7977307 0.01900835 0.01598571
    ## 4  0.55 0.001849969 0.5416818 0.7562426 0.03079238 0.02748609
    ## 5  0.55 0.018499693 0.5076483 0.7816776 0.02531912 0.01722448
    ## 6  0.55 0.184996928 0.5040930 0.7936772 0.01952542 0.01754268
    ## 7  1.00 0.001849969 0.5382231 0.7588928 0.03066090 0.02655933
    ## 8  1.00 0.018499693 0.5012262 0.7863142 0.02697196 0.01709585
    ## 9  1.00 0.184996928 0.5315277 0.7819272 0.02666278 0.02571668

<img src="Success_of_City_Libraries_files/figure-markdown_github/ex_outlier elastic regression residuals-1.png" style="display: block; margin: auto;" />

### Random forests

``` r
model_rf_nlout<-train(REGBOR~., data=LIB2012_sub_lg_nlout,
                method="ranger",
                na.action=na.pass,
                preProcess=c("knnImpute", "center", "scale"),
                trControl=trainControl(method="cv", number=10, index=model_folds2,verboseIter=F))

print(model_rf_nlout$bestTune)
```

    ##   mtry
    ## 2    9

``` r
print(model_rf_nlout$results)
```

    ##   mtry      RMSE  Rsquared     RMSESD  RsquaredSD
    ## 1    2 0.5419762 0.7634868 0.03499755 0.008064418
    ## 2    9 0.5189386 0.7702190 0.02591807 0.011646094
    ## 3   16 0.5263728 0.7609304 0.02389871 0.011810031

We can again try to run another random forests model with our own specifications of the mytry tuning grid.

``` r
model_rf2_nlout<-train(REGBOR~., data=LIB2012_sub_lg_nlout,
                 method="ranger",
                 tuneGrid=data.frame(mtry=c(4:14)),
                 na.action=na.pass,
                 preProcess=c("knnImpute", "center", "scale"),
                 trControl=trainControl(method="cv", number=10, index=model_folds2,verboseIter=F))

print(model_rf2_nlout$bestTune)
```

    ##   mtry
    ## 5    8

``` r
print(model_rf2_nlout$results)
```

    ##    mtry      RMSE  Rsquared     RMSESD  RsquaredSD
    ## 1     4 0.5242558 0.7698230 0.02773226 0.009709285
    ## 2     5 0.5226094 0.7703836 0.02706607 0.010013298
    ## 3     6 0.5216164 0.7701394 0.02764197 0.008819076
    ## 4     7 0.5198613 0.7708874 0.02571772 0.010768328
    ## 5     8 0.5195383 0.7707201 0.02594460 0.010180177
    ## 6     9 0.5201197 0.7696813 0.02513226 0.010057423
    ## 7    10 0.5199637 0.7691080 0.02415595 0.010668902
    ## 8    11 0.5197803 0.7690667 0.02568503 0.010938215
    ## 9    12 0.5213720 0.7669069 0.02434266 0.010310741
    ## 10   13 0.5231957 0.7651724 0.02370746 0.009665981
    ## 11   14 0.5237874 0.7640515 0.02407423 0.012061323

To examine the residuals: <img src="Success_of_City_Libraries_files/figure-markdown_github/ex_outlier random forest 2 residuals-1.png" style="display: block; margin: auto;" />

Comparing models
----------------

``` r
model_compare_nlout<-resamples(list(Linear_pca_regression=model_pca_lm_nlout, 
                              Elastic_regression=model_glmnet_nlout, 
                              Random_forest_A=model_rf_nlout,
                              Random_forest_B=model_rf2_nlout))

summary(model_compare_nlout)
```

    ## 
    ## Call:
    ## summary.resamples(object = model_compare_nlout)
    ## 
    ## Models: Linear_pca_regression, Elastic_regression, Random_forest_A, Random_forest_B 
    ## Number of resamples: 10 
    ## 
    ## RMSE 
    ##                         Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
    ## Linear_pca_regression 0.4731  0.4952 0.5134 0.5127  0.5279 0.5466    0
    ## Elastic_regression    0.4661  0.4721 0.4845 0.4878  0.4976 0.5185    0
    ## Random_forest_A       0.4842  0.5019 0.5138 0.5189  0.5311 0.5756    0
    ## Random_forest_B       0.4850  0.5043 0.5126 0.5195  0.5311 0.5759    0
    ## 
    ## Rsquared 
    ##                         Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
    ## Linear_pca_regression 0.7373  0.7692 0.7773 0.7770  0.7876 0.8031    0
    ## Elastic_regression    0.7690  0.7895 0.7978 0.7977  0.8097 0.8222    0
    ## Random_forest_A       0.7513  0.7605 0.7745 0.7702  0.7780 0.7839    0
    ## Random_forest_B       0.7578  0.7605 0.7728 0.7707  0.7795 0.7837    0

Again, we see that the linear regression model with elastic net regularization performed the best, with the lowest mean RMSE and highest mean Rsquared.

So, does excluding the lower bound library outliers improve our model? Let's revisit the comparison for our first set of models:

``` r
summary(model_compare)
```

    ## 
    ## Call:
    ## summary.resamples(object = model_compare)
    ## 
    ## Models: Linear_regression, Linear_pca_regression, Elastic_regression, Random_forest_A, Random_forest_B 
    ## Number of resamples: 10 
    ## 
    ## RMSE 
    ##                         Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
    ## Linear_regression     0.5009  0.5114 0.5201 0.5291  0.5388 0.5809    0
    ## Linear_pca_regression 0.5044  0.5085 0.5157 0.5218  0.5267 0.5744    0
    ## Elastic_regression    0.4458  0.4772 0.4922 0.4879  0.4990 0.5206    0
    ## Random_forest_A       0.4811  0.5421 0.5952 0.5815  0.6119 0.6955    0
    ## Random_forest_B       0.4767  0.5410 0.6011 0.5802  0.6105 0.6886    0
    ## 
    ## Rsquared 
    ##                         Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
    ## Linear_regression     0.7850  0.7953 0.8120 0.8086  0.8200 0.8260    0
    ## Linear_pca_regression 0.7868  0.8050 0.8139 0.8109  0.8201 0.8230    0
    ## Elastic_regression    0.8198  0.8280 0.8329 0.8345  0.8413 0.8544    0
    ## Random_forest_A       0.6956  0.7474 0.7703 0.7734  0.8078 0.8377    0
    ## Random_forest_B       0.7006  0.7483 0.7676 0.7738  0.8071 0.8370    0

We find that actually, excluding the lower-bound outliers doesn't really improve our model performance very much so maybe we might want to use the model that includes our complete dataset in the future.

Revisiting our best model
-------------------------

Recall for our models, we log transformed our variables such that they fit a more normal distribution. As such, the predictions of our outcome variable (and RMSE) are also logged transformed, which can be somewhat hard to interpret (especially for the RMSE). To get a better since of how accurate our model is, let's re-transform our predicted values and compare them to their corresponding actual values in their original form (i.e., not log transformed). We'll look at the model that was produced using the complete data set and linear regression with elastic net regularization since that appeared to perform the best.

``` r
y_predict<-predict(model_glmnet, LIB2012_sub_lg)
y_predict<-exp(y_predict)-1

y_actual<-LIB2012_sub$REGBOR

sqrt(mean((y_predict-y_actual)^2))
```

    ## [1] 68901.48

So on average, our model is off by +/- 68,901 users. There are also a couple ways that we can visualize how good our model is. We can plot the actual values against the predicted values as well as the distribution of the errors. Together, they seem to suggest that our model does a somewhat reasonable job, although it has more problems when predicting larger values. ![](Success_of_City_Libraries_files/figure-markdown_github/scatterplot%20and%20hist%20RMSE-1.png)
