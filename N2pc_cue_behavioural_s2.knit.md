---
title: "behavioural measures and n2pc to cue"
author: "jac"
date: "2024-05-26"
output:
  html_document: default
  pdf_document: default
---

Data extracted from fieldtrip cue locked files:
        cfg.channel     = {'PO3','PO7','O1'}; and  cfg.channel     = {'PO4','PO8','O2'};
        cfg.baseline    = [-0.1 -0.00001]; %%% baseline corrected
        



# create laterality index for N2pc (i.e. flip according to cue side)



```r
####function from https://doi.org/10.3389/fpsyg.2015.01171
invfn <- function() {
    ## link
    linkfun <- function(y) -1000/y
    ## inverse link
    linkinv <- function(eta)  -1000/eta
    ## derivative of invlink wrt eta
    mu.eta <- function(eta) { 1000/(eta^2) }
    valideta <- function(eta) TRUE
    link <- "-1000/y"
    structure(list(linkfun = linkfun, linkinv = linkinv,
                   mu.eta = mu.eta, valideta = valideta, 
                   name = link),
              class = "link-glm")
}
```




# behavioural data

```r
pick=data$correct2=="correct" #pick the correct data
dataC=data[pick,]

# responsetime time - submission 1 - not used. 
#model.RT=lmer(responsetime ~ cueV+edgedisCS*texrot+  (1|subj), data=dataC, REML = FALSE);



# responsetime time using glmm as requested by reviewer. BEST FIT - used for paper 26/05/2024
model.RTRR2=glmer(responsetime ~ cueV+edgedisCS*texrot+  (1|subj), data=dataC,family=inverse.gaussian(link = "identity"));


# responsetime time using glmm as requested by reviewer. 

#model.RTRR=glmer(responsetime ~ cueV+edgedisCS*texrot+  (1|subj), data=dataC,family=Gamma(link = "identity"));

# reponsetime time using glmm as requested by reviewer. 
#model.RTRR3=glmer(responsetime ~ cueV+edgedisCS*texrot+  (1|subj), data=dataC,family=Gamma(link = invfn()));

# reponsetime time using glmm as requested by reviewer. won't fit at all. 
#model.RTRR4=glmer(responsetime ~ cueV+edgedisCS*texrot+  (1|subj), data=dataC,family=inverse.gaussian(link = invfn()));




# accuracy
model.ACC=glmer(correct ~ cueV+edgedisCS*texrot+  (1|subj), data=data,family=binomial(link = "logit"));


#certainty
data$certain2 <- ordered (data$certain2, levels = c("guess","unsure","certain"))
model.CERT=multinom(certain2 ~ cueV + edgedisCS*texrot, random =~1|subj, data=data) 
```

```
## # weights:  24 (14 variable)
## initial  value 6335.697069 
## iter  10 value 5881.619769
## final  value 5865.613759 
## converged
```

#N2pc reponse to the cue

```r
model.LI=lmer(csLatindex ~ edgedisCS*texrot+  (1|subj), data=data, REML = FALSE);

#cert and correct model
model.lat_CerCor=lmer(csLatindex ~  certain2*correct2  + (1|subj), data=data, REML = FALSE);
```

#submission 2 behavioral section and N2pc

```r
# model redone as per reviewer request. see below 
#summary(model.RT)
#anova(model.RT, type=3)

# reaction time model 
summary(model.RTRR2)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: inverse.gaussian  ( identity )
## Formula: responsetime ~ cueV + edgedisCS * texrot + (1 | subj)
##    Data: dataC
## 
##      AIC      BIC   logLik deviance df.resid 
##  62949.2  63006.7 -31465.6  62931.2     4426 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.4471 -0.6449 -0.2379  0.3860 12.6431 
## 
## Random effects:
##  Groups   Name        Variance  Std.Dev.
##  subj     (Intercept) 5.066e+03 71.17324
##  Residual             1.340e-04  0.01158
## Number of obs: 4435, groups:  subj, 18
## 
## Fixed effects:
##                       Estimate Std. Error t value Pr(>|z|)    
## (Intercept)           1046.618     20.633  50.724  < 2e-16 ***
## cueVincong              82.754     11.364   7.282 3.29e-13 ***
## edgedisCS               16.760      8.415   1.992  0.04641 *  
## texrotdeg45             28.295     10.156   2.786  0.00534 ** 
## texrotdeg90             34.557     10.956   3.154  0.00161 ** 
## edgedisCS:texrotdeg45  -11.580      9.261  -1.250  0.21115    
## edgedisCS:texrotdeg90  -44.110     10.927  -4.037 5.42e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) cVncng edgdCS txrt45 txrt90 eCS:45
## cueVincong  -0.008                                   
## edgedisCS    0.022 -0.040                            
## texrotdeg45 -0.009  0.013  0.194                     
## texrotdeg90  0.045  0.049  0.115  0.577              
## edgdsCS:t45 -0.016  0.016 -0.852 -0.125 -0.106       
## edgdsCS:t90 -0.010  0.039 -0.580 -0.117 -0.114  0.493
```

```r
confint(model.RTRR2,method="Wald")
```

```
##                             2.5 %      97.5 %
## .sig01                         NA          NA
## .sigma                         NA          NA
## (Intercept)           1006.177452 1087.059162
## cueVincong              60.480273  105.027470
## edgedisCS                0.266516   33.254134
## texrotdeg45              8.389614   48.200831
## texrotdeg90             13.084097   56.030769
## edgedisCS:texrotdeg45  -29.731690    6.571123
## edgedisCS:texrotdeg90  -65.527374  -22.692714
```

```r
Anova(model.RTRR2, type=2)
```

```
## Analysis of Deviance Table (Type II Wald chisquare tests)
## 
## Response: responsetime
##                    Chisq Df Pr(>Chisq)    
## cueV             53.0264  1  3.291e-13 ***
## edgedisCS         0.1729  1  0.6775224    
## texrot            8.6688  2  0.0131095 *  
## edgedisCS:texrot 17.0150  2  0.0002019 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# accuracy model
summary(model.ACC)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: correct ~ cueV + edgedisCS * texrot + (1 | subj)
##    Data: data
## 
##      AIC      BIC   logLik deviance df.resid 
##   5840.4   5893.7  -2912.2   5824.4     5759 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.7002  0.2663  0.4326  0.5710  1.3748 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  subj   (Intercept) 0.3098   0.5566  
## Number of obs: 5767, groups:  subj, 18
## 
## Fixed effects:
##                       Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            1.37915    0.17457   7.900 2.78e-15 ***
## cueVincong            -0.50027    0.08495  -5.889 3.88e-09 ***
## edgedisCS             -0.48078    0.10394  -4.625 3.74e-06 ***
## texrotdeg45            0.02442    0.12035   0.203   0.8392    
## texrotdeg90            0.03325    0.13248   0.251   0.8018    
## edgedisCS:texrotdeg45  0.13607    0.11175   1.218   0.2234    
## edgedisCS:texrotdeg90  0.29012    0.13114   2.212   0.0269 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) cVncng edgdCS txrt45 txrt90 eCS:45
## cueVincong  -0.112                                   
## edgedisCS   -0.420  0.027                            
## texrotdeg45 -0.612  0.030  0.603                     
## texrotdeg90 -0.558  0.045  0.548  0.803              
## edgdsCS:t45  0.389 -0.026 -0.930 -0.569 -0.510       
## edgdsCS:t90  0.334 -0.038 -0.793 -0.479 -0.481  0.737
```

```r
confint(model.ACC,method="Wald")
```

```
##                             2.5 %     97.5 %
## .sig01                         NA         NA
## (Intercept)            1.03700455  1.7212876
## cueVincong            -0.66676686 -0.3337800
## edgedisCS             -0.68450392 -0.2770478
## texrotdeg45           -0.21146841  0.2603075
## texrotdeg90           -0.22639861  0.2929019
## edgedisCS:texrotdeg45 -0.08295518  0.3551023
## edgedisCS:texrotdeg90  0.03308728  0.5471561
```

```r
Anova(model.ACC, type=2) 
```

```
## Analysis of Deviance Table (Type II Wald chisquare tests)
## 
## Response: correct
##                    Chisq Df Pr(>Chisq)    
## cueV             34.6830  1   3.88e-09 ***
## edgedisCS        91.8613  1  < 2.2e-16 ***
## texrot            2.1131  2    0.34766    
## edgedisCS:texrot  5.2692  2    0.07175 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
#ODDS ratios cueVincong
exp(summary(model.ACC)$coefficients["cueVincong",1] + qnorm(c(0.025,0.5,0.975)) * summary(model.ACC)$coefficients["cueVincong",2])
```

```
## [1] 0.5133657 0.6063648 0.7162113
```

```r
#ODDS ratios edgedisCS
exp(summary(model.ACC)$coefficients["edgedisCS",1] + qnorm(c(0.025,0.5,0.975)) * summary(model.ACC)$coefficients["edgedisCS",2])
```

```
## [1] 0.5043404 0.6183035 0.7580183
```

```r
#certainty
summary(model.CERT)
```

```
## Call:
## multinom(formula = certain2 ~ cueV + edgedisCS * texrot, data = data, 
##     random = ~1 | subj)
## 
## Coefficients:
##         (Intercept) cueVincong  edgedisCS texrotdeg45 texrotdeg90
## unsure    0.1008714 -0.1699349 -0.1680716  0.09661896  0.17299966
## certain   0.8396195 -0.3970810 -0.5347076  0.02135604 -0.02568159
##         edgedisCS:texrotdeg45 edgedisCS:texrotdeg90
## unsure            -0.17153951            0.09077053
## certain           -0.03100111            0.22258858
## 
## Std. Errors:
##         (Intercept) cueVincong edgedisCS texrotdeg45 texrotdeg90
## unsure    0.1327765 0.09947872 0.1198456    0.139605   0.1527149
## certain   0.1146335 0.09148210 0.1059960    0.121044   0.1336759
##         edgedisCS:texrotdeg45 edgedisCS:texrotdeg90
## unsure              0.1291715             0.1503994
## certain             0.1145479             0.1345984
## 
## Residual Deviance: 11731.23 
## AIC: 11759.23
```

```r
mtable(model.CERT)
```

```
## 
## Calls:
## model.CERT: multinom(formula = certain2 ~ cueV + edgedisCS * texrot, data = data, 
##     random = ~1 | subj)
## 
## ========================================================
##                                     unsure    certain   
## --------------------------------------------------------
##   (Intercept)                         0.101   0.840***  
##                                      (0.133) (0.115)    
##   cueV: incong                       -0.170  -0.397***  
##                                      (0.099) (0.091)    
##   edgedisCS                          -0.168  -0.535***  
##                                      (0.120) (0.106)    
##   texrot: deg45/deg0                  0.097   0.021     
##                                      (0.140) (0.121)    
##   texrot: deg90/deg0                  0.173  -0.026     
##                                      (0.153) (0.134)    
##   edgedisCS x texrot: deg45/deg0     -0.172  -0.031     
##                                      (0.129) (0.115)    
##   edgedisCS x texrot: deg90/deg0      0.091   0.223     
##                                      (0.150) (0.135)    
## --------------------------------------------------------
##   Log-likelihood                  -5865.614             
##   N                                5767                 
## ========================================================
##   Significance: *** = p < 0.001; ** = p < 0.01;   
##                 * = p < 0.05
```

```r
Anova(model.CERT, type=2)
```

```
## Analysis of Deviance Table (Type II tests)
## 
## Response: certain2
##                  LR Chisq Df Pr(>Chisq)    
## cueV               19.514  2  5.787e-05 ***
## edgedisCS         223.040  2  < 2.2e-16 ***
## texrot              3.578  4    0.46613    
## edgedisCS:texrot   10.175  4    0.03759 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
confint(model.CERT,method="Wald")
```

```
## , , unsure
## 
##                            2.5 %     97.5 %
## (Intercept)           -0.1593658 0.36110861
## cueVincong            -0.3649096 0.02503979
## edgedisCS             -0.4029647 0.06682149
## texrotdeg45           -0.1770018 0.37023968
## texrotdeg90           -0.1263160 0.47231533
## edgedisCS:texrotdeg45 -0.4247109 0.08163192
## edgedisCS:texrotdeg90 -0.2040069 0.38554795
## 
## , , certain
## 
##                            2.5 %     97.5 %
## (Intercept)            0.6149420  1.0642969
## cueVincong            -0.5763826 -0.2177794
## edgedisCS             -0.7424559 -0.3269592
## texrotdeg45           -0.2158858  0.2585979
## texrotdeg90           -0.2876815  0.2363183
## edgedisCS:texrotdeg45 -0.2555108  0.1935086
## edgedisCS:texrotdeg90 -0.0412195  0.4863967
```

```r
### N2pc
summary(model.LI)
```

```
## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
##   method [lmerModLmerTest]
## Formula: csLatindex ~ edgedisCS * texrot + (1 | subj)
##    Data: data
## 
##      AIC      BIC   logLik deviance df.resid 
##  16355.0  16408.2  -8169.5  16339.0     5759 
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -15.9633  -0.4918   0.0005   0.5086   9.6504 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  subj     (Intercept) 0.007156 0.08459 
##  Residual             0.991614 0.99580 
## Number of obs: 5767, groups:  subj, 18
## 
## Fixed effects:
##                         Estimate Std. Error         df t value Pr(>|t|)
## (Intercept)            1.997e-02  4.737e-02  2.685e+02   0.422    0.674
## edgedisCS              4.932e-02  4.059e-02  5.750e+03   1.215    0.224
## texrotdeg45           -2.419e-02  4.598e-02  5.750e+03  -0.526    0.599
## texrotdeg90           -3.411e-02  5.095e-02  5.751e+03  -0.670    0.503
## edgedisCS:texrotdeg45 -3.298e-02  4.359e-02  5.750e+03  -0.757    0.449
## edgedisCS:texrotdeg90 -5.167e-03  5.161e-02  5.752e+03  -0.100    0.920
## 
## Correlation of Fixed Effects:
##             (Intr) edgdCS txrt45 txrt90 eCS:45
## edgedisCS   -0.460                            
## texrotdeg45 -0.847  0.473                     
## texrotdeg90 -0.764  0.427  0.787              
## edgdsCS:t45  0.428 -0.931 -0.425 -0.398       
## edgdsCS:t90  0.362 -0.787 -0.372 -0.355  0.732
```

```r
anova(model.LI,type=3) # no interaction, use type 3
```

```
## Type III Analysis of Variance Table with Satterthwaite's method
##                  Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
## edgedisCS        4.1014  4.1014     1 5749.5  4.1361 0.04202 *
## texrot           0.4446  0.2223     2 5750.2  0.2242 0.79918  
## edgedisCS:texrot 1.0085  0.5042     2 5751.2  0.5085 0.60142  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(model.LI)$coeffcients
```

```
## NULL
```

```r
# certainty and accuracy predicting N2pc
summary(model.lat_CerCor)
```

```
## Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's
##   method [lmerModLmerTest]
## Formula: csLatindex ~ certain2 * correct2 + (1 | subj)
##    Data: data
## 
##      AIC      BIC   logLik deviance df.resid 
##  16355.4  16408.7  -8169.7  16339.4     5759 
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -15.8787  -0.4971   0.0009   0.5101   9.6164 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  subj     (Intercept) 0.006955 0.0834  
##  Residual             0.991750 0.9959  
## Number of obs: 5767, groups:  subj, 18
## 
## Fixed effects:
##                                Estimate Std. Error         df t value Pr(>|t|)
## (Intercept)                   5.741e-03  2.665e-02  2.847e+01   0.215    0.831
## certain2.L                   -4.576e-02  3.223e-02  5.269e+03  -1.420    0.156
## certain2.Q                    4.746e-02  3.071e-02  5.372e+03   1.546    0.122
## correct2incorrect            -2.003e-02  3.763e-02  5.738e+03  -0.532    0.595
## certain2.L:correct2incorrect -4.249e-02  6.825e-02  5.764e+03  -0.623    0.534
## certain2.Q:correct2incorrect -5.515e-02  6.157e-02  5.767e+03  -0.896    0.370
## 
## Correlation of Fixed Effects:
##             (Intr) crt2.L crt2.Q crrct2 c2.L:2
## certain2.L  -0.356                            
## certain2.Q   0.035 -0.349                     
## crrct2ncrrc -0.322  0.239 -0.033              
## crtn2.L:cr2  0.161 -0.459  0.175  0.311       
## crtn2.Q:cr2 -0.024  0.180 -0.486  0.154  0.232
```

```r
anova(model.lat_CerCor,type=3)
```

```
## Type III Analysis of Variance Table with Satterthwaite's method
##                   Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
## certain2          5.0228 2.51141     2 5516.1  2.5323 0.07957 .
## correct2          0.2809 0.28091     1 5738.2  0.2833 0.59460  
## certain2:correct2 0.9763 0.48817     2 5766.8  0.4922 0.61129  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(model.lat_CerCor)$coef
```

```
##                                 Estimate Std. Error       df    t value
## (Intercept)                   0.00574058 0.02664907   28.472  0.2154139
## certain2.L                   -0.04576249 0.03223263 5268.665 -1.4197566
## certain2.Q                    0.04746075 0.03070814 5371.745  1.5455429
## correct2incorrect            -0.02002923 0.03763391 5738.205 -0.5322124
## certain2.L:correct2incorrect -0.04248969 0.06824733 5764.168 -0.6225839
## certain2.Q:correct2incorrect -0.05515148 0.06156799 5766.998 -0.8957817
##                               Pr(>|t|)
## (Intercept)                  0.8309803
## certain2.L                   0.1557377
## certain2.Q                   0.1222739
## correct2incorrect            0.5945995
## certain2.L:correct2incorrect 0.5335826
## certain2.Q:correct2incorrect 0.3704068
```


mediation for cue N2pc 

```r
data$cer<-c(center_scale(data$certain))
library(lmerTest) 
pkg <- "package:lmerTest"
detach(pkg, character.only = TRUE)  # remove else mediation won't work

data$texrot=factor(data$texrot, levels=c("deg0","deg45","deg90"), order=FALSE)
 fit.mediator=lmer(cer ~ edgedisCS*texrot  + (1|subj), data=data,REML = FALSE);
 fit.totaleffect=lmer(csLatindex ~ edgedisCS*texrot     + (1|subj), data=data,REML = FALSE);
 fit.dv=lmer(csLatindex ~ edgedisCS*texrot + cer    + (1|subj), data=data,REML = FALSE);
 results = mediation::mediate(fit.mediator, fit.dv, treat="edgedisCS",   mediator="cer")

 library(lmerTest) # add back for full summaries/ anova info
```

```
## 
## Attaching package: 'lmerTest'
```

```
## The following object is masked from 'package:lme4':
## 
##     lmer
```

```
## The following object is masked from 'package:stats':
## 
##     step
```

```r
 fit.mediator2=lmer(cer ~ edgedisCS*texrot  + (1|subj), data=data,REML = FALSE);
 fit.totaleffect2=lmer(csLatindex ~ edgedisCS*texrot     + (1|subj), data=data,REML = FALSE);
 fit.dv2=lmer(csLatindex ~ edgedisCS*texrot + cer    + (1|subj), data=data,REML = FALSE);
```


```r
# mediation for cue N2pc 
summary(results)
```

```
## 
## Causal Mediation Analysis 
## 
## Quasi-Bayesian Confidence Intervals
## 
## Mediator Groups: subj 
## 
## Outcome Groups: subj 
## 
## Output Based on Overall Averages Across Groups 
## 
##                 Estimate 95% CI Lower 95% CI Upper p-value  
## ACME            0.003368    -0.002407         0.01   0.214  
## ADE             0.022985    -0.004831         0.05   0.102  
## Total Effect    0.026354    -0.000178         0.05   0.054 .
## Prop. Mediated  0.116708    -0.221362         0.96   0.256  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Sample Size Used: 5767 
## 
## 
## Simulations: 1000
```

```r
summary(fit.mediator) #edgedis - path a
```

```
## Linear mixed model fit by maximum likelihood  ['lmerMod']
## Formula: cer ~ edgedisCS * texrot + (1 | subj)
##    Data: data
## 
##      AIC      BIC   logLik deviance df.resid 
##  15409.5  15462.8  -7696.8  15393.5     5759 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.7295 -0.7686  0.2498  0.7769  2.0214 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  subj     (Intercept) 0.1202   0.3467  
##  Residual             0.8347   0.9136  
## Number of obs: 5767, groups:  subj, 18
## 
## Fixed effects:
##                        Estimate Std. Error t value
## (Intercept)            0.004495   0.090752   0.050
## edgedisCS             -0.229520   0.037239  -6.163
## texrotdeg45           -0.002066   0.042186  -0.049
## texrotdeg90           -0.008495   0.046747  -0.182
## edgedisCS:texrotdeg45  0.012892   0.039994   0.322
## edgedisCS:texrotdeg90  0.098257   0.047354   2.075
## 
## Correlation of Fixed Effects:
##             (Intr) edgdCS txrt45 txrt90 eCS:45
## edgedisCS   -0.220                            
## texrotdeg45 -0.406  0.473                     
## texrotdeg90 -0.366  0.427  0.787              
## edgdsCS:t45  0.205 -0.931 -0.425 -0.398       
## edgdsCS:t90  0.173 -0.787 -0.372 -0.355  0.732
```

```r
anova(fit.mediator2)
```

```
## Type III Analysis of Variance Table with Satterthwaite's method
##                   Sum Sq Mean Sq NumDF  DenDF  F value  Pr(>F)    
## edgedisCS        113.407 113.407     1 5749.1 135.8591 < 2e-16 ***
## texrot             0.047   0.023     2 5749.1   0.0281 0.97225    
## edgedisCS:texrot   6.173   3.086     2 5749.2   3.6973 0.02485 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(fit.dv) # certain - path b
```

```
## Linear mixed model fit by maximum likelihood  ['lmerMod']
## Formula: csLatindex ~ edgedisCS * texrot + cer + (1 | subj)
##    Data: data
## 
##      AIC      BIC   logLik deviance df.resid 
##  16355.5  16415.4  -8168.7  16337.5     5758 
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -15.9485  -0.4946   0.0015   0.5103   9.6272 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  subj     (Intercept) 0.006986 0.08358 
##  Residual             0.991406 0.99569 
## Number of obs: 5767, groups:  subj, 18
## 
## Fixed effects:
##                        Estimate Std. Error t value
## (Intercept)            0.020075   0.047269   0.425
## edgedisCS              0.045376   0.040707   1.115
## texrotdeg45           -0.024256   0.045972  -0.528
## texrotdeg90           -0.034296   0.050943  -0.673
## cer                   -0.017269   0.014056  -1.229
## edgedisCS:texrotdeg45 -0.032767   0.043585  -0.752
## edgedisCS:texrotdeg90 -0.003503   0.051618  -0.068
## 
## Correlation of Fixed Effects:
##             (Intr) edgdCS txrt45 txrt90 cer    eCS:45
## edgedisCS   -0.459                                   
## texrotdeg45 -0.848  0.472                            
## texrotdeg90 -0.766  0.426  0.787                     
## cer         -0.001  0.079  0.001  0.003              
## edgdsCS:t45  0.429 -0.928 -0.425 -0.398 -0.004       
## edgdsCS:t90  0.362 -0.786 -0.372 -0.355 -0.026  0.732
```

```r
anova(fit.dv2)
```

```
## Type III Analysis of Variance Table with Satterthwaite's method
##                  Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
## edgedisCS        3.3175  3.3175     1 5759.8  3.3463 0.06741 .
## texrot           0.4494  0.2247     2 5750.1  0.2266 0.79723  
## cer              1.4964  1.4964     1 3739.1  1.5094 0.21931  
## edgedisCS:texrot 1.0579  0.5289     2 5751.8  0.5335 0.58656  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(fit.totaleffect) # edgedis total effect - path c
```

```
## Linear mixed model fit by maximum likelihood  ['lmerMod']
## Formula: csLatindex ~ edgedisCS * texrot + (1 | subj)
##    Data: data
## 
##      AIC      BIC   logLik deviance df.resid 
##  16355.0  16408.2  -8169.5  16339.0     5759 
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -15.9633  -0.4918   0.0005   0.5086   9.6504 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  subj     (Intercept) 0.007156 0.08459 
##  Residual             0.991614 0.99580 
## Number of obs: 5767, groups:  subj, 18
## 
## Fixed effects:
##                        Estimate Std. Error t value
## (Intercept)            0.019973   0.047373   0.422
## edgedisCS              0.049319   0.040585   1.215
## texrotdeg45           -0.024194   0.045977  -0.526
## texrotdeg90           -0.034114   0.050948  -0.670
## edgedisCS:texrotdeg45 -0.032984   0.043589  -0.757
## edgedisCS:texrotdeg90 -0.005167   0.051606  -0.100
## 
## Correlation of Fixed Effects:
##             (Intr) edgdCS txrt45 txrt90 eCS:45
## edgedisCS   -0.460                            
## texrotdeg45 -0.847  0.473                     
## texrotdeg90 -0.764  0.427  0.787              
## edgdsCS:t45  0.428 -0.931 -0.425 -0.398       
## edgdsCS:t90  0.362 -0.787 -0.372 -0.355  0.732
```

```r
anova(fit.totaleffect2)
```

```
## Type III Analysis of Variance Table with Satterthwaite's method
##                  Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)  
## edgedisCS        4.1014  4.1014     1 5749.5  4.1361 0.04202 *
## texrot           0.4446  0.2223     2 5750.2  0.2242 0.79918  
## edgedisCS:texrot 1.0085  0.5042     2 5751.2  0.5085 0.60142  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


Model comparisons - example. We considered models other that our hypothesised EdgeDis*TexRot interaction for completeness. this included - no TexRot in model, no interaction effect. 

```r
#responsetime time using glmm as requested by reviewer. 
#full model
#BEST FIT - used for paper 26/05/2024 -AIC  62949.2 
model.RTRR2=glmer(responsetime ~ cueV+edgedisCS*texrot+  (1|subj), data=dataC,family=inverse.gaussian(link = "identity"));


#AIC - 62953.0 - no TexRot
model.RTRR4=glmer(responsetime ~ cueV+texrot+  (1|subj), data=dataC,family=inverse.gaussian(link = "identity"));

#AIC - 62955.2 - no interaction.
model.RTRR6=glmer(responsetime ~ cueV+edgedisCS+texrot+  (1|subj), data=dataC,family=inverse.gaussian(link = "identity"));

# would not converge- maxmodel
#model.RTRR_full=glmer(responsetime ~ cueV+edgedisCS*texrot+  #(edgedisCS*texrot|subj), data=dataC,family=inverse.gaussian(link =# "identity"));

# AIC  16355.0 - model used
model.LI=lmer(csLatindex ~ edgedisCS*texrot+  (1|subj), data=data, REML = FALSE);

#AIC  16352.0
# exp((6355.0 - 16352.0)/2) =0 
# 0 probablity of imporving model. 
model.LI2=lmer(csLatindex ~ edgedisCS+texrot+  (1|subj), data=data, REML = FALSE);


#AIC  1 16349.0
# exp((6355.0 - 16349.0)/2) =0 
# 0 probablity of imporving model. 
model.LI3=lmer(csLatindex ~ edgedisCS+  (1|subj), data=data, REML = FALSE);

# would not fit. 
#model.LI4=lmer(csLatindex ~ edgedisCS*texrot+  (edgedisCS*texrot|subj), data=data, REML = FALSE);
```




