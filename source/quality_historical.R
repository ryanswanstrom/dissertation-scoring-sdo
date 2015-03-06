### Load Raw Quality Data
setAs("character","myDate", 
      function(from) {as.Date(from, format="%m/%d/%Y")} )
setClass('myDate')

quality_raw <- read.csv('data/quality_raw.csv', 
                         colClasses=c('factor',
                                      'myDate',
                                      'numeric',
                                      'numeric',
                                      'numeric',
                                      'numeric') )

### Get descriptive statistics
str(quality_raw)
summary(quality_raw)
var(quality_raw)

### Find the Baseline Quality Function
#### Use data prior to 2014
history_quality_raw = 
        quality_raw[quality_raw$MONTH_DT 
                     <= as.Date('2013-12-31') ,]

#### Create some plots of the historical quality data
par(mfrow=c(2,2))
plot(history_quality_raw$DEV_EFF, 
    history_quality_raw$PROD_DFTS, 
     xlab='DEV_EFF', ylab='PROD_DFTS', col='steelblue')
plot(history_quality_raw$SIT_DFTS, 
    history_quality_raw$PROD_DFTS, 
     xlab='SIT_DFTS', ylab='PROD_DFTS', col='steelblue')
plot(history_quality_raw$DEV_EFF, 
    history_quality_raw$PROD_DFTS, 
     xlab='UAT_DFTS', ylab='PROD_DFTS', col='steelblue')


##### Remove the outlier data point with 1216 PROD_DFTS
history_quality_clean = history_quality_raw[history_quality_raw$PROD_DFTS < 1000,]

##### create the model after dropping the
##### data point with over 1000 PROD_DFTS
baseline_quality_function = lm(PROD_DFTS ~ DEV_EFF + SIT_DFTS + UAT_DFTS, 
           data=history_quality_clean )
summary(baseline_quality_function)

par(mfrow=c(1,2))
qqnorm(baseline_quality_function$resid,col='steelblue')
qqline(baseline_quality_function$resid,col='steelblue')
summary(baseline_quality_function)$sigma
plot(baseline_quality_function$fitted,abs(baseline_quality_function$resid),
     col='steelblue',
    main='Fitted vs Resid',
    xlab='Fitted',
    ylab='Absolute Value of Residuals')
pairs(history_quality_clean[ ,c('DEV_EFF','SIT_DFTS','UAT_DFTS')],col='steelblue')

#### source code for ridge regression
#### did not yield better results
install.packages('ridge')
library('ridge')
rd = linearRidge(PROD_DFTS ~ DEV_EFF 
           + SIT_DFTS + UAT_DFTS , 
           data=history_quality_clean, nPCs=1)
summary(rd)