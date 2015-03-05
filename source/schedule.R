source('barchart.R')

## Load the Schedule data.
setAs("character","myDate", function(from) {as.Date(from, format="%m/%d/%Y")} )
setClass('myDate')

schedule_raw <- read.csv('data/schedule_raw.csv',
                      colClasses=c('factor',
                                   'myDate',
                                   'myDate',
                                   'myDate',
                                   'myDate',
                                   'myDate'))
summary(schedule_raw)
str(schedule_raw)

#### Clean up data by removing all rows with 
#### schedule start, schedule end, and actual end
clean_sched_data = schedule_raw[ !is.na(schedule_raw$ACTUAL_FINISH) & !is.na(schedule_raw$SCHED_START) & !is.na(schedule_raw$SCHED_FINISH),]

#### Find the estimated duration, 
#### and the delta from the estimated finish 
#### and percent
clean_sched_data$EST_DUR = as.numeric(clean_sched_data$SCHED_FINISH - clean_sched_data$SCHED_START+1)
clean_sched_data$DELTA = as.numeric(clean_sched_data$SCHED_FINISH - clean_sched_data$ACTUAL_FINISH )
clean_sched_data$PERCENT_DELTA = clean_sched_data$DELTA/clean_sched_data$EST_DUR

#### Remove some of the outlier data
clean_sched_data = clean_sched_data[abs(clean_sched_data$PERCENT_DELTA) < 20 , ]
summary(clean_sched_data)
str(clean_sched_data)

### Fit a Distribution to the data

location = median(clean_sched_data$PERCENT_DELTA)
scale = IQR(clean_sched_data$PERCENT_DELTA)/2
c(location, scale)
h = hist(clean_sched_data$PERCENT_DELTA, breaks = 70, prob=TRUE, ylim=c(0,6), main='Histogram of Schedule Data')
x=seq(-5,5,length=200)
curve(dcauchy(x, location, scale ), 
          col="darkblue", lwd=2, add=TRUE)

### Now find the CRI scores
sched_data = clean_sched_data
sched_data$SCORE = (200/pi)*atan(sched_data$PERCENT_DELTA /scale)
sched_data$MONTH_DT = as.yearmon(sched_data$ACTUAL_FINISH, "%Y-%B")
sched_scores = aggregate(SCORE ~ MONTH_DT, sched_data, mean )
sched_scores

barchart(sched_scores, main='CRI Schedule Scores', ylab='CRI Schedule Score')


