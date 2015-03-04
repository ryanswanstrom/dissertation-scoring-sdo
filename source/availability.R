library('sense')
source('barchart.R')

# Load Data
# ---------
#
# Load the Availability data.
setAs("character","myDate", 
      function(from) {as.Date(from, format="%m/%d/%Y")} )
setClass('myDate')

avail_raw <- read.csv('data/availability_raw.csv',
                      colClasses=c('factor',
                                    'myDate',
                                    'numeric',
                                    'numeric'))

summary(avail_raw)
str(avail_raw)

# trim to only 2014 and newer
avail_data = 
        avail_data[avail_data$SLA_DATE >= as.Date('2014-01-01') ,]

avail_data$SCORE =  ifelse(
  avail_data$ACTUAL <= avail_data$EXPECTED,
  (avail_data$ACTUAL - avail_data$EXPECTED)/avail_data$EXPECTED,
  (avail_data$ACTUAL - avail_data$EXPECTED)/(1 - avail_data$EXPECTED)
  )       
 
avail_scores = aggregate(SCORE ~ SLA_DATE, avail_data, mean )
avail_scores$SLA_DATE = as.yearmon(avail_scores$SLA_DATE, "%Y-%B")
avail_scores$SCORE = round(100* avail_scores$SCORE, 2)

barchart(avail_scores, main='CRI Availability Scores',
         ylab='CRI Quality Score',xlab='Month/Year')