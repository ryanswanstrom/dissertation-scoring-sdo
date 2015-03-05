library('sense')
source('barchart.R')

### Now read in the requirements data
requirements_raw = read.csv('data/requirements_raw.csv',
                              colClasses=c('factor',
                                      'Date',
                                      'numeric',
                                      'numeric') )

##### remove rows where COMPLETED and SCHEDULED are both 0
requirements_clean = requirements_raw[requirements_raw$SCHEDULED != 0, ]

requirements_clean$DIFF = requirements_clean$COMPLETED/requirements_clean$SCHEDULED
summary(requirements_clean)
str(requirements_clean)
var(requirements_clean$SCHEDULED)
var(requirements_clean$COMPLETED)
var(requirements_clean$DIFF)

hist(requirements_clean[requirements_clean$DIFF != 1,]$DIFF)

##### Now calculate the requirements CRI score
##### but only for year 2014 and 2015
current_requirements = requirements_clean[requirements_clean$MONTH_DT 
                     > as.Date('2013-12-31'), ]
current_requirements$MONTH_DT = as.yearmon(as.Date(current_requirements$MONTH_DT), "%Y-%B")
current_requirements$SCORE = 100*(current_requirements$COMPLETED-current_requirements$SCHEDULED)/current_requirements$SCHEDULED

req_scores = aggregate(SCORE ~ MONTH_DT, current_requirements, mean )
req_scores$SCORE = round(req_scores$SCORE, 2)
req_scores

barchart(req_scores, main='CRI Requirements Scores', ylab='CRI Requirements Score',xlab='Month/Year')




