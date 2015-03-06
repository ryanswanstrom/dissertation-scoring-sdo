### Now calculate the Quality scores
#### Get data for 2014 and newer
current_quality = 
        quality_raw[quality_raw$MONTH_DT 
                     > as.Date('2013-12-31') ,]

current_quality$PREDICTION = 
        predict(baseline_quality_function, newdata=current_quality)
current_quality$SCORE = 100 * 
        ifelse(current_quality$PREDICTION 
               >= current_quality$PROD_DFTS,
                  (current_quality$PREDICTION 
                    - current_quality$PROD_DFTS)
                    /current_quality$PREDICTION,
                  (current_quality$PREDICTION 
                    - current_quality$PROD_DFTS)
                    /(summary(baseline_quality_function)$sigma^2)
                                     )

qual_scores = aggregate(SCORE ~ MONTH_DT, current_quality, mean )
qual_scores$MONTH_DT = as.yearmon(qual_scores$MONTH_DT, "%Y-%B")
qual_scores$SCORE = round(qual_scores$SCORE, 2)
qual_scores

barchart(qual_scores, main='CRI Quality Scores', ylab='CRI Quality Score',xlab='Month/Year')
