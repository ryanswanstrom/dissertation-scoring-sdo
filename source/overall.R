
### Combine the scores
# overall

full = merge(x = qual_scores, y = avail_scores, by = "MONTH_DT", all = TRUE)
full = merge(x = full, y = req_scores, by = "MONTH_DT", all = TRUE)
full = merge(x = full, y = sched_scores, by = "MONTH_DT", all = TRUE, suffixes='sched')

full$SCORE = apply(full[,seq(2,5)], 1, 
                   function(x) {sum(x,na.rm=TRUE)/sum(!is.na(x))} )

barchart(full)
