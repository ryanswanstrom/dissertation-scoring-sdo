
### Combine the scores
# overall

# overall

full = merge(x = qual_scores, y = avail_scores, by = "MONTH_DT", all = TRUE, suffixes=c('_QUAL','_AVAIL'))
full = merge(x = full, y = sched_scores, by = "MONTH_DT", all = TRUE, suffixes='_SCHED' )
colnames(full)[4] = 'SCORE_SCHED'
full = merge(x = full, y = req_scores, by = "MONTH_DT", all = TRUE, suffixes=c('','_REQ') )
colnames(full)[5] = 'SCORE_REQ'

full$SCORE = apply(full[,seq(2,5)], 1, 
                   function(x) {sum(x,na.rm=TRUE)/sum(!is.na(x))} )

barchart(full)