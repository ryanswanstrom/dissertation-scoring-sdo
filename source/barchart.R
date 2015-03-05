library('ggplot2')
install.packages('zoo')
library('zoo')
library('scales')

barchart <- function(data, main='CRI Scores', xlab='Month/Year', ylab='CRI Score') {
    
    ggplot(data, aes(x=as.Date(MONTH_DT), y=SCORE, fill=SCORE)) + 
      geom_bar(stat = "identity" ) +
      geom_text(aes(label=round(SCORE, 2), vjust=ifelse(sign(SCORE)>=0, -.4, 1.4)), size=9 ) + 
      guides(fill=FALSE) +
      ylim(-100,100) +
      scale_x_date(labels = date_format("%m/%y"), breaks = date_breaks("month")) +
      ggtitle(main) +
      ylab(ylab) +
      xlab(xlab)
}