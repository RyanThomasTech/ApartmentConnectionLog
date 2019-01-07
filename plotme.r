plotMe <- function(){
  txt <- readLines("connlog.txt")
  txt <- txt[3:3770]
  timeout <- "Request timed out."
  reply <- "Reply"
  timeout_count <- length(which(txt %in% timeout))
  txt <- txt[(which(!(txt %in% timeout)))]
  txt <- sapply(txt, function(x) {sub(".*time\\W","",x)})
  txt <- sapply(txt, function(x) {sub("(ms).*","",x)})
  txt <- sapply(txt, function(x) {sub("^R.*","Reply",x)})
  txt <- txt[(which(!(txt %in% reply)))]
  txt <- as.numeric(txt)
  data <- data.frame("Under50ms" = sum(txt<50),"50to200ms" = sum((txt>50)&(txt<200)),"above200ms" = sum(txt>200),"disconnected" = timeout_count)
  
  par(mfrow = c(1,2))
  barplot(txt,xlab = "Time(approximately 1 hour)",ylab = "Response Time(ms)", main = "Raw Capture Data w/o Total Packet Loss")
  txtadjusted <- txt[which(txt<700)]
  barplot(txtadjusted,xlab = "Time(approximately 1 hour)",ylab = "Response Time(ms)", main = "Capture Data Adjusted for Visibility", col = "gray")
  abline(h = 60, col="blue", lty=5, lwd=2)
  abline(h = 87, col = "red", lty = 2, lwd = 2)
  legend(x="topright", c("Typical connection response time(60ms)","Average recorded response time(87ms)"), lty =c(1,1), col=c("blue", "red"), bty="n")
  dev.copy(png, "RecordedDataPlots.png", height = 480, width = 960)
  dev.off()
}