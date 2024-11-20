
standard_mean_diff <- function() {
  
  library(metafor)
  
  data <- read.csv("~/Desktop/Dr Sherry Chan Lab/HRV-B review/HRVB Review Meta Analysis - Study Standard Diff.csv")
  
  dat1 <- escalc(measure="SMD", m1i=data$m1i, sd1i=data$sd1i, n1i=data$n1i,
                 m2i=data$m2i, sd2i=data$sd2i, n2i=data$n2i, data=data)
  
  low_CI <- (dat1$yi - (dat1$vi)/2)
  high_CI <- (dat1$yi + (dat1$vi)/2)
  
}

PTSD <- dat1[dat1$Outcome == "PTSD",]

authors <- dat1$Author
n1 <- dat1$n1i
n2 <- dat1$n2i
smd <- round(dat1$yi, digits = 2)

label <- list(authors,n1,n2,smd)

forestplot(mean = dat1$yi, lower = low_CI, upper = high_CI, labeltext = label, boxsize = 0.2, lineheight = unit(5,"mm"))
           
write.csv(dat1, "~/Desktop/Dr Sherry Chan Lab/HRV-B review/HRVB Review Meta Analysis - Study Standard Diff.csv", row.names = FALSE)

 


