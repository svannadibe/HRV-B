
library(metafor)

library(forestplot)
library(dplyr)

#data <- read.csv("C:/Users/svannadibe/Desktop/HRV Biofeedback/HRV-B meta-analysis/HRVB Review Meta Analysis - Study Standard Diff.csv")

#data <- read.csv("~/Desktop/Dr Sherry Chan Lab/HRV-B review/HRVB Review Meta Analysis - Study Standard Diff TAU.csv")

data <- read.csv("C:/Users/svannadibe/Desktop/HRV Biofeedback/HRV-B meta-analysis/HRVB Review Meta Analysis - Study Standard Diff TAU.csv")

practice <- read.csv("C:/Users/svannadibe/Desktop/HRV Biofeedback/HRV-B meta-analysis/HRV-B Review Data Extraction - Practice Time TAU.csv")

dat1 <- escalc(measure="SMD", m1i=data$m1i, sd1i=data$sd1i, n1i=data$n1i,
               m2i=data$m2i, sd2i=data$sd2i, n2i=data$n2i, data=data)


PTSD <- dat1[dat1$Outcome == "PTSD",]
PTSD$Tot_Practice <- practice$Total.Practice.Time[practice$Outcome == "PTSD"]

Depression <- dat1[dat1$Outcome == "Depression",]
Depression$Tot_Practice <- practice$Total.Practice.Time[practice$Outcome == "Depression"]

Substance_Abuse <- dat1[dat1$Outcome == "Substance Abuse",]
Substance_Abuse$Tot_Practice <- practice$Total.Practice.Time[practice$Outcome == "Substance Abuse"]

Mindfulness <- dat1[dat1$Outcome == "Mindfulness",]
Mindfulness$Tot_Practice <- practice$Total.Practice.Time[practice$Outcome == "Mindfulness"]

Sleep <- dat1[dat1$Outcome == "Sleep",]
Sleep$Tot_Practice <- practice$Total.Practice.Time[practice$Outcome == "Sleep"]

Anxiety <- dat1[dat1$Outcome == "Anxiety",]
Anxiety$Tot_Practice <- practice$Total.Practice.Time[practice$Outcome == "Anxiety"]

Quality_of_Life <- dat1[dat1$Outcome == "Quality of Life",]
Quality_of_Life$Tot_Practice <- practice$Total.Practice.Time[practice$Outcome == "Quality of Life"]

Stress <- dat1[dat1$Outcome == "Stress",]
Stress$Tot_Practice <- practice$Total.Practice.Time[practice$Outcome == "Stress"]

c <- 0

for (j in Quality_of_Life$yi) {
  c <- c + 1
  Quality_of_Life$yi[c] <- -j
  
}

c <- 0

for (k in Mindfulness$yi) {
  c <- c + 1
  Mindfulness$yi[c] <- -k
  
}

# ind <- which(dat1$Control == "TAU/WL")
# TAU_yi_tot <- dat1$yi[ind]
# TAU_vi_tot <- dat1$vi[ind]
# 
# ind2 <- which(practice$Control == "TAU/WL")
# tot_practice <- practice$Total.Practice.Time[ind2]
# 
# duration <- practice$Study.Duration..Weeks.[ind2]
# 
# 
# plot(tot_practice, TAU_yi_tot)
# cor.test(tot_practice, TAU_yi_tot, method = "pearson")
# cor.test(duration, TAU_yi_tot, method = "pearson")
# 
# shapiro.test(TAU_yi_tot)
# shapiro.test(tot_practice)
# 
# TAU_yi_tot






outcomes <- list(PTSD,Depression,Substance_Abuse,Mindfulness,Sleep,Anxiety,Quality_of_Life,Stress)




for (n in outcomes) {
  
  TAU_data <- n
  TAU_yi <- TAU_data$yi
  TAU_vi <- TAU_data$vi
  TAU_study <- TAU_data$Author
  TAU_Outcome <- TAU_data$Outcome
  X_Axis <- c("Favors Intervention", "Favors Control")
  Exp_n <- n$n1i
  Cont_n <- n$n2i
  left_col <- data.frame(Exp_n, Cont_n)
  
  tot_practice <- TAU_data$Tot_Practice
  

 
  if (length(TAU_yi) >= 3) {
    
    random_effects <- rma(TAU_yi, TAU_vi, data = TAU_data, method = "REML")
    print(TAU_Outcome[1])
    
    print("Random Effects")
    
    print(random_effects)
    
    
    random_effects_mr <- rma(TAU_yi, TAU_vi, mods = ~ tot_practice , data = TAU_data)
    print(TAU_Outcome[1])
    print("Meta-Regression")
    print(random_effects_mr)
    
    
    # forest <- meta::forest(random_effects, xlab = X_Axis, col = "sienna", slab = TAU_study, header = c("Study", "SMD [95% CI]"))
    # print(forest) 
    # print(TAU_Outcome[1])
    
  }
  
}
  

  # 
  # col_vec <- c()
  # pop <- c()
  # count <- 0
  # 
  # for (i in n$Control) {
  #   
  #   cont <- c("TAU/WL"= 1, "J" = 2, "PE"  = 3  , "MM" = 4  ,  "PA"  = 5, "RM" = 6, "Sham" = 7, "PMR" = 8)
  #   
  #   colors <- c("cornsilk3", "rosybrown2","orchid", "lightblue", "olivedrab3", "sienna", "mediumpurple", "royalblue3")
  #   
  #   add <- colors[cont[i]]
  #   
  #   col_vec <- append(col_vec, add, after = length(col_vec)) 
  #   
  # }
    

    

  
 


#write.csv(dat1, "~/Desktop/Dr Sherry Chan Lab/HRV-B review/HRVB Review Meta Analysis - Study Standard Diff.csv", row.names = FALSE)

