
library(metafor)

library(dplyr)

library(DT)

library(lme4)

library(ggplot2)

library(shiny)

library(plyr)


#Mac
#data <- read.csv("~/Desktop/Dr Sherry Chan Lab/HRV-B review/HRV-B/HRVB Review Meta Analysis - Study Standard Diff TAU.csv")

#Windows
data <- read.csv("C:/Users/svannadibe/Desktop/HRV Biofeedback/HRV-B meta-analysis/HRV-B Review Data Extraction - Study Standard Diff All Outcomes.csv")

#Windows
practice <- read.csv("C:/Users/svannadibe/Desktop/HRV Biofeedback/HRV-B meta-analysis/HRV-B Review Data Extraction - VariablesTAU.csv")

#Mac
#practice <- read.csv("~/Desktop/Dr Sherry Chan Lab/HRV-B review/HRV-B/HRV-B Review Data Extraction - Practice Time TAU.csv")

#Desktop
HRV_data <- read.csv("C:/Users/svannadibe/Desktop/HRV Biofeedback/HRV-B meta-analysis/HRVB Review Meta Analysis - HRV Data.csv")


################################################################################

#calculate SMD for outcomes
dat1 <- escalc(measure="SMD", m1i=data$m1i, sd1i=data$sd1i, n1i=data$n1i,
               m2i=data$m2i, sd2i=data$sd2i, n2i=data$n2i, data=data)

outcome_names <- c("PTSD","Depression","Substance Abuse", "Mindfulness","Sleep", "Anxiety" , "Quality of Life", "Stress")
outcome <- c("PTSD","Depression","Substance_Abuse", "Mindfulness","Sleep", "Anxiety" , "Quality_of_Life", "Stress") 

#change sign of QoL and Mindfulness SMD as positive SMD means improvement
dat1$yi[dat1$Outcome == "Quality of Life"] <- -(dat1$yi[dat1$Outcome == "Quality of Life"])
dat1$yi[dat1$Outcome == "Mindfulness"] <- -(dat1$yi[dat1$Outcome == "Mindfulness"])

c <- 0 #counter

#divide data set by outcome
for (n in outcome_names) {

  c <- c + 1
  outcome_data <- dat1[dat1$Outcome == n,]
  outcome_data$Tot_Practice <- practice$Total.Practice.Time[practice$Outcome == n]
  outcome_data$Duration <- practice$Study.Duration..Weeks.[practice$Outcome == n]
  outcome_data$Visits <- practice$Number.Lab.Visits[practice$Outcome == n]
  name <- outcome[c]
  assign(name, outcome_data)
  
}

c <- 0

outcomes <- list(PTSD,Depression,Substance_Abuse, Mindfulness,Sleep, Anxiety, Quality_of_Life, Stress)

#iterate through outcomes to produce forest plot for each outcome
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
  
  
  if (length(TAU_yi) > 2) {
    
    #RE for each outcome
    random_effects <- rma(TAU_yi, TAU_vi, data = TAU_data, method = "REML")
    
    #forest plot with RE for each outcome
    forest <- forest(random_effects, xlab = X_Axis, col = "sienna", slab = TAU_study, header = c(TAU_Outcome[1], "SMD [95% CI]"), data = TAU_data)
    print(forest)
    
  }
  
}

################################################################################

#influencing factors as variables
yi <- dat1$yi
duration <- practice$Study.Duration..Weeks.
tot_practice <- practice$Total.Practice.Time
visits <- as.integer(practice$Number.Lab.Visits)
min_per_day <- practice$Min.Per.Day
av_age <- practice$Average.Age.Tot
per_fem <- practice$Percent.Female
  
#meta-regression with influencing factors across outcomes
MR_df <- data.frame()

factor <- data.frame(duration, tot_practice, visits, min_per_day, av_age, per_fem)
summary(factor)

factor_name <- c("Study Duration", "Total Practice Time", "Number of Lab Visits", "Practice Time Per Day", "Age of Participants", "Gender(% Female)")

for (j in seq(1, ncol(factor), 1)) {
  
  MR <- rma(dat1$yi, dat1$vi, mods = factor[,j], method = "REML", digits = 3
            , data = dat1)
  
  new_row <- data.frame(factor_name[j], round(MR$b[1],3), round(MR$se[1],3), round(MR$pval[1],3), round(MR$ci.lb[1],3), round(MR$ci.ub[1],3))
  
  # data frame with p-val, other important variables
  MR_df <- rbind(MR_df, new_row)

}

#make MR dataframe pretty
colnames(MR_df) <- c("Factor", "Intercept", "SE", "p-val", "CI upper", "CI lower")

sig_index <- which(MR_df$`p-val`> 0.01 & MR_df$`p-val`< 0.05)
very_sig_index <- which(MR_df$`p-val`< 0.01)

MR_df_image <- datatable(MR_df) 

print(MR_df_image)



#plot SMD and factor
plot <- ggplot(dat1, aes(x = practice_time, y = yi)) + geom_point()
print(plot)


#################################################################################

# categorical meta-regression (diagnosis)
 diagnosis_meta_reg <- rma(dat1$yi, dat1$vi, mods = ~dat1$Diagnosis - 1,
                               method = "REML", digits = 3, data = dat1)
 
 #summary(diagnosis_meta_reg)
 
 #get sample size of each diagnosis
 sample_size <- plyr::count(dat1$Diagnosis)
 diagnosis_sample_size <- sample_size$freq
 diagnosis_pval <- round(diagnosis_meta_reg$pval, 3)

 #forest plot with effect of diagnosis on all outcomes
 forest_diagnosis <- forest(diagnosis_meta_reg$b, ci.lb = diagnosis_meta_reg$ci.lb, annotate = TRUE, 
                            ci.ub = diagnosis_meta_reg$ci.ub, header = "Diagnosis",
                            xlab = c("Favors Intervention", "Effect Size", "Favors Control"), slab = sample_size$x, ilab = data.frame(diagnosis_sample_size, diagnosis_pval), 
                            ilab.lab = c("Studies", "p-value")) 
 
 print(forest_diagnosis)
 
 
 ###############################################################################
 
 #HRV calculations
 
 #calculate SMD for HRV
 dat2 <- escalc(measure="SMD", m1i=HRV_data$M1, sd1i=HRV_data$SD1, n1i=HRV_data$N1,
                m2i=HRV_data$M2, sd2i=HRV_data$SD2, n2i=HRV_data$N2, data=HRV_data)
 
 #random effects for HRV
 RMA_HRV <- rma(dat2$yi, dat2$vi, data = dat2, method = "REML")
 
 #summary(RMA_HRV)
 
 #forest plot with HRV RMA
 forest_HRV <- forest(RMA_HRV, slab = dat2$Author, xlab = c("Favors Intervention", "Effect Size", "Favors Control"), col = "sienna", 
                      header = c("Study", "SMD [95% CI]"), digits = 2)
  
print(forest_HRV)

#################################################################################

#maximize function to find optimum study duration
MR_factor <- lmer(formula = dat1$yi ~ 1 + duration +(1 | dat1$Author), data = dat1)


predval <- function(x) {
  newdata <- data.frame(duration = x[1], tot_practice = x[2], visits = x[3], 
                        min_per_day = x[4], av_age = x[5], per_fem=x[6])
  return(predict(MR_factor, newdata = newdata, re.form=~0))
}

opt1 <- optimize(f = predval, interval = dat1$yi, lower= min(duration), upper = max(duration),
                 maximum = TRUE)

max <- paste(opt1, "Optimum Study Duration")
print(max)

#maximize function to find optimum total practice time
MR_factor <- lmer(formula = dat1$yi ~ 1 + tot_practice +(1 | dat1$Author), data = dat1)


predval <- function(x) {
  newdata <- data.frame(duration = x[1], tot_practice = x[2], visits = x[3], 
                        min_per_day = x[4])
  return(predict(MR_factor, newdata = newdata, re.form=~0))
}

opt1 <- optimize(f = predval, interval = dat1$yi, lower= min(tot_practice), upper = max(tot_practice),
                 maximum = TRUE)
max <- paste(opt1, "Optimum Total Practice Time")
print(max)

#write.csv(dat1, "C:/Users/svannadibe/Desktop/HRV Biofeedback/HRV-B meta-analysis/HRV-B Review Data Extraction - Study Standard Diff All Outcomes.csv")

