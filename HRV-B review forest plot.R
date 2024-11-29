
library(metafor)

library(dplyr)

library(DT)

library(lme4)

library(ggplot2)

library(shiny)

library(plyr)

#Windows
data <- read.csv("C:/Users/svannadibe/Desktop/HRV Biofeedback/HRV-B meta-analysis/HRV-B Review Data Extraction - Study Standard Diff All Outcomes.csv")

#Windows
practice <- read.csv("C:/Users/svannadibe/Desktop/HRV Biofeedback/HRV-B meta-analysis/HRV-B Review Data Extraction - VariablesTAU.csv")

#Windows
HRV_data <- read.csv("C:/Users/svannadibe/Desktop/HRV Biofeedback/HRV-B meta-analysis/HRVB Review Meta Analysis - HRV Data.csv")

#Mac
#practice <- read.csv("~/Desktop/Dr Sherry Chan Lab/HRV-B review/HRV-B/HRV-B Review Data Extraction - VariablesTAU.csv")

#HRV_data <- read.csv("~/Desktop/Dr Sherry Chan Lab/HRV-B review/HRV-B/HRVB Review Meta Analysis - HRV Data.csv")

#HRV_var <- read.csv("~/Desktop/Dr Sherry Chan Lab/HRV-B review/HRV-B/HRVB Review Meta Analysis - HRV Variables.csv")


################################################################################

#calculate SMD for outcomes
dat1 <- escalc(measure="SMD", m1i=data$m1i, sd1i=data$sd1i, n1i=data$n1i,
               m2i=data$m2i, sd2i=data$sd2i, n2i=data$n2i, data=data)

outcome_names <- c("PTSD","Depression","Substance Abuse", "Mindfulness","Sleep", "Anxiety" , "Quality of Life", "Stress")
outcome <- c("PTSD","Depression","Substance_Abuse", "Mindfulness","Sleep", "Anxiety" , "Quality_of_Life", "Stress") 

#change sign of QoL and Mindfulness SMD as positive SMD means improvement
dat1$yi[dat1$Outcome == "Quality of Life"] <- -(dat1$yi[dat1$Outcome == "Quality of Life"])
dat1$yi[dat1$Outcome == "Mindfulness"] <- -(dat1$yi[dat1$Outcome == "Mindfulness"])

optimization_duration <- data.frame()
optimization_practice_time <- data.frame()

c <- 0 #counter

#divide data set by outcome
for (n in outcome_names) {

  # add columns for diff influencing variables and assign to data frame for each outcome
  c <- c + 1
  outcome_data <- dat1[dat1$Outcome == n,]
  outcome_data$Tot_Practice <- practice$Total.Practice.Time[practice$Outcome == n]
  outcome_data$Duration <- practice$Study.Duration..Weeks.[practice$Outcome == n]
  outcome_data$Visits <- practice$Number.Lab.Visits[practice$Outcome == n]
  outcome_data$MinperDay <- practice$Min.Per.Day[practice$Outcome == n]
  outcome_data$Age <- practice$Average.Age.Tot[practice$Outcome == n]
  outcome_data$Gender <- practice$Percent.Female[practice$Outcome == n]
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

  
  if (length(TAU_yi) > 3) {
    
    #RE for each outcome
    random_effects <- rma(TAU_yi, TAU_vi, data = TAU_data, method = "REML")
    
    #forest plot with RE for each outcome
    forest <- forest(random_effects, xlab = X_Axis, col = "sienna", slab = TAU_study, header = c(TAU_Outcome[1], "SMD [95% CI]"), data = TAU_data)
    print(forest)
    
    ###########################################################################
    
    #Meta Regression with different factors
    
    #influencing factors as variables
    duration <- TAU_data$Duration
    tot_practice <- TAU_data$Tot_Practice
    visits <- TAU_data$Visits
    age <- TAU_data$Age
    gender <- TAU_data$Gender
    min_per_day <- TAU_data$MinperDay
    
    #initialize an empty data frame to add rows to
    MR_df <- data.frame()
    
    #create dataframe with different factors
    factor <- data.frame(duration, tot_practice, visits, min_per_day, age, gender)
    
    
    factor_name <- c("Study Duration", "Total Practice Time", "Number of Lab Visits", "Practice Time Per Day", "Age of Participants", "Gender(% Female)")
    
    for (j in seq(1, ncol(factor), 1)) {
      #meta-regression with influencing factors across outcomes
      MR <- rma(TAU_yi, TAU_vi, mods = factor[,j], method = "REML", digits = 3
                , data = TAU_data)
      
      new_row <- data.frame(factor_name[j], round(MR$b[1],3), round(MR$se[1],3), round(MR$pval[1],3), round(MR$ci.lb[1],3), round(MR$ci.ub[1],3))
      
      # data frame with p-val, other important variables
      MR_df <- rbind(MR_df, new_row)
      
    }
    
    #make MR dataframe pretty
    outcome_factor <- paste(TAU_Outcome[1], "Factor")
    colnames(MR_df) <- c(outcome_factor, "Intercept", "SE", "p-val", "CI upper", "CI lower")
    
    sig_index <- which(MR_df$`p-val`> 0.01 & MR_df$`p-val`< 0.05)
    very_sig_index <- which(MR_df$`p-val`< 0.01)
    
    MR_df_image <- datatable(MR_df) 
    
    print(MR_df_image)
    
    ##############################################################################
    
    #categorical meta-regression and optimization
    
    diagnosis <- TAU_data$Diagnosis
    sample_size <- plyr::count(diagnosis)
    diagnosis_sample_size <- sample_size$freq
    
    if (length(diagnosis_sample_size) < length(TAU_yi)) {
      
      #categorical meta-regression (diagnosis)
      diagnosis_meta_reg <- rma(TAU_yi, TAU_vi, mods = ~diagnosis - 1,
                                method = "REML", digits = 3, data = TAU_data)
      
    }
    
    #################################################################################
    
    #check levels of grouping factor are less than number of observations
    
    #https://stackoverflow.com/questions/40027690/finding-model-predictor-values-that-maximize-the-outcome
    
    if (length(unique(duration)) < length(TAU_study) | length(unique(tot_practice)) < length(TAU_study)) {
      
      print(length(unique(duration)))
      print(length(TAU_study))
      
      #maximize function to find optimum study duration
      MR_factor <- lmer(formula = TAU_yi ~ 1 + duration + (1 | TAU_study), data = TAU_data)
      
      predval <- function(x) {
        newdata <- data.frame(duration = x[1], tot_practice = x[2], visits = x[3], 
                              min_per_day = x[4], av_age = x[5], per_fem=x[6])
        return(predict(MR_factor, newdata = newdata, re.form=~0))
      }
      
      #optimize based on study duration
      opt1 <- optimize(f = predval, interval = TAU_yi, lower= min(duration), upper = max(duration),
                       maximum = TRUE)
      
      max <- c(TAU_Outcome[1] ,opt1$maximum[1])
      optimization_duration <- rbind(optimization_duration, max)
      
      
      #maximize function to find optimum total practice time
      MR_factor <- lmer(formula = TAU_yi ~ 1 + tot_practice +(1 | TAU_study), data = TAU_data)
      
      
      predval <- function(x) {
        newdata <- data.frame(duration = x[1], tot_practice = x[2], visits = x[3], 
                              min_per_day = x[4])
        return(predict(MR_factor, newdata = newdata, re.form=~0))
      }
      
      #optimize based on total practice time
      opt1 <- optimize(f = predval, interval = dat1$yi, lower= min(tot_practice), upper = max(tot_practice),
                       maximum = TRUE)
      max <- c(TAU_Outcome[1] ,opt1$maximum[1])
      optimization_practice_time <- rbind(optimization_practice_time, max)
      
    }
    
  }
    
    
  }
  

#plot SMD and factor
#plot <- ggplot(dat1, aes(x = practice_time, y = yi)) + geom_point()


#################################################################################

 # #summary(diagnosis_meta_reg)
 # 
 # #get sample size of each diagnosis

 # diagnosis_pval <- round(diagnosis_meta_reg$pval, 3)
 # 
 # #forest plot with effect of diagnosis on all outcomes
 # forest_diagnosis <- forest(diagnosis_meta_reg$b, ci.lb = diagnosis_meta_reg$ci.lb, annotate = TRUE, 
 #                            ci.ub = diagnosis_meta_reg$ci.ub, header = "Diagnosis",
 #                            xlab = c("Favors Intervention", "Effect Size", "Favors Control"), slab = sample_size$x, ilab = data.frame(diagnosis_sample_size, diagnosis_pval), 
 #                            ilab.lab = c("Studies", "p-value")) 
 # 
 # print(forest_diagnosis)
 
 
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

# Meta Regression with HRV + Influencing Factors
HRV_age <- HRV_var$Av.Age
HRV_Gender <- HRV_var$Percent.Female
HRV_duration <- HRV_var$Study.Duration
HRV_practice <- HRV_var$Total.Practice.Time

factor_HRV <- data.frame(HRV_age, HRV_Gender, HRV_duration, HRV_practice)
factor_HRV_name <- c("Age", "Gender(%Female)", "Duration", "Total Practice Time")
MR_HRV_df <- data.frame()

for (k in seq(1, ncol(factor_HRV), 1)) {
  
  MR_HRV_Factor <- rma(dat2$yi, dat2$vi, mods = factor_HRV[,k], method = "REML", digits = 3
                       , data = dat2)
  summary(MR_HRV_Factor)
  
  new_row_HRV <- data.frame(factor_HRV_name[k], round(MR_HRV_Factor$b[1],3), round(MR_HRV_Factor$se[1],3), round(MR_HRV_Factor$pval[1],3), 
                            round(MR_HRV_Factor$ci.lb[1],3), round(MR_HRV_Factor$ci.ub[1],3))
  
  # data frame with p-val, other important variables
  MR_HRV_df <- rbind(MR_HRV_df, new_row_HRV)
  
}

#make dataframe pretty
colnames(MR_HRV_df) <- c("Factor", "Intercept", "SE", "p-val", "CI upper", "CI lower")

MR_df_image_HRV <- datatable(MR_HRV_df) 

print(MR_df_image_HRV)



################################################################################




#write.csv(dat1, "C:/Users/svannadibe/Desktop/HRV Biofeedback/HRV-B meta-analysis/HRV-B Review Data Extraction - Study Standard Diff All Outcomes.csv")

