
library(metafor)

library(forestplot)
library(dplyr)

#data <- read.csv("C:/Users/svannadibe/Desktop/HRV Biofeedback/HRV-B meta-analysis/HRVB Review Meta Analysis - Study Standard Diff.csv")

data <- read.csv("~/Desktop/Dr Sherry Chan Lab/HRV-B review/HRVB Review Meta Analysis - Study Standard Diff.csv")

dat1 <- escalc(measure="SMD", m1i=data$m1i, sd1i=data$sd1i, n1i=data$n1i,
               m2i=data$m2i, sd2i=data$sd2i, n2i=data$n2i, data=data)


PTSD <- dat1[dat1$Outcome == "PTSD",]
Depression <- dat1[dat1$Outcome == "Depression",]
Substance_Abuse <- dat1[dat1$Outcome == "Substance Abuse",]
Mindfulness <- dat1[dat1$Outcome == "Mindfulness",]
Sleep <- dat1[dat1$Outcome == "Sleep",]
Anxiety <- dat1[dat1$Outcome == "Anxiety",]
Quality_of_Life <- dat1[dat1$Outcome == "Quality of Life",]
Stress <- dat1[dat1$Outcome == "Stress",]

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

outcomes <- list(PTSD,Depression,Substance_Abuse,Mindfulness,Sleep,Anxiety,Quality_of_Life,Stress)

# prepLabelText <- function(labeltext, nr) {
#   # Get the number of columns (nc) and number of rows (nr)
#   # if any columns are to be spacers the widthcolumn variable
#   if (is.expression(labeltext)) {
#     widthcolumn <- c(TRUE)
#     # Can't figure out multiple levels of expressions
#     nc <- 1
#     label_nr <- length(labeltext)
#     # Names are retained
#     labeltext <- as.list(labeltext)
#   } else if (is.data.frame(labeltext)) {
#     # If labeltext is a data frame, handle it differently than a generic list
#     widthcolumn <- !apply(is.na(labeltext), 1, any)
#     nc <- ncol(labeltext)
#     label_nr <- nrow(labeltext)
#     cn <- colnames(labeltext)
#     labeltext <- lapply(seq(nc), function(i) as.list(labeltext[[i]]))
#     names(labeltext) <- cn
#   } else if (is.list(labeltext)) {
#     if (isValidLabelList(labeltext)) {
#       labeltext <- list(labeltext)
#     }
#     labeltext <- sapply(labeltext,
#                         function(x) {
#                           if (is.list(x)) {
#                             return(x)
#                           }
#                           
#                           return(as.list(x))
#                         },
#                         simplify = FALSE,
#                         USE.NAMES = TRUE
#     )
#     
#     if (!prFpValidateLabelList(labeltext)) {
#       stop("Invalid labellist, it has to be formed as a matrix m x n elements")
#     }
#     
#     # Can't figure out multiple levels of expressions
#     nc <- length(labeltext)
#     
#     widthcolumn <- c()
#     # Should mark the columns that don't contain
#     # expressions, text or numbers as width columns
#     for (col.no in seq(along = labeltext)) {
#       empty_row <- TRUE
#       for (row.no in seq(along = labeltext[[col.no]])) {
#         if (is.expression(labeltext[[col.no]][[row.no]]) ||
#             !is.na(labeltext[[col.no]][[row.no]])) {
#           empty_row <- FALSE
#           break
#         }
#       }
#       widthcolumn <- append(widthcolumn, empty_row)
#     }
#     
#     label_nr <- length(labeltext[[1]])
#   } else if (is.vector(labeltext)) {
#     widthcolumn <- c(FALSE)
#     nc <- 1
#     label_nr <- length(labeltext)
#     
#     labeltext <- list(as.list(labeltext))
#   } else {
#     # Original code for matrixes
#     widthcolumn <- !apply(is.na(labeltext), 1, any)
#     nc <- NCOL(labeltext)
#     label_nr <- NROW(labeltext)
#     label_colnames <- colnames(labeltext)
#     labeltext <- (\(x) lapply(
#       seq(NCOL(labeltext)),
#       function(i) as.list(x[, i])
#     ))(labeltext)
#     names(labeltext) <- label_colnames
#   }
#   
#   if (nr != label_nr) {
#     stop(
#       "You have provided ", nr, " rows in your",
#       " mean arguement while the labels have ", label_nr, " rows"
#     )
#   }
#   
#   structure(labeltext,
#             no_cols = nc,
#             no_rows = label_nr,
#             widthcolumn = widthcolumn,
#             class = "forestplot_labeltext"
#   )
# }
# 
# # Helper function to validate if all elements of a list are atomic (not lists) and of length 1
# isValidLabelList <- function(listData) {
#   sapply(listData, function(x) length(x) == 1 && !is.list(x)) |> all()
#   
# }
# 
# prepAlign <- function(align, graph.pos, nc) {
#   # Prepare the summary and align variables
#   if (is.null(align)) {
#     if (graph.pos == 1) {
#       return(rep("l", nc))
#     }
#     
#     if (graph.pos == nc + 1) {
#       return(c("l", rep("r", nc - 1)))
#     }
#     
#     return(c("l", rep("c", nc - 1)))
#   }
#   
#   if (length(align) == 1 &&
#       is.character(align) &&
#       nchar(align) > 1) {
#     align <- strsplit(align, split = "")[[1]]
#   }
#   
#   if (any(!align %in% c("l", "c", "r"))) {
#     stop("The align argument must only contain 'l', 'c', or 'r'. You provided: ", align)
#   }
#   rep(align, length.out = nc)
# }
# 
# prepGraphPositions <- function(graph.pos, nc) {
#   if (is.character(graph.pos)) {
#     return(switch(graph.pos,
#                   right = nc + 1L,
#                   last = nc + 1L,
#                   left = 1L,
#                   first = 1L,
#                   stop(
#                     "The graph.pos argument has an invalid text argument.",
#                     " The only values accepted are 'left'/'right' or 'first'/'last'.",
#                     " You have provided the value '", graph.pos, "'")))
#   }
#   
#   if (is.numeric(graph.pos)) {
#     if (!graph.pos %in% 1:(nc + 1)) {
#       stop(
#         "The graph position must be between 1 and ", (nc + 1), ".",
#         " You have provided the value '", graph.pos, "'."
#       )
#     }
#     return(graph.pos)
#   }
#   
#   stop("The graph pos must either be a string consisting of 'left'/'right' (alt. 'first'/'last')",
#        ", or an integer value between 1 and ", (nc + 1))
# }


for (n in outcomes) {
  
  #plot.new()
  
  authors <- n$Author
  n1 <- n$n1i
  n2 <- n$n2i
  smd <- round(n$yi, digits = 2)
  control <- n$Control.1
  low_CI <- (n$yi - (n$vi*2))
  upper_CI <- (n$yi + (n$vi*2))
  
  m <- mean(n$yi)
  m_sd <- mean(n$vi)
  m_lower <- (m - 2*m_sd)
  m_upper <- (m + 2*m_sd)
  
  # smd <- append(smd, m, after = length(smd))
  # low_CI <- append(low_CI, m_lower, length(low_CI))
  # upper_CI <- append(upper_CI, m_upper, length(upper_CI))
  
  
  
  #plot_name <- c("Forest Plot", n)
  
  col_vec <- c()
  pop <- c()
  count <- 0
  
  for (i in n$Control.1) {
    
    cont <- c("TAU/WL"= 1, "J" = 2, "PE"  = 3  , "MM" = 4  ,  "PA"  = 5, "RM" = 6, "Sham" = 7, "PMR" = 8)
    
    colors <- c("cornsilk3", "rosybrown2","orchid", "lightblue", "olivedrab3", "sienna", "mediumpurple", "royalblue3")
    
    add <- colors[cont[i]]
    
    col_vec <- append(col_vec, add, after = length(col_vec)) 
    
    count <- count + 1
    
    num <- (n$n1i[count] + n$n2i[count])/150
    
    if (num > 1) {
      num <- 0.75
    }
    
    pop <- append(pop, num, after = length(pop))
    
  }
  
  
  # print(unique(n$Control.1))
  # print(col_vec)
  
  # labels <- prepLabelText(labeltext = n$Control.1,
  #                         nr = length(n$Control.1))  
  
  # labels <- prepLabelText(labeltext = label,
  #                         nr = length(n$Control.1) + 1)
  # 
  # print(labels)
  # 
  # 
  # graph.pos <- "left"
  # graph.pos <- prepGraphPositions(graph.pos, nc = attr(labels, "no_cols"))
  # align <- c()
  # align <- prepAlign(align, graph.pos, nc = attr(labels, "no_cols"))
  # 
  Cont = unique(n$Control.1)
  
  label <- list(authors, smd)
  
  # 
  # labels[[1]] <- append(labels[[1]], "summary", after = length(labels[[1]]))
  # 
  # print(labels[[1]])
  
  plot.new()
  
  legend("topright", inset = 0, legend = Cont, title="Legend", fill = unique(col_vec), box.lty = 0, cex = 0.5)
  
  forest <- forestplot(mean = n$yi, lower = low_CI, upper = upper_CI, labeltext = label, boxsize = pop, lineheight = "lines", title = n$Outcome[1]) |>
    fp_set_style(box = col_vec) |>
    fp_add_header(authors = c("Study"), smd = c("SMD")) #|>
   # fp_append_row(mean = m, lower = m_lower, upper = m_upper)
  
  print(forest)
  
}

#write.csv(dat1, "~/Desktop/Dr Sherry Chan Lab/HRV-B review/HRVB Review Meta Analysis - Study Standard Diff.csv", row.names = FALSE)
