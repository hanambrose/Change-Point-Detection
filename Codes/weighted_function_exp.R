library(dplyr)
library(ggplot2)

seq_change <- function(data, kappa) {
  
  df2 <- data %>% mutate(cumsum = cumsum(data$x),
                         ID = row_number(), #ID is reset every sequence for calculation but k stays the same to track change point
                         total =sum(data$x)) %>%
    mutate(length_df = length(ID)) %>%
    mutate(frac = ID/length(ID)) %>%
    mutate(temp1 = frac * total) %>%
    mutate(absolute = abs(cumsum - temp1)) %>%
    mutate(T1 = absolute / (sqrt(length(ID)))) %>%
    mutate(T = T1/((frac * (1-frac))^kappa)) # kappa = 0.1, 0.45
  
  df2$T[is.na(df2$T)] <- 0
  
  #Find the max value of T and return k where the max value is, this is the cutoff point
  #if it is tie return the smaller k
  maxk <- df2$k[max.col(t(df2$T), ties.method = "first")]
  maxT <- df2$T[max.col(t(df2$T), ties.method = "first")]
  
  variables_num <- paste("Sample Size = ",N,",")
  variance <- paste("Variance = ", var)
  maxk_plot <- paste("Max_k = ", maxk,",")
  maxT_plot <- paste("Max_T = ", round(maxT,2))
  kp <- paste("Kappa = ", kappa)
  
  maxplot <- ggplot(df2, aes(x = k, y = T)) + 
    geom_col(color="gray40", fill="gray", alpha =0.5, width = 0.1)+ 
    ggtitle(paste0("Weighted Function T(k) ","\n",variables_num,variance,"\n",kp, "\n",maxk_plot,maxT_plot))+
    theme_light()
  
  print(maxplot)
  
  return (list(maxT=maxT,maxk=maxk))
}

N <- 100
var <- 0.1
sd <- sqrt(var)
kappa = 0.1

# N1 <- floor(N/3)
# N2 <- floor(2*N/3)-floor(N/3)
# N3 <- N - floor(2*N/3)
# #generate n independent normal random variables
# #first sequence
# x1 <- rnorm(N1, mean = 2, sd = sd)
# #second sequence
# x2 <- rnorm(N2, mean = 1, sd = sd)
# #third sequence
# x3 <- rnorm(N3, mean = 0, sd = sd)
# #combine 3 sequences
# x<- c(x1,x2,x3)

x <- rexp(N,var)
#create a dataframe
df <- data.frame(x) %>% mutate(k = row_number())
seq_change(df, kappa)