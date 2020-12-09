library(tidyverse)
library(dplyr)
library(ggplot2)

#function that return the change point given dataframe
seq_change <- function(data) {
  df2 <- data %>% mutate(cumsum = cumsum(data$x),
                         ID = row_number(), #ID is reset every sequence for calculation but k stays the same to track change point
                         total =sum(data$x)) %>% 
    mutate(length_df = length(ID)) %>% 
    mutate(frac = ID/length(ID)) %>% 
    mutate(temp1 = frac * total) %>% 
    mutate(absolute = abs(cumsum - temp1)) %>% 
    mutate(T = absolute / (sqrt(length(ID))))
  
  #print(df2[1:5,])
  #maxplot <- ggplot(df2, aes(x = k, y = T)) + geom_col()
  
  #Find the max value of T and return k where the max value is, this is the cutoff point
  #if it is tie return the smaller k
  maxk <- df2$k[max.col(t(df2$T), ties.method = "first")]
  maxT <- df2$T[max.col(t(df2$T), ties.method = "first")]
  #print(paste("maxT",maxT))
  #print(paste("maxk",maxk))
  
  return (list(maxT=maxT,maxk=maxk))
}

coordinates <- function(segment) {
  if (segment$maxT >= 1.23) { # Kolmogorov critical values
    
    vector <- data.frame(0,segment$maxk)
  }else {
    vector <- data.frame(1,0)
  }
  names(vector) <- c("first_coord", "sec_coord")
  
  return (vector)
}

all_left_vectors <- data.frame(matrix(ncol = 2, nrow = 0))
names(all_left_vectors) <- c("first_coord", "sec_coord")

all_right_vectors <- data.frame(matrix(ncol = 2, nrow = 0))
names(all_right_vectors) <- c("first_coord", "sec_coord")

N <- 150
var <- 0.1
sd <- sqrt(var)
N1 <- floor(N/3)
N2 <- floor(2*N/3)-floor(N/3)
N3 <- N - floor(2*N/3)

iter = 200
for (i in 1:iter){
  print(paste("i = ", i))
  #generate n independent normal random variables
  #first sequence
  x1 <- rnorm(N1, mean = 2, sd = sd)
  #second sequence
  x2 <- rnorm(N2, mean = 1, sd = sd)
  #third sequence
  x3 <- rnorm(N3, mean = 0, sd = sd)
  #combine 3 sequences
  x<- c(x1,x2,x3)
  #create a dataframe
  data <- data.frame(x) %>% mutate(k = row_number())
  
  #print(data[1:5,])
  
  #print('First cusum')
  first_cusum <- seq_change(data)
  left_cut <- subset(data, k<=first_cusum$maxk) %>% select(k, x)
  #print('Second cumsum')
  #print(left_cut)
  second_cusum_left <- seq_change(left_cut)
  #print(second_cusum_left$maxk)
  #print(second_cusum_left$maxT)
  left_vector <- coordinates(second_cusum_left)
  all_left_vectors <<- rbind(all_left_vectors, left_vector)
  
  right_cut <- subset(data, k>first_cusum$maxk) %>% select(k, x)
  second_cusum_right <- seq_change(right_cut)
  right_vector <- coordinates(second_cusum_right)
  all_right_vectors <<- rbind(all_right_vectors, right_vector)
}

result <- function(all_vectors) {
  prob_change <- mean(all_vectors$first_coord)
  sec_loc_change <- sum(all_vectors$sec_coord)/(nrow(all_vectors)-nrow(all_vectors)*prob_change)
  
  return (list(1-prob_change, round(sec_loc_change,2)))
}

r_left <- result(all_left_vectors)
r_right <- result(all_right_vectors)

left_title <- paste("Point of Change (Left) = ", r_left[2], ", Prob = ", r_left[1])
right_title <- paste("Point of Change (Right) = ", r_right[2], ", Prob = ", r_right[1])

#ggplot(all_left_vectors,aes(x = sec_coord)) + geom_histogram()
#ggplot(all_right_vectors,aes(x = sec_coord)) + geom_histogram()
#as.data.frame(table(all_left_vectors$sec_coord))   

iterations <- paste("Iterations = ", iter) 
variables_num <- paste("Sample Size = ", N)
variance <- paste("Variance = ", var)

d = data.frame(x = c(all_left_vectors$sec_coord, all_right_vectors$sec_coord), 
               type=rep(c("Left", "Right"), c(length(all_left_vectors$sec_coord), length(all_right_vectors$sec_coord))))
ggplot(d, aes(x=x, color=type)) + geom_histogram(fill="white",position="dodge")+
  ggtitle(paste0("Location of Change Points", "\n", iterations,"\n",variables_num,"\n",variance, "\n",left_title, "\n",right_title))+
  xlab("Location") + ylab("Count") + theme_light() + theme(plot.title = element_text(size=10)) 

# ggplot(d, aes(x = x, color=type)) + 
#   geom_histogram(aes(y = (..count..)/sum(..count..)), fill="white",position="dodge") +
#   scale_y_continuous(labels = percent)

p = data.frame(x = c(all_left_vectors$first_coord, all_right_vectors$first_coord), 
               type=rep(c("Left", "Right"), c(length(all_left_vectors$first_coord), length(all_right_vectors$first_coord))))
ggplot(p, aes(x=x, color=type)) + geom_histogram(fill="white",position="dodge")+
  ggtitle(paste0("Change Detection (1 = No, 0 =Yes)", "\n", iterations,"\n",variables_num,"\n",variance))+
  xlab("Location") + ylab("Count") + theme_light() + theme(plot.title = element_text(size=10)) 
