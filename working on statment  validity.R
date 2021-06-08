
data <- read.csv("Excel for statment validity data.csv") #load data

library(ggplot2)
library(tidyverse) #load packages



#simplify data
data <- data %>% select(-consent1,-please_rate, -consent_info,
                        -X_submitted_by, -X_notes, -X_validation_status, -X_tags, -X_index, 
                        -X_submission_time, -X_status, -X_uuid ) 
data <- na.omit(data)


#create dataframe for visualizing rating choice frequencey of participants per question
data1 <- gather(data, question, score, 2:35) 

ggplot(data1, aes(score)) + geom_histogram() + facet_wrap( .~question) + labs(title = "Distribution of Statment Ratings")



dataN <- data1 %>%
  group_by(question) %>% 
  summarise(mean = mean(score), sum = sum(score)) %>% 
  mutate(type = substr(question, 1,1)) 


Freq <- as.data.frame.matrix(table(data1$question, data1$score))
Freq <- Freq %>% 
  rownames_to_column("question")
  
colnames(Freq)  = c("question", "NegTwo", "NegOne", "Zero", "One", "Two")

Freq <- Freq %>% 
  mutate(V = NegOne + NegTwo, E = One + Two)



dataN <- left_join(dataN, Freq, by = "question") 


N <- dataN %>% 
  filter(type == "N")

V <- dataN %>% 
  filter(type == "V")

E <- dataN %>% 
  filter(type == "E")
 


ggplot(dataN, aes(mean, sum)) + geom_point() + facet_wrap(.~type)
    

  








tapply(data1$score, data1$question, summary)

data2 <- separate(data1, question, c("type", "question"),"(?<=[A-Z),(?=[1-9])(?=[1-9])")

dataN <- data1 %>% filter(question == starts_with("N"))

?pivot_wider
c <- pivot_wider(data1, question, score)
data2 <- data1 %>% 
  group_by(question) %>% 
  mutate(mean = mean(score))


DataMean <- data1 %>% 
  select(-X_id, -knowledge) %>% 
  group_by(question) %>% 
  mutate(mean = mean(score)) 
 


