
data <- read.csv("Excel for statment validity data.csv")

library(ggplot2)
library(tidyverse)


data <- data %>% select(-consent1,-please_rate, -consent_info,
                        -X_submitted_by, -X_notes, -X_validation_status, -X_tags, -X_index, 
                        -X_submission_time, -X_status, -X_uuid ) 
data <- na.omit(data)

data1 <- gather(data, question, score, 2:35) 




ggplot(data1, aes(score)) + geom_histogram() + facet_wrap( .~question) + labs(title = "Distribution of Statment Ratings") + 

g <- as.data.frame(tapply(data1$score, data1$question, mean))

tapply(data1$score, data1$question, summary)

data2 <- separate(data1, question, c("type", "question"),"(?<=[A-Z),(?=[1-9])")

dataN <- data1 %>% filter(question == starts_with("N"))

?pivot_wider
c <- pivot_wider(data1, question, score)
data2 <- data1 %>% 
  group_by(question) %>% 
  mutate(mean = mean(score))


DataMean <- data1 %>% 
  select(-X_id, -knowledge) %>% 
  group_by(question) %>% 
  mutate(mean = mean(score)) %>% 
  separate(question, c("type", "number"), sep = "") 

ggplot(data2, aes(x=))