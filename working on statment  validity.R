
data <- read.csv("Excel for statment validity data.csv")

data <- data %>% select(-consent1,-please_rate, -consent_info,
                        -X_submitted_by, -X_notes, -X_validation_status, -X_tags, -X_index, 
                        -X_submission_time, -X_status, -X_uuid ) 


data1 <- gather(data, question, score, 2:35) %>%  
  seperate(question, c("type", "question"))


ggplot(data1, aes(score)) + geom_histogram() + facet_wrap( .~question)

g <- as.data.frame(tapply(data1$score, data1$question, mean))
