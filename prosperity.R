#loading necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
#reading data
job_data = read.csv("C:/Users/Simran/Desktop/Big Data2/jobdata.csv")
print(job_data)
#filtering required data
job_data_set<- subset(job_data, Gender == "Total, all persons" & Indicators == "Very satisfied or satisfied with job")
print(job_data_set)
#tidying data
job_new <- job_data_set[, c("GEO", "Indicators", "VALUE")]
colnames(job_new) <- c("Province", "Satisfaction", "Percentage")
jobdata1 <- subset(job_new, Province != "Canada (excluding territories)")
print(jobdata1)
#Plotting the graph
ggplot(jobdata1, aes(x = reorder(Province, Percentage), y = Percentage, fill = Province)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Job Satisfaction by Province",
       x = "Provinces",
       y = "Percentage (%)",
       fill = "Provinces") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )




