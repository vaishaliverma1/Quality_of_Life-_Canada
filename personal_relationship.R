#loading necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
#reading data
personal_data = read.csv("C:/Users/Simran/Desktop/Big Data2/personaldata.csv")
print(personal_data)
#filtering required data
personal_data_set<- subset(personal_data, Gender == "Total, all persons" & Indicators == "Satisfaction with personal relationships rating of 8, 9 or 10")
print(personal_data_set)
#tidying data
personal_new <- personal_data_set[, c("GEO", "Indicators", "VALUE")]
 colnames(personal_new) <- c("Province", "Satisfaction", "Percentage")
life_data <- subset(personal_new, Province != "Canada (excluding territories)")
print(life_data)
library(ggplot2)
library(RColorBrewer)
#library(RColorBrewer)

# Define the base colors for the ombre effect
base_colors <- brewer.pal(12, "Set1")  # Using Brewer palette as base colors

# Generate ombre shades for each base color
num_shades <- 5  # Number of shades for each color
ombre_palette <- character(length(base_colors) * num_shades)
for (i in seq_along(base_colors)) {
  shades <- colorRampPalette(c("white", base_colors[i]))(num_shades + 1)
  ombre_palette[((i - 1) * num_shades + 1):(i * num_shades)] <- shades[-1]
}

# Your ggplot code using the ombre palette
ggplot(life_data, aes(x = reorder(Province, Percentage), y = Percentage, fill = Province)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Personal Satisfaction by Province",
       x = "Provinces",
       y = "Percentage (%)",
       fill = "Provinces") +
  scale_fill_manual(values = ombre_palette) +  # Use the ombre palette here
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
