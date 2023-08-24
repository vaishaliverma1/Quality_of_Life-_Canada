# Load the necessary packages
library('tidyverse')
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)


#get Working directory
getwd()

############################################# HEALTH-DATA-ANALYSIS #################################################################

# Read the first CSV file for Health data at Birth time
Birth_Data <- read.csv("Health_Birth_Data.csv")

#Replace all missing values with NA
Birth_Data <- Birth_Data %>%
  mutate_all(~ ifelse(is.na(.), NA, .))

head(Birth_Data)

Birth_Data

# Read the second CSV file for Health data at 65 Yrs above age.
Above65_Data <- read.csv("Health_65above_data.csv")

#Replace all missing values with NA
Above65_Data <- Above65_Data %>%
  mutate_all(~ ifelse(is.na(.), NA, .))

head(Above65_Data)

Above65_Data

# Check data types of common columns in Birth_Data
cat("Data types of common columns in Birth_Data:\n")
str(Birth_Data[c("REF_DATE", "GEO", "Age.group", "Sex", "Income.group", "VALUE")])

# Check data types of common columns in Above65_Data
cat("\nData types of common columns in Above65_Data:\n")
str(Above65_Data[c("REF_DATE", "GEO", "Age.group", "Sex", "Income.group", "VALUE")])


# Merge the files based on multiple common columns
Health_merged_data <- merge(Birth_Data, Above65_Data, by = c("REF_DATE", "GEO", "Age.group", "Sex", "Income.group", "VALUE"), all.x=TRUE, all.y = TRUE)

print(Health_merged_data)


#Selecting only useful columns
Health_merged_data <- Health_merged_data %>% 
  select(REF_DATE,GEO,Age.group,Sex,Income.group,VALUE)

print(Health_merged_data)

# Create a custom color palette for Age.group categories
custom_colors <- c("#E41A1C", "#377EB8")

# Create a new PDF device with a custom width and height
pdf("Health Data Analysis QOL.pdf", width = 20, height = 10)

# Create the bar plot using ggplot with the custom color palette
ggplot(Health_merged_data, aes(x = GEO, y = VALUE, fill = factor(Age.group))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rows = vars(Sex), cols = vars(Income.group)) +
  labs(title = "Health Data Analysis",
       x = "GEO",
       y = "Health Adjusted Life Expectancy by Sex",
       fill = "Age group") +
  scale_fill_manual(values = custom_colors) +  # Specify the custom color palette
  theme_minimal()

# Close the PDF device
dev.off()


############################################# GOOD-GOVERNANCE-DATA-ANALYSIS #################################################################

# Read the first CSV file for Governance data for age above 18 yrs
Above18_Data <- read.csv("Governance_18above.csv")

# Get the coloum names
head(Above18_Data)

# Read the second CSV files for Governance data for Men
Men_Data <- read.csv("Governance_Men.csv")

# Get the coloum names
head(Men_Data)

# Read the third CSV file for Governance data for Women
Women_Data <- read.csv("Governance_Women.csv")

# Get the column names of the Women_Data data frame
head(Women_Data)


# Check data types of common columns in Above18_Data
cat("Data types of common columns in Above18_Data:\n")
str(Above18_Data[c("REF_DATE", "GEO", "Visible.minority", "Selected.sociodemographic.characteristics", "Indicators", "VALUE")])

# Check data types of common columns in Governance_Men
cat("Data types of common columns in Above18_Data:\n")
str(Men_Data[c("REF_DATE", "GEO", "Visible.minority", "Selected.sociodemographic.characteristics", "Indicators", "VALUE")])

# Check data types of common columns in Governance_Women
cat("Data types of common columns in Above18_Data:\n")
str(Women_Data[c("REF_DATE", "GEO", "Visible.minority", "Selected.sociodemographic.characteristics", "Indicators", "VALUE")])

# Merge Above18_Data and Men_Data
merged_data1 <- merge(Above18_Data, Men_Data, 
                      by = c("REF_DATE", "GEO", "Visible.minority", "Selected.sociodemographic.characteristics", "Indicators", "VALUE"), 
                      all.x = TRUE, all.y = TRUE)

# Merge merged_data1 and Women_Data
Governance_merged_data <- merge(merged_data1, Women_Data,
                                by = c("REF_DATE", "GEO", "Visible.minority", "Selected.sociodemographic.characteristics", "Indicators", "VALUE"), 
                                all.x = TRUE, all.y = TRUE)


#Print the merged database
Governance_merged_data

#Selecting only useful columns
Governance_merged_data <- Governance_merged_data %>% 
  select(REF_DATE,GEO,Visible.minority,Selected.sociodemographic.characteristics,Indicators,VALUE)

#Print the merged database
Governance_merged_data

# Create a custom color palette for Visible.minority categories
#custom_colors <- c("#E41A1C", "#377EB8", "#E69F00", "#F0E442","#0072B2","#D55E00","#CC79A7","#000000", "#999999", "#8DD3C7")

# Create a new PDF device with a custom width and height
pdf("Good Governance Data Analysis QOL.pdf", width = 20, height = 10)


# Create a custom color palette
custom_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33")

# Create the bar plot using ggplot
ggplot(Governance_merged_data, aes(x = Visible.minority, y = VALUE, fill = Indicators)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(rows = vars(Selected.sociodemographic.characteristics)) +
  labs(title = "Good Governance Data Analysis",
       x = "Visible Minority",
       y = "Value",
       fill = "Indicators") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()


# Close the PDF device
dev.off()





