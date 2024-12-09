#require-data-manipulation-library-for-R
library(dplyr)
library(ggplot2)
#if-not-installed download it
install.packages("dplyr")
#open-the-csv-file
tbdata_csv <- read.csv("/home/setup/Downloads/TB-Cases.csv")

#--------- QUESTION(1) ----------------
# separate male patient from female patients. (where male =1 and female = 2)
male_datas = subset(tbdata_csv, SEX==1)
female_datas = subset(tbdata_csv, SEX==2)

#-------- QUESTION (2)---------------
#compute the average age of patients (male).from quest 1
average_males <- mean(male_datas$AGE, na.rm = T) #where narm arg strip of NULL values
cat("Average Male is", average_males , "\n")
#compute the average age of patients (female).from quest 1
average_females <- mean(female_datas$AGE, na.rm = T) #where narm arg strip of NULL values
cat("Average Female is", average_females , "\n")

average_ages <- data.frame(
  Gender = c("Male", "Female"),
  AverageAge = c(average_males, average_females)
)

# Create the pie chart using ggplot2
ggplot(average_ages, aes(x = "", y = AverageAge, fill = Gender)) +
  geom_bar(stat = "identity", width = 1, color = "white") + # Bar chart as base
  coord_polar(theta = "y") + # Convert to pie chart
  theme_void() + # Simplify to remove unnecessary elements
  labs(title = "Average Age by Gender") + # Add chart title
  scale_fill_manual(values = c(rgb(32/255, 134/255, 135/255), rgb(50/255, 77/255, 142/255))) # Use RGB color values
# Assign specific colors

#-------- QUESTION (3)---------------
# which year recorded the highest cases of TB across the gender?
#we calculate the total cases for each-year and get -the max-occurence
total_cases_by_year <- tbdata_csv %>%
  group_by(YEAR) %>%
  summarise(Total_Cases = n())
#the above creates a TABLE and OF YEAR AND TOTALCASES COUNTS

#then we can get the year with highest cases
year_with_highest_cases <- total_cases_by_year %>% 
  filter(Total_Cases == max(Total_Cases))
print(year_with_highest_cases$YEAR)
#generating-the-plot
bar_chart <- ggplot(total_cases_by_year, aes(x = as.factor(YEAR), y = Total_Cases)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  geom_text(aes(label = Total_Cases), vjust = -0.5, size = 3) +
  labs(
    title = "Total TB Cases by Year",
    x = "Year",
    y = "Total Cases"
  ) +
  theme_minimal()
bar_chart <- bar_chart + 
  geom_bar(
    data = year_with_highest_cases,
    aes(x = as.factor(YEAR), y = Total_Cases),
    stat = "identity",
    fill = "#f16b6a",
    color = "black",
  )
print(bar_chart)
#------------QUESTION (4)------------------
#From the dataset, separate the samples with respect to race.

split_by_race <- split(tbdata_csv, tbdata_csv$RACE)
#splitting-by-race-and-getting-the first few rows of the first data
head(split_by_race[1])

#--------------QUESTION (5) ------------------
#From your solution in Q4, compute the average age of patients by gender across race.

# Compute the average age by gender across race
average_age_by_gender_race <- tbdata_csv %>%
  filter(RACE %in% 1:4) %>%  # Filter for valid race categories i.e 1,2,3,4
  group_by(RACE, SEX) %>%    # Group by RACE and SEX
  summarise(Average_Age = mean(AGE, na.rm = TRUE), .groups = "drop") # Compute average age

# Display the results
print("Average Age of Patients by Gender Across Race:")
print(average_age_by_gender_race)

#------------- QUESTION (6) ----------------------
# Compute the total cases for each race and year
cases_by_race_year <- tbdata_csv %>%
  filter(RACE %in% 1:4)  %>% #filter out non -valid values
  group_by(RACE, YEAR) %>%
  summarise(Total_Cases = n(), .groups = "drop")

print(cases_by_race_year)

# Find the year with the highest number of TB cases for each race
highest_cases_by_race <- cases_by_race_year %>%
  group_by(RACE) %>%
  filter(Total_Cases == max(Total_Cases)) %>%
  ungroup()

# Display the results
print("Year with the highest cases of TB across each race:")
print(highest_cases_by_race)

ggplot(cases_by_race_year, aes(x = as.factor(YEAR), y = Total_Cases, fill = as.factor(RACE))) +
  geom_bar(stat = "identity", position = "dodge") + # Use 'dodge' for clustered bars
  scale_fill_manual(
    values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"), 
    labels = c("African American", "Caucasian", "Hispanic", "Native American")
  ) +
  labs(
    title = "TB Cases by Race and Year",
    x = "Year",
    y = "Total Cases",
    fill = "Race"
  ) +
  theme_minimal()

# --------QUESTION (7) -------------
#which ethnicity recorded the highest cases of TB across the race?

# Compute total cases for each race and ethnicity
cases_by_race_ethnicity <- tbdata_csv %>%
  group_by(RACE, ETHNICITY) %>%
  summarise(Total_Cases = n(), .groups = "drop")

# Find the ethnicity with the highest TB cases for each race
highest_cases_by_ethnicity <- cases_by_race_ethnicity %>%
  group_by(RACE) %>%
  filter(RACE %in% 1:4)  %>% #filter out non -valid values
  filter(Total_Cases == max(Total_Cases)) %>%
  ungroup()

# Display the results
print("Ethnicity with the highest cases of TB across each race:")
print(highest_cases_by_ethnicity)

# ------------  QUESTION (8) ------------------
#which month (across race) is TB most prevalence?
# Compute total cases for each month across all races
cases_by_month <- tbdata_csv %>%
  group_by(MONTH) %>%
  summarise(Total_Cases = n(), .groups = "drop")

print(cases_by_month)
# Find the month with the highest TB cases
most_prevalent_month <- cases_by_month %>%
  filter(Total_Cases == max(Total_Cases))

# Display the results
print("Month with the highest TB cases across races:")
print(most_prevalent_month)

# Bar Chart: Total TB Cases by Month
ggplot(cases_by_month, aes(x = factor(MONTH, levels = 1:12), y = Total_Cases)) +
  geom_bar(stat = "identity", fill = "#1f78b4", color = "black") +
  labs(
    title = "Total TB Cases by Month",
    x = "Month",
    y = "Total Cases"
  ) +
  scale_x_discrete(labels = month.abb) + # Replace 1:12 with month abbreviations (Jan, Feb, ...)
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels for better readability
  )

#--------------   QUESTION (9) --------------
#Q9: Prepare a report of all your findings - using PowerPoint presentation application.



