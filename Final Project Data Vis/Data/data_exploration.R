# load libraries -----
library(tidyverse)
library(readr)
library(skimr)
library(stringr)
library(dbplyr)

# read data -----
data_jobs_cleaned <- read_csv("Processed/data_jobs_cleaned.csv")


#Total Salary ----
data_jobs_cleaned %>% 
  skim_without_charts(totalyearlycompensation)

# Job Title and Total Yearly Compensation in Respect to Gender

ggplot(data = gender, mapping = aes(x = title, y = totalyearlycompensation)) + 
  geom_boxplot(aes(fill = gender)) +
  ylim(c(0,400000)) +
  ylab("Total Yearly Compensation") +
  xlab("Job Title") +
  labs(title = "The Total Yearly Compensation in Each Job Title in respect to Gender") +
  coord_flip()

# Title -----

data_jobs_cleaned %>% 
  skim_without_charts(title)

## univariable title

job_types <- data_jobs_cleaned %>%
  group_by(title) %>%
  arrange(title) %>%
  count(sort = TRUE)

print(job_types)

ggplot(data_jobs_cleaned, aes(x = title)) +
  geom_bar(fill = "#69b3a2") +
  labs(
    title = "Job Titles Distribution"
  ) +
  coord_flip()



# Job Title Vs Compensation

ggplot(data = data_jobs_cleaned, mapping = aes(x = reorder(title, totalyearlycompensation, FUN = median), y = totalyearlycompensation)) + 
  geom_boxplot() +
  ylim(c(0,400000)) +
  xlab(" Job Title") +
  ylab("Total Yearly Compensation") +
  labs(title = "Total Yearly Compensation within each Job Title") +
  coord_flip() 

# Gender -----

# Does gender impact pay? 
gender <- data_jobs_cleaned %>%
  group_by(gender) %>%
  arrange(gender) %>%
  count(sort = TRUE)

#univariable gender

gender <- data_jobs_cleaned %>%
  filter(gender == 'male' | gender == 'female')

gender_prop <- gender %>%
  count(gender) %>%
  mutate(proportions(n/sum(n)))

print(gender_prop)

ggplot(gender, aes(x = gender)) +
  geom_bar(fill = "#69b3a2") +
  labs(
    title = "Male and Female Distribution"
  )

# Gender and Job Titles

gender <- data_jobs_cleaned %>%
  filter(gender == 'male' | gender == 'female')

title_order <- gender %>%
  count(title, gender) %>%
  group_by(title) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup() %>%
  filter(gender == "male") %>%
  arrange(prop) %>%
  pull(title)


ggplot(data = gender, mapping = aes(x = factor(title, levels = title_order), fill = gender)) + 
  geom_bar(position = "fill") +
  ylab("Proportion") +
  xlab(" Job Title") +
  labs(title = "Proportion of Gender within Job Title") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
               
               
# Education ----

# univariable education

education_prop <- data_jobs_cleaned %>%
  count(Education) %>%
  mutate(proportions(n/sum(n)))

print(education_prop)

ggplot(data_jobs_cleaned, aes(x = Education)) +
  geom_bar(fill = "#69b3a2") +
  labs(
    title = "Education Distribution"
  )

# Job Title and Total Yearly Compensation in Resepct to Education

scientist_engineer <- data_jobs_cleaned %>%
  filter(title == c('software engineer' , 'data scientist', 'mechanical engineer', 'hardware engineer'))

ggplot(data = scientist_engineer, mapping = aes(x = title, y = totalyearlycompensation)) + 
  geom_boxplot(aes(fill = Education)) +
  ylim(c(0,400000)) +
  ylab("Total Yearly Compensation") +
  xlab("Job Title") +
  labs(title = "The Total Yearly Compensation in Each Job Title in respect to Education") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Company ----

# univeriable company 

data_jobs_cleaned %>%
  group_by(company) %>%
  arrange(company) %>%
  count(sort = TRUE)

# bivariable best company for software engineering

companies_software <- data_jobs_cleaned %>%
  filter(title == "software engineer") %>%
  count(company, sort = TRUE) %>%
  head(30)

companies_software <- companies_software$company

plot_software <- data_jobs_cleaned %>%
  filter(title == "software engineer") %>%
  filter(company %in% companies_software) %>%
  group_by(company) %>%
  summarise(median_comp = median(totalyearlycompensation)) 

ggplot(plot_software) +
  geom_col(mapping = aes(x = reorder(company, median_comp), y= median_comp)) +
  ylab("Median Compensation") +
  xlab("Company") +
  labs(title = "Median Compensation of Data Scienctist Salaries within each Comapny ") +
  coord_flip()

# bivariable best company for data_science  

companies_data_science <- data_jobs_cleaned %>%
  filter(title == "data scientist") %>%
  count(company, sort = TRUE) %>%
  head(30)

companies_data_science <- companies_data_science$company

plot_data_science <- data_jobs_cleaned %>%
  filter(title == "data scientist") %>%
  filter(company %in% companies_data_science) %>%
  group_by(company) %>%
  summarise(median_comp = median(totalyearlycompensation)) 

ggplot(plot_data_science) +
  geom_col(mapping = aes(x = reorder(company, median_comp), y= median_comp)) +
  ylab("Median Compensation") +
  xlab("Company") +
  labs(title = "Median Compensation of Engineer Salaries within each Comapny ") +
  coord_flip()

# Years at Company in Respect to Gender and Race 

ggplot(data = gender) + 
  geom_histogram(aes(x = yearsatcompany)) + 
  ylim(c(0, 150)) +
  labs(title = "The Years at Company in Different Race and Gender Groups") +
  facet_wrap(Race ~ gender)
  
# Race ----

# univariable race

race_prop <- data_jobs_cleaned %>%
  count(Race) %>%
  mutate(proportions(n/sum(n)))

print(race_prop)

ggplot(data_jobs_cleaned, aes(x = Race)) +
  geom_bar(fill = "#69b3a2") +
  labs(
    title = "Race Distribution"
  )

# Race and Job Title
  
  scientist_engineer <- data_jobs_cleaned %>%
    filter(title == c('software engineer' , 'data scientist', 'mechanical engineer', 'hardware engineer'))
  
  title_order_race <- scientist_engineer %>%
    count(title, Race) %>%
    group_by(title) %>%
    mutate(prop = n/sum(n)) %>%
    ungroup() %>%
    filter(Race == "asian") %>%
    arrange(prop) %>%
    pull(title)
  
  scientist_engineer %>%
    filter(!is.na(Race)) %>%
    ggplot() +
    geom_bar(aes(x = factor(title, levels = title_order_race), fill = Race), position = "fill") +
    ylab("Proportion") +
    xlab("Job Title") +
    labs(title = "Proportion of Race within Job Title") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
