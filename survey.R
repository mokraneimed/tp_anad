
#
library(dplyr)
getwd()
data <- read.csv("anad/new_data.csv")

#data <- data %>%
#  mutate(AgeClass = case_when(
#    Age < 25 ~ "<25",
#    Age >= 25 & Age <= 34 ~ "25-34",
#    Age >= 35 & Age <= 44 ~ "35-44",
#    Age >= 45 & Age <= 54 ~ "45-54",
#    Age >= 55 ~ ">55"
# ))
# data <- data %>%
#   select(EmpID, JobSatisfaction, AgeClass, Gender, MaritalStatus, JobLevel, 
#          EduLevel)

head(data)

# Statistiques
library(ggplot2)

# Fréquences pour chaque classe dans les colonnes (1 à 5)
freq_job_satisfaction <- table(data$JobSatisfaction)
freq_mstatus <- table(data$MaritalStatus)
freq_job_level <- table(data$JobLevel)
# freq_dept <- table(data$Dept)
freq_gender <- table(data$Gender)
freq_AgeClass <- table(data$AgeClass)
freq_EduLevel <- table(data$EduLevel)

print(freq_job_satisfaction)
print(freq_mstatus)
print(freq_job_level)
print(freq_gender)
print(freq_AgeClass)
print(freq_EduLevel)
# print(freq_dept)

# Histogramme pour JobSatisfaction
ggplot(data, aes(x = JobSatisfaction)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  scale_x_continuous(breaks = 1:5) +
  labs(title = "Histogramme de la satisfaction au travail",
       x = "Satisfaction au travail (1-5)",
       y = "Fréquence") +
  theme_minimal()
ggsave("anad/job_satisfaction_histogram.png", width = 6, height = 4, dpi = 300)

# Visualize the frequency of Age categories (assuming Age is divided into categories)
data$AgeClass <- factor(data$AgeClass, levels = c("<25", "25-34", "35-44", "45-54", ">55"))
ggplot(data, aes(x = AgeClass)) +
  geom_bar(fill = "salmon", color = "black") +
  labs(title = "Fréquence des catégories d'âge",
       x = "Âge",
       y = "Fréquence") +
  theme_minimal()

# Save as an image
ggsave("anad/age_frequency.png", width = 6, height = 4, dpi = 300)

data_disjoint <- data %>%
  select(-JobSatisfaction) %>%  # Remove non-categorical columns
  mutate_if(is.factor, as.character) %>%     # Convert factors to characters
  model.matrix(~ . - 1, data = .)            # One-hot encoding (remove intercept column)

head(data_disjoint)

#data_mca <- data %>%
#  mutate(JobSatisfaction = factor(JobSatisfaction, levels = 1:5))

# Run MCA on the corrected data
afcm <- MCA(data_mca)

eig_values <- afcm$eig
eig_table <- data.frame(
  Dimension = 1:nrow(eig_values),
  Eigenvalue = eig_values[, 1],
  Percentage = eig_values[, 2],
  Cumulative = eig_values[, 3]
)

print(eig_table)

library(factoextra)
fviz_mca_biplot(afcm,
		    label = "var", 
                repel = TRUE,          # Évite le chevauchement des étiquettes
                ggtheme = theme_minimal(), 
                title = "Biplot Individus-Variables")


