---
title: "Case"
author: "Hebert Frangiotti"
date: "2024-02-06"
output:
  word_document: default
  pdf_document: default
---

# Starting the Data

```{r}
library(readxl)

file_path <- "C:\\Users\\Admin\\Downloads\\Data_performance.xlsx"
explicit_group_data <- read_excel(file_path, sheet = "Explicit group")
implicit_group_data <- read_excel(file_path, sheet = "Implicit group")


# For the explicit_group_data dataframe
colnames(explicit_group_data)[colnames(explicit_group_data) == "Score in the selection test"] <- "Score sel."
colnames(explicit_group_data)[colnames(explicit_group_data) == "Score reading test before"] <- "read. bef"
colnames(explicit_group_data)[colnames(explicit_group_data) == "Score reading test after"] <- "read. aft"
colnames(explicit_group_data)[colnames(explicit_group_data) == "Number of aspects observed before"] <- "aspects bef"
colnames(explicit_group_data)[colnames(explicit_group_data) == "Number of aspects observed after"] <- "aspects aft"
colnames(explicit_group_data)[colnames(explicit_group_data) == "Oral score before"] <- "Oral bef"
colnames(explicit_group_data)[colnames(explicit_group_data) == "Oral score after"] <- "Oral aft"

# For the implicit_group_data dataframe
colnames(implicit_group_data)[colnames(implicit_group_data) == "Score in the selection test"] <- "Score sel."
colnames(implicit_group_data)[colnames(implicit_group_data) == "Score reading test before"] <- "read. bef"
colnames(implicit_group_data)[colnames(implicit_group_data) == "Score reading test after"] <- "read. aft"
colnames(implicit_group_data)[colnames(implicit_group_data) == "Number of aspects observed before"] <- "aspects bef"
colnames(implicit_group_data)[colnames(implicit_group_data) == "Number of aspects observed after"] <- "aspects aft"
colnames(implicit_group_data)[colnames(implicit_group_data) == "Oral score before"] <- "Oral bef"
colnames(implicit_group_data)[colnames(implicit_group_data) == "Oral score after"] <- "Oral aft"

# Adding group indicators
explicit_group <- rep("Explicit", nrow(explicit_group_data))
implicit_group <- rep("Implicit", nrow(implicit_group_data))

explicit_group_data$group <- explicit_group
implicit_group_data$group <- implicit_group

# Combining the data
combined_data <- rbind(explicit_group_data, implicit_group_data)


# Selecting variables of interest
interest_data <- combined_data[, c("Score sel.", "read. bef", "read. aft", "Oral bef", "Oral aft")]

library(ggplot2)
library(reshape2)
library(GGally)

# Correlation matrix
correlation_matrix <- cor(interest_data)
correlation_melted <- melt(correlation_matrix)

# Heatmap
ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Heatmap of Variable Correlations", x = "Variable", y = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggpairs(interest_data)



# The selection of interest_data was based on the relevance of variables related to student results before and after the course.

library(dplyr)

# Interest variables for each group
databox_implicit <- implicit_group_data[, c("read. bef", "read. aft", "Oral bef", "Oral aft")]
databox_explicit <- explicit_group_data[, c("read. bef", "read. aft", "Oral bef", "Oral aft")]

# Melting data
data_melted_implicit <- melt(databox_implicit)
data_melted_implicit$Group <- "Implicit"
data_melted_explicit <- melt(databox_explicit)
data_melted_explicit$Group <- "Explicit"

# Combining melted data
combined_melted_data <- rbind(data_melted_implicit, data_melted_explicit)

# Boxplots
ggplot(combined_melted_data %>% filter(variable %in% c("read. bef", "read. aft")), aes(x = variable, y = value, fill = Group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Reading Before and After", x = "Variable", y = "Value", fill = "Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(combined_melted_data %>% filter(!variable %in% c("read. bef", "read. aft")), aes(x = variable, y = value, fill = Group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Other Variables Before and After", x = "Variable", y = "Value", fill = "Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Observations indicate outliers affecting normality, such as Rodolpho, a photographer, and Elena, a pedagogue. Reanalysis without Rodolpho shows the impact of outliers on averages.

# Removing outlier
combined_melted_data_filtered <- combined_melted_data %>%
  filter(!(variable %in% c("read. bef", "read. aft") & Group == "Explicit" & value == 33))

# Boxplot without outlier
ggplot(combined_melted_data_filtered %>% filter(variable %in% c("read. bef", "read. aft")), aes(x = variable, y = value, fill = Group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Reading Before and After", x = "Variable", y = "Value", fill = "Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Comparing results, the mean for read. bef increased after outlier removal. This suggests outliers negatively affected the mean scores for the explicit group, while no significant impact was observed for the implicit group.

library(car)

# Linear regression models
model1_explicit <- lm(`Oral aft` ~ Age + `Study time (years)`, data = explicit_group_data)
model2_explicit <- lm(`read. aft` ~ Age + `Study time (years)`, data = explicit_group_data)
model3_implicit <- lm(`Oral aft` ~ Age + `Study time (years)`, data = implicit_group_data)
model4_implicit <- lm(`read. aft` ~ Age + `Study time (years)`, data = implicit_group_data)

# Summaries
summary(model1_explicit)
summary(model2_explicit)
summary(model3_implicit)
summary(model4_implicit)

# Removing outlier for explicit group
explicit_group_data_filtered <- explicit_group_data[explicit_group_data$`read. aft` != 33, ]

# New regression models without outlier
model1_explicit_filtered <- lm(`Oral aft` ~ Age + `Study time (years)`, data = explicit_group_data_filtered)
model2_explicit_filtered <- lm(`read. aft` ~ Age + `Study time (years)`, data = explicit_group_data_filtered)

# Summaries without outlier
summary(model1_explicit_filtered)
summary(model2_explicit_filtered)

# Shapiro-Wilk test for normality of residuals
shapiro1 <- shapiro.test(model1_explicit$residuals)
shapiro2 <- shapiro.test(model2_explicit$residuals)
shapiro3 <- shapiro.test(model3_implicit$residuals)
shapiro4 <- shapiro.test(model4_implicit$residuals)
shapirofiltered1 <- shapiro.test(model1_explicit_filtered$residuals)

# Summaries of residuals
summary(rstandard(model1_explicit))
summary(rstandard(model2_explicit))
summary(rstandard(model3_implicit))
summary(rstandard(model4_implicit))
summary(rstandard(model1_explicit_filtered))
summary(rstandard(model2_explicit_filtered))


par(mfrow = c(2, 2))

# Plot diagnostics
plot(model1_explicit)
text(x = 0, y = par("usr")[4], labels = paste("Shapiro p-value:", round(shapiro1$p.value, 4)), adj = c(0, 1), col = "blue")

plot(model2_explicit)
text(x = 0, y = par("usr")[4], labels = paste("Shapiro p-value:", round(shapiro2$p.value, 4)), adj = c(0, 1), col = "blue")

plot(model3_implicit)
text(x = 0, y = par("usr")[4], labels = paste("Shapiro p-value:", round(shapiro3$p.value, 4)), adj = c(0, 1), col = "blue")

plot(model4_implicit)
text(x = 0, y = par("usr")[4], labels = paste("Shapiro p-value:", round(shapiro4$p.value, 4)), adj = c(0, 1), col = "blue")

plot(model1_explicit_filtered)
text(x = 0, y = par("usr")[4], labels = paste("Shapiro p-value:", round(shapirofiltered1$p.value, 4)), adj = c(0, 1), col = "blue")
# Perform t-tests
result_t_after <- t.test(`Oral aft` ~ group, data = combined_data)
result_t_after2 <- t.test(`read. aft` ~ group, data = combined_data)

# T-tests with filtered data
result_t_after_filtered1 <- t.test(`Oral aft` ~ group, data = combined_data_filtered)
result_t_after_filtered2 <- t.test(`read. aft` ~ group, data = combined_data_filtered)

# Summaries
summary(result_t_after)
summary(result_t_after2)
summary(result_t_after_filtered1)
summary(result_t_after_filtered2)



# Oral aft vs. Group
p-value: 0.0364861, indicating a statistically significant difference in Oral aft scores between groups.
Read. aft vs. Group
p-value: 0.6900625, indicating no significant difference in Read. aft scores between groups.
Filtered Data Analysis
Oral aft vs. Group
p-value: 0.02973489, indicating a significant difference in Oral aft scores between groups.
Read. aft vs. Group
p-value: 0.7270604, indicating no significant difference in Read. aft scores between groups.
Summary and Implications
Significant Differences: Oral aft scores differ significantly between groups.
No Differences in Read. aft: No significant differences in Read. aft scores between groups.

# Paired t-tests
result_paired_t_test_PAIRE1 <- t.test(combined_data$`Oral aft`, combined_data$`Oral bef`, paired = TRUE, alternative = "greater")
result_paired_t_test_PAIRE2 <- t.test(combined_data$`read. aft`, combined_data$`read. bef`, paired = TRUE, alternative = "greater")

# Summaries
print(result_paired_t_test_PAIRE1)
cat("P-value for Oral aft vs. Oral bef:", result_paired_t_test_PAIRE1$p.value, "\n")

print(result_paired_t_test_PAIRE2)
cat("P-value for read. aft vs. read. bef:", result_paired_t_test_PAIRE2$p.value, "\n")

# Paired t-tests with filtered data
result_paired_t_test_PAIRE_filtered1 <- t.test(combined_data_filtered$`Oral aft`, combined_data_filtered$`Oral bef`, paired = TRUE, alternative = "greater")
result_paired_t_test_PAIRE_filtered2 <- t.test(combined_data_filtered$`read. aft`, combined_data_filtered$`read. bef`, paired = TRUE, alternative = "greater")

# Summaries
print(result_paired_t_test_PAIRE_filtered1)
cat("P-value for filtered Oral aft vs. Oral bef:", result_paired_t_test_PAIRE_filtered1$p.value, "\n")

print(result_paired_t_test_PAIRE_filtered2)
cat("P-value for filtered read. aft vs. read. bef:", result_paired_t_test_PAIRE_filtered2$p.value, "\n")



# Professional group indicators
combined_data$Professional_Group <- ifelse(combined_data$Profession %in% c("Literature student", "english translator", "reviewer", "pedagogue", "Italian teacher"), "Related to Letters", "Not Related to Letters")
combined_data_filtered$Professional_Group <- ifelse(combined_data_filtered$Profession %in% c("Literature student", "english translator", "reviewer", "pedagogue", "Italian teacher"), "Related to Letters", "Not Related to Letters")

# Regression analysis
regression_model_multiple1 <- lm(`Oral aft` ~ Professional_Group, data = combined_data)
summary(regression_model_multiple1)
boxplot(`Oral aft` ~ Professional_Group, data = combined_data, main = "Oral aft by Professional Group", xlab = "Professional Group", ylab = "Oral aft", col = c("lightblue", "lightgreen"))

regression_model_multiple_filtered1 <- lm(`Oral aft` ~ Professional_Group, data = combined_data_filtered)
summary(regression_model_multiple_filtered1)
boxplot(`Oral aft` ~ Professional_Group, data = combined_data_filtered, main = "Oral aft by Professional Group", xlab = "Professional Group", ylab = "Oral aft", col = c("lightblue", "lightgreen"))

# Replicate to Read.aft
regression_model_multiple1_read <- lm(`read. aft` ~ Professional_Group, data = combined_data)
summary(regression_model_multiple1_read)
boxplot(`read. aft` ~ Professional_Group, data = combined_data, main = "Read. aft by Professional Group", xlab = "Professional Group", ylab = "Read. aft", col = c("lightblue", "lightgreen"))

regression_model_multiple_filtered1_read <- lm(`read. aft` ~ Professional_Group, data = combined_data_filtered)
summary(regression_model_multiple_filtered1_read)
boxplot(`read. aft` ~ Professional_Group, data = combined_data_filtered, main = "Read. aft by Professional Group", xlab = "Professional Group", ylab = "Read. aft", col = c("lightblue", "lightgreen"))

# T-Test for Professional Group
result_t_Professional_Group1 <- t.test(`Oral aft` ~ Professional_Group, data = combined_data)
result_t_Professional_Group_filtered1 <- t.test(`Oral aft` ~ Professional_Group, data = combined_data_filtered)
cat("P-value for Oral aft vs. Professional Group:", result_t_Professional_Group1$p.value, "\n")
cat("P-value for filtered Oral aft vs. Professional Group:", result_t_Professional_Group_filtered1$p.value, "\n")

result_t_Professional_Group2 <- t.test(`read. aft` ~ Professional_Group, data = combined_data)
result_t_Professional_Group_filtered2 <- t.test(`read. aft` ~ Professional_Group, data = combined_data_filtered)
cat("P-value for read. aft vs. Professional Group:", result_t_Professional_Group2$p.value, "\n")
cat("P-value for filtered read. aft vs. Professional Group:", result_t_Professional_Group_filtered2$p.value, "\n")


# Linear regression for combined data
regression_model_group <- lm(`Oral aft` ~ group, data = combined_data)
summary(regression_model_group)

regression_model_group_filtered1 <- lm(`Oral aft` ~ group, data = combined_data_filtered)
summary(regression_model_group_filtered1)

# Replicate for read. aft
regression_model_group2 <- lm(`read. aft` ~ group, data = combined_data)
summary(regression_model_group2)

regression_model_group_filtered2 <- lm(`read. aft` ~ group, data = combined_data_filtered)
summary(regression_model_group_filtered2)

# Regression with additional predictors
regression_model_Age_Study <- lm(`Oral aft` ~ Age + `Study time (years)`, data = combined_data)
summary(regression_model_Age_Study)

regression_model_Age_Study_filtered <- lm(`Oral aft` ~ Age + `Study time (years)`, data = combined_data_filtered)
summary(regression_model_Age_Study_filtered)

regression_model_Age_Study3 <- lm(`read. aft` ~ Age + `Study time (years)`, data = combined_data)
summary(regression_model_Age_Study3)

regression_model_Age_Study_filtered3 <- lm(`read. aft` ~ Age + `Study time (years)`, data = combined_data_filtered)
summary(regression_model_Age_Study_filtered3)


# Correlation matrix with additional predictor
combined_data_filtered$traveled_to_italy_numeric <- ifelse(combined_data_filtered$`Have you traveled to Italy?` == "Yes", 1, 0)

cor_matrix <- cor(combined_data_filtered[, c("Oral aft", "read. aft", "Age", "Study time (years)", "traveled_to_italy_numeric")])
cor_matrix_melted <- melt(cor_matrix)

ggplot(cor_matrix_melted, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(abs(value) > 0.5, ifelse(value > 0, "Positive", "Negative"), "")), size = 3, color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Variables", y = "Variables", title = "Correlation Heatmap") +
  coord_fixed()

# Regression for travel to Italy
regression_model_ht1 <- lm(`Oral aft` ~ `Have you traveled to Italy?`, data = combined_data)
summary(regression_model_ht1)

regression_model_ht2 <- lm(`read. aft` ~ `Have you traveled to Italy?`, data = combined_data)
summary(regression_model_ht2)


# Study context indicators
combined_data$Study_context_category <- ifelse(combined_data$`Study context` == "university", "University", "Not University")
combined_data_filtered$Study_context_category <- ifelse(combined_data_filtered$`Study context` == "university", "University", "Not University")

# Regression analysis for study context
regression_model_Study1 <- lm(`Oral aft` ~ `Study_context_category`, data = combined_data)
summary(regression_model_Study1)

regression_model_Study2 <- lm(`read. aft` ~ `Study_context_category`, data = combined_data)
summary(regression_model_Study2)

regression_model_Study_filtered <- lm(`Oral aft` ~ `Study_context_category`, data = combined_data_filtered)
summary(regression_model_Study_filtered)

regression_model_Study_filtered <- lm(`read. aft` ~ `Study_context_category`, data = combined_data_filtered)
summary(regression_model_Study_filtered)




#The analyses performed on the combined and filtered datasets provide valuable insights into the impact of different variables on student performance. Significant differences were found in Oral aft scores between groups, while Read. aft scores showed no significant differences. Professional background and study context were not significant predictors of performance in most cases. Further research with larger sample sizes and additional variables could help to better understand these relationships.







