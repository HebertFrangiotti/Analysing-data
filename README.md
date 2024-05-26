# Analysing-data
Analysing data on the effects of explicit and implicit instruction on sociolinguistic competence in Brazilian adults learning Italian


# Starting the data...

```{r }
library(readxl)

file_path = "C:\\Users\\Admin\\Downloads\\Data_performance.xlsx"
explicit_group_data <- read_excel(file_path, sheet = "Explicit group")
implicit_group_data <- read_excel(file_path, sheet = "Implicit group")
```


Renaming columns

# For the explicit_group_data dataframe
nomes_colunas_explicit <- colnames(explicit_group_data)
print(nomes_colunas_explicit)

# For the implicit_group_data dataframe
nomes_colunas_implicit <- colnames(implicit_group_data)
print(nomes_colunas_implicit)

# Renaming columns in the explicit_group_data dataframe
colnames(explicit_group_data)[colnames(explicit_group_data) == "Score in the selection test"] <- "Score sel."
colnames(explicit_group_data)[colnames(explicit_group_data) == "Score reading test before"] <- "read. bef"
colnames(explicit_group_data)[colnames(explicit_group_data) == "Score reading test after"] <- "read. aft"
colnames(explicit_group_data)[colnames(explicit_group_data) == "Number of aspects observed before"] <- "aspects bef"
colnames(explicit_group_data)[colnames(explicit_group_data) == "Number of aspects observed after"] <- "aspects aft"
colnames(explicit_group_data)[colnames(explicit_group_data) == "Oral score before"] <- "Oral bef"
colnames(explicit_group_data)[colnames(explicit_group_data) == "Oral score after"] <- "Oral aft"

# Renaming columns in the implicit_group_data dataframe
colnames(implicit_group_data)[colnames(implicit_group_data) == "Score in the selection test"] <- "Score sel."
colnames(implicit_group_data)[colnames(implicit_group_data) == "Score reading test before"] <- "read. bef"
colnames(implicit_group_data)[colnames(implicit_group_data) == "Score reading test after"] <- "read. aft"
colnames(implicit_group_data)[colnames(implicit_group_data) == "Number of aspects observed before"] <- "aspects bef"
colnames(implicit_group_data)[colnames(implicit_group_data) == "Number of aspects observed after"] <- "aspects aft"
colnames(implicit_group_data)[colnames(implicit_group_data) == "Oral score before"] <- "Oral bef"
colnames(implicit_group_data)[colnames(implicit_group_data) == "Oral score after"] <- "Oral aft"


DATA IN A SINGLE SPREADSHEET


# Creating an indicator vector for the group
explicit_group <- rep("Explicit", nrow(explicit_group_data))
implicit_group <- rep("Implicit", nrow(implicit_group_data))

# Adding the group variable to the original data frames
explicit_group_data$group <- explicit_group
implicit_group_data$group <- implicit_group

# Combining the data from both groups
combined_data <- rbind(explicit_group_data, implicit_group_data)



Create a separate data frame for analyzing scores, let's call it "interest_data" where the most relevant scores will be analyzed



```{r }
# Selecting only the variables of interest from the combined data
interest_data <- combined_data[, c("Score sel.", 
                                        "read. bef", 
                                        "read. aft", 
                                        "Oral bef", 
                                        "Oral aft")]
```


Below the correlation of data will be analyzed in interest_data

```{r }

# Calculating the correlation matrix
correlation_matrix <- cor(interest_data)

library(ggplot2)
library(reshape2)
library(GGally)

# Converting the matrix into a suitable format for plotting
correlation_melted <- melt(correlation_matrix)

# Plotting the heatmap
ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Heatmap of Variable Correlations",
       x = "Variable",
       y = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggpairs(interest_data)

```

interest_data - This selection was made because the variables of interest are related to the results obtained by students before and after the course.


So now will separate for databox_implicit only variables of interest for the implicit group and then create databox_explicit and do the same for explicit to show in the boxplot

```{r }
library(dplyr)

# Select only the variables of interest from the combined data for the Implicit group
databox_implicit <- implicit_group_data[, c( 
                                            "read. bef", 
                                            "read. aft", 
                                            "Oral bef", 
                                            "Oral aft")]

# Select only the variables of interest from the combined data for the Explicit group
databox_explicit <- explicit_group_data[, c(
                                            "read. bef", 
                                            "read. aft", 
                                            "Oral bef", 
                                            "Oral aft")]
```


Below it will be analyzed in a box plot in explicit and implicit groups and a new boxplot for reading bef and aftr separately from the others

```{r }

# Calculating the number of other variations in each group
other_variations_implicit <- rowSums(databox_implicit[, -1] != 0)
other_variations_explicit <- rowSums(databox_explicit[, -1] != 0)

# Creating a table with the number of other variations in each group
other_variations_table <- data.frame(Group = c("Implicit", "Explicit"),
                                     Other_Variations = c(sum(other_variations_implicit), 
                                                          sum(other_variations_explicit)))

# Melt the data to prepare for boxplot
data_melted_implicit <- melt(databox_implicit, id.vars = NULL)
data_melted_implicit$Group <- "Implicit"

data_melted_explicit <- melt(databox_explicit, id.vars = NULL)
data_melted_explicit$Group <- "Explicit"

# Combine the data from Implicit and Explicit groups
combined_melted_data <- rbind(data_melted_implicit, data_melted_explicit)

# Plotting boxplot for reading bef and aftr separately
ggplot(combined_melted_data %>% filter(variable %in% c("read. bef", "read. aft")), aes(x = variable, y = value, fill = Group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Reading Before and After",
       x = "Variable",
       y = "Value",
       fill = "Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plotting boxplot for rest of variables bef and aftr separately
ggplot(combined_melted_data %>% filter(!variable %in% c("read. bef", "read. aft")), aes(x = variable, y = value, fill = Group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Other Variables Before and After",
       x = "Variable",
       y = "Value",
       fill = "Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display table with the number of other variations in each group
print(other_variations_table)




```




We can notice outliers that deviate from the normality of the data such as Rodolpho, a photographer, who has already traveled to Italy and had obtained a lower score in the score reading test before, which repeated in the after test. On the other hand, Elena, a pedagogue, achieved a good result in Oral score before and almost doubled it in the after test. This could be related to individual characteristics of the students, where one may have more ease in learning than the other, or even greater facilities or difficulties in each test.

 I will run again without Rodolpho and see if the outlier results are pulling the average.



```{r }

# Filtrar o valor 33 apenas para o grupo "Explicit"
combined_melted_data_filtered <- combined_melted_data %>%
  filter(!(variable %in% c("read. bef", "read. aft") & Group == "Explicit" & value == 33))
combined_melted_data_filtered


# Plotting boxplot for reading bef and aftr separately
ggplot(combined_melted_data_filtered %>% filter(variable %in% c("read. bef", "read. aft")), aes(x = variable, y = value, fill = Group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Reading Before and After",
       x = "Variable",
       y = "Value",
       fill = "Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

Comparing the results, we observed that the mean of `read bef` for the explicit group increased after the removal of outliers, as did the mean of `read aft` for the same group. This suggests that outliers were pulling the mean down before removal. It is noteworthy that the lowest score for the explicit group changed from 33 in reading before and after, becoming 50, and the scores tended to increase. Therefore, the removal of outliers had a positive impact on the mean scores of reading for the explicit group, indicating that outliers were negatively affecting the mean. However, there was no significant impact for the implicit group as no outliers were removed.


We will conduct the Shapiro test and Levene Test analysis, we will use the combined data to analyze the data frame and verify the hypotheses

The variable "group" reported in Levene test refers to the two explicit and implicit groups that were added to a single combined_data spreadsheet and are used to be identified in the data frame

A t-test will compare the mean oral scores after the course between two groups: 'Explicit' and 'Implicit'. A common significance level of 0.05 will be used to determine if there is a statistically significant difference between the means of these two groups.


```{r }
library(car)
model1_explicit <- lm(`Oral aft` ~ Age + `Study time (years)`  , data = explicit_group_data)
model2_explicit <- lm(`read. aft` ~ Age + `Study time (years)` , data = explicit_group_data)

model3_implicit <- lm(`Oral aft` ~ Age + `Study time (years)`  , data = implicit_group_data)
model4_implicit <- lm(`read. aft` ~ Age + `Study time (years)` , data = implicit_group_data)

# Summary of regressions
summary(model1_explicit)
summary(model2_explicit)
summary(model3_implicit)
summary(model4_implicit)
```


# New regression model without the outlier
```{r }

# Identificar e remover o outlier para o grupo explícito
explicit_group_data_filtered <- explicit_group_data[explicit_group_data$`read. aft` != 33, ]

# Criar um novo modelo de regressão sem o outlier
model1_explicit_filtered <- lm(`Oral aft` ~ Age + `Study time (years)`, data = explicit_group_data_filtered)
model2_explicit_filtered <- lm(`read. aft` ~ Age + `Study time (years)`, data = explicit_group_data_filtered)

# Resumo do modelo atualizado para verificar as mudanças
summary(model1_explicit_filtered)
summary(model2_explicit_filtered)
```



```{r }
# Shapiro-Wilk test for normality of residuals
shapiro1 <- shapiro.test(model1_explicit$residuals)
shapiro1
shapiro2 <- shapiro.test(model2_explicit$residuals)
shapiro2
shapiro3 <- shapiro.test(model3_implicit$residuals)
shapiro3
shapiro4 <- shapiro.test(model4_implicit$residuals)
shapiro4
```


# Shapiro-Wilk test for normality of residuals without the outlier
```{r }
shapirofiltered1 <- shapiro.test(model1_explicit_filtered$residuals)
shapirofiltered1
```

```{r }
summary(rstandard(model1_explicit))
summary(rstandard(model2_explicit))
summary(rstandard(model3_implicit))
summary(rstandard(model4_implicit))
```


```{r }
summary(rstandard(model1_explicit_filtered))
summary(rstandard(model2_explicit_filtered))
```

*Model 1 (Oral aft):*
- Coefficients:
  - The intercept is not statistically significant (p = 0.140).
  - Age and Study time are not statistically significant, as their p-values are very high (p > 0.05).
- The coefficient of determination (R²) is very low (0.03126), indicating that only about 3.13% of the variability in Oral aft is explained by the independent variables included in the model.
- The Shapiro-Wilk test for normality of residuals yields a p-value of 0.05821, suggesting that the residuals may not follow a normal distribution.

*Model 1 (Oral aft) – without the outlier:*

Coefficients:
The intercept is not statistically significant (p = 0.3042).
Age and Study time are now statistically significant (p = 0.0233 and p = 0.0265, respectively), indicating a significant relationship after outlier removal.
The coefficient of determination (R²) improved to 0.5749, showing that about 57.49% of the variability in Oral aft is now explained by the independent variables.
The Shapiro-Wilk test for normality of residuals yields a p-value of 0.8879, strongly suggesting that the residuals now follow a normal distribution.

*Model 2 (read. aft):*
- Coefficients:
  - The intercept is statistically significant (p < 0.001).
  - Age is statistically significant (p = 0.00934), indicating a significant relationship between age and reading performance after the study.
  - Study time is not statistically significant (p > 0.05).
- The coefficient of determination (R²) is moderate (0.5935), indicating that approximately 59.35% of the variability in reading performance can be explained by the variables included in the model.
- The Shapiro-Wilk test for normality of residuals yields a p-value of 0.1403, suggesting that the residuals may follow a normal distribution.


*Model 2 (read. aft) – without the outlier:*

Coefficients:
The intercept remains statistically significant (p = 0.00128).
Age and Study time remain not statistically significant (p = 0.61677 and p = 0.96219, respectively), showing no significant changes in their relationship to reading performance.
The coefficient of determination (R²) decreased slightly to 0.06534, suggesting a decrease in the explanatory power of the model without the outlier.
The Shapiro-Wilk test for normality of residuals yields a p-value of 0.02232, indicating that the residuals may not follow a normal distribution, pointing to potential issues in the model fit or other unmodeled influences.

*Model 3 (Oral aft for the implicit group):*
- Coefficients:
  - The intercept is not statistically significant (p = 0.0666).
  - Age and Study time are not statistically significant, as their p-values are very high (p > 0.05).
- The coefficient of determination (R²) is very low (0.1142), indicating that only about 11.42% of the variability in Oral aft for the implicit group is explained by the variables included in the model.
- The Shapiro-Wilk test for normality of residuals yields a p-value of 0.333, suggesting that the residuals may follow a normal distribution.

*Model 4 (read. aft for the implicit group):*
- Coefficients:
  - The intercept is statistically significant (p < 0.001).
  - Age is not statistically significant (p > 0.05).
  - Study time is not statistically significant (p > 0.05).
- The coefficient of determination (R²) is low (0.2627), indicating that only about 26.27% of the variability in reading performance for the implicit group can be explained by the variables included in the model.
- The Shapiro-Wilk test for normality of residuals yields a p-value of 0.7442, suggesting that the residuals may follow a normal distribution.



VIF (Variance Inflation Factor)
The VIF (Variance Inflation Factor) is a measure used to assess multicollinearity among independent variables in a linear regression model. It quantifies how much the variance of a regression coefficient is inflated due to multicollinearity with other independent variables in the model.

When the VIF is close to 1, it indicates no significant multicollinearity. Typically, VIF values greater than 5 or 10 are considered concerning, indicating substantial multicollinearity.

```{r }
vif(model1_explicit)
vif(model2_explicit)
vif(model3_implicit)
vif(model4_implicit)

vif(model1_explicit_filtered)
vif(model2_explicit_filtered)
```

*Model 1 (Oral aft):*
- VIF for Age: 1.058872
- VIF for Study time (years): 1.058872

These VIF values are close to 1, indicating that multicollinearity is not a significant concern in this model.

*Model 1 (Oral aft) – without the outlier:*

VIF for Age: 2.040987
VIF for Study time (years): 2.040987
After removing the outlier, the VIF values increased but are still below the commonly used threshold of 5, suggesting that while multicollinearity has increased, it remains within acceptable limits.

*Model 2 (read. aft):*
- VIF for Age: 1.058872
- VIF for Study time (years): 1.058872

Similar to Model 1, the VIF values for both predictors are close to 1, suggesting that multicollinearity is not a significant issue in this model as well.

*Model 2 (read. aft) – without the outlier:*

VIF for Age: 2.040987
VIF for Study time (years): 2.040987
Like Model 1 without the outlier, the removal of the outlier led to an increase in the VIF values. However, the values are still below 5, indicating that multicollinearity, although slightly increased, does not compromise the model's integrity.

*Model 3 (Oral aft for the implicit group):*
- VIF for Age: 1.672028
- VIF for Study time (years): 1.672028

While the VIF values are slightly higher in this model compared to the previous ones, they are still below 5, indicating that multicollinearity is not a major concern.

*Model 4 (read. aft for the implicit group):*
- VIF for Age: 1.672028
- VIF for Study time (years): 1.672028

Similar to Model 3, the VIF values are slightly elevated but still below 5, suggesting that multicollinearity is not a significant issue in this model.

Overall, the VIF analysis for both models before and after outlier removal indicates that multicollinearity is not a major problem in any of the regression models. This allows us to interpret the results of the predictors with confidence. Even with slight increases in the VIF values after outlier removal, they remain well within acceptable levels, ensuring that the models are not adversely affected by multicollinearity.

```{r }
par(mfrow = c(2,2))

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


```


```{r }
model_age_explicit <- lm(`read. aft` ~ Age , data = explicit_group_data)
summary(model_age_explicit)
shapiro1 <- shapiro.test(model_age_explicit$residuals)
summary(rstandard(model_age_explicit))

model_age_explicit2 <- lm(`Oral aft` ~ Age , data = explicit_group_data)
summary(model_age_explicit2)

par(mfrow = c(2,2))
plot(model_age_explicit)
text(x = 0, y = par("usr")[4], labels = paste("Shapiro p-value:", round(shapiro4$p.value, 4)), adj = c(0, 4), col = "blue")
```



```{r }
# Ajustar o layout para 2 colunas ao invés de 3
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # Adjust margins for better visualization

# Gráficos dos modelos
plot(model1_explicit)
text(x = 0, y = par("usr")[4] + 0.20 * (par("usr")[4] - par("usr")[4]), 
     labels = paste("Shapiro p-value:", round(shapiro1$p.value, 4)), 
     adj = c(0, 1), col = "blue")

plot(model2_explicit)
text(x = 0, y = par("usr")[4] + 0.20 * (par("usr")[4] - par("usr")[4]), 
     labels = paste("Shapiro p-value:", round(shapiro2$p.value, 4)), 
     adj = c(0, 1), col = "blue")

plot(model3_implicit)
text(x = 0, y = par("usr")[4] + 0.20 * (par("usr")[4] - par("usr")[4]), 
     labels = paste("Shapiro p-value:", round(shapiro3$p.value, 4)), 
     adj = c(0, 1), col = "blue")

plot(model4_implicit)
text(x = 0, y = par("usr")[4] + 0.20 * (par("usr")[4] - par("usr")[4]), 
     labels = paste("Shapiro p-value:", round(shapiro4$p.value, 4)), 
     adj = c(0, 1), col = "blue")

plot(model1_explicit_filtered)
text(x = 0, y = par("usr")[4] + 0.20 * (par("usr")[4] - par("usr")[4]), 
     labels = paste("Shapiro p-value:", round(shapirofiltered1$p.value, 4)), 
     adj = c(0, 1), col = "blue")

# Resetar para as configurações padrão
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

# Criar função para plotar e adicionar o p-valor de Shapiro
plot_with_shapiro_higher <- function(model, shapiro_result) {
  plot(model)
  text_y_position <- par("usr")[40] - 0.02 * (par("usr")[4] - par("usr")[3])
  text(x = 0, y = text_y_position, 
       labels = paste("Shapiro p-value:", round(shapiro_result$p.value, 4)), 
       adj = c(0, 1), col = "blue")
}

# Exemplo de uso da função com um dos modelos
pdf("Model_Diagnostics_Higher_Text.pdf")  # Salvar os gráficos em um arquivo PDF, um gráfico por página
plot_with_shapiro_higher(model1_explicit, shapiro1)
plot_with_shapiro_higher(model2_explicit, shapiro2)
plot_with_shapiro_higher(model3_implicit, shapiro3)
plot_with_shapiro_higher(model4_implicit, shapiro4)
plot_with_shapiro_higher(model1_explicit_filtered, shapirofiltered1)
dev.off()  # Fechar o dispositivo de plotagem


```


*Regression Model (read. aft vs. Age):*
- The estimated coefficient for Age is -1.1208, indicating that for each additional year of age, the reading performance (`read. aft`) decreases by approximately 1.1208 points.
- The intercept is estimated to be 117.8137.
- The t-value for Age is -3.181, and the associated p-value is 0.0112, indicating that Age is statistically significant in predicting reading performance.
- The multiple R-squared value is 0.5293, suggesting that approximately 52.93% of the variability in reading performance can be explained by Age.
- The residual standard error is 15.27, and the model has 9 degrees of freedom.
- The Shapiro-Wilk test for normality of residuals yields a p-value of 0.0814, suggesting that the assumption of normality may not be met.
- The standardized residuals range from -1.66580 to 1.08640, with a mean close to 0.

Overall, the regression model suggests that Age is a statistically significant predictor of reading performance (`read. aft`) in the explicit group data. However, the normality assumption of residuals may not be entirely met.


```{r }

# Assuming implicit_group_data does not contain outliers needing removal
# Combine the filtered explicit data with the unfiltered implicit data
combined_data_filtered <- rbind(explicit_group_data_filtered, implicit_group_data)


model_age_explicit <- lm(`read. aft` ~ Age , data = explicit_group_data_filtered)
summary(model_age_explicit)
shapiro1 <- shapiro.test(model_age_explicit$residuals)
summary(rstandard(model_age_explicit))

par(mfrow = c(2,2))
plot(model_age_explicit)
text(x = 0, y = par("usr")[4], labels = paste("Shapiro p-value:", round(shapirofiltered1$p.value, 4)), adj = c(0, 4), col = "blue")
```



Oral aft T-test for group Explicit and Implicit in combined_data

```{r }

# Perform Student's t-test
result_t_after <- t.test(`Oral aft` ~ group, data = combined_data)

# Summarize the results
summary(result_t_after)

# Visualize the p-value
result_t_after$p.value

```


read. aft T-test for group Explicit and Implicit in combined_data

```{r }

# Perform Student's t-test
result_t_after2 <- t.test(`read. aft` ~ group, data = combined_data)

# Summarize the results
summary(result_t_after2)

# Visualize the p-value
result_t_after2$p.value

```
```{r }

# Perform Student's t-test on 'Oral aft' comparing between groups in the filtered combined data
result_t_after_filtered1 <- t.test(`Oral aft` ~ group, data = combined_data_filtered)

# Print the summary of the t-test results
print(result_t_after_filtered1)

# Output the p-value of the test
result_t_after_filtered1$p.value
```
```{r }

# Perform Student's t-test on 'Oral aft' comparing between groups in the filtered combined data
result_t_after_filtered2 <- t.test(`read. aft` ~ group, data = combined_data_filtered)

# Print the summary of the t-test results
print(result_t_after_filtered2)

# Output the p-value of the test
result_t_after_filtered2$p.value
```

### Unfiltered Data Analysis

#### Oral aft vs. Group
- **Statistical Analysis**: A Student's t-test was performed on the `Oral aft` variable comparing the groups within the combined data.
- **Results**:
  - **p-value**: 0.0364861
  - This p-value indicates a statistically significant difference in the mean scores of `Oral aft` between the groups. The result suggests that there is sufficient evidence to reject the null hypothesis, indicating that the groups differ significantly in their mean `Oral aft` scores.

#### Read. aft vs. Group
- **Statistical Analysis**: A Student's t-test was conducted for the `read. aft` variable between the groups.
- **Results**:
  - **p-value**: 0.6900625
  - Since the p-value is greater than the conventional alpha level of 0.05, it suggests that there is no statistically significant difference in the mean scores of `read. aft` between the groups. Thus, we fail to reject the null hypothesis, suggesting that the mean scores of `read. aft` do not significantly differ between groups.

### Analysis of Filtered Data

#### Oral aft vs. Group (Filtered Data)
- **Statistical Analysis**: Another Student's t-test was performed on the `Oral aft` variable within the filtered combined data.
- **Detailed Results**:
  - **p-value**: 0.02973489
  - **t-value**: 2.4903
  - **Degrees of Freedom**: 11.163
  - **Confidence Interval**: 2.106857 to 33.674962
  - The analysis shows a significant difference in the mean scores between groups, suggesting that the mean scores for `Oral aft` in the explicit group are significantly higher than in the implicit group.

#### Read. aft vs. Group (Filtered Data)
- **Statistical Analysis**: A t-test was also repeated for the `read. aft` variable using the filtered data.
- **Detailed Results**:
  - **p-value**: 0.7270604
  - **t-value**: 0.35443
  - **Degrees of Freedom**: 18.329
  - **Confidence Interval**: -9.974115 to 14.028660
  - Similar to the unfiltered results, the p-value significantly exceeds 0.05, indicating no statistically significant difference in the mean scores of `read. aft` between the groups in the filtered dataset. The groups' mean scores for `read. aft` remain statistically indistinguishable.

### Summary and Implications

- **Significant Differences**: The analysis reveals a significant difference in `Oral aft` scores between the groups, both before and after filtering for outliers. This difference is notable, especially since it persists even after the data is cleansed of potential outliers, strengthening the evidence for a genuine difference between group means.
  
- **No Differences in Read. aft**: In contrast, the `read. aft` scores show no significant differences between the groups, both in the original and filtered datasets. This consistency across different datasets supports the conclusion that the teaching interventions or group conditions do not impact `read. aft` performance.

This comprehensive analysis using both unfiltered and filtered data provides a robust insight into how group conditions might affect different aspects of performance measured by `Oral aft` and `read. aft`. The significant results for `Oral aft` highlight the potential impact of the experimental or group conditions.



Summary of the paired t-test Oral aft and Oral bef
```{r }
result_paired_t_test_PAIRE1 <- t.test(combined_data$`Oral aft`, combined_data$`Oral bef`, paired = TRUE, alternative = "greater")

# Print the full result object
print(result_paired_t_test_PAIRE1)

# Output just the p-value
cat("P-value from the paired t-test comparing 'Oral aft' to 'Oral bef':", result_paired_t_test_PAIRE1$p.value, "\n")

```

Based on the paired t-test, you would conclude that there is insufficient evidence to claim a significant improvement in scores from Oral bef to Oral aft. The analysis indicates that while there might be an average increase, the variability and the possibility of the increase being as low as -2.06 points mean that statistically, the intervention cannot be confirmed as effective based solely on this data.

```{r }
result_paired_t_test_PAIRE2 <- t.test(combined_data$`read. aft`, combined_data$`read. bef`, paired = TRUE, alternative = "greater")

# Print the full result object
print(result_paired_t_test_PAIRE2)

# Output just the p-value
cat("P-value from the paired t-test comparing `read. aft` to `read. bef`:", result_paired_t_test_PAIRE2$p.value, "\n")

```
Based on the paired t-test, there is compelling evidence to assert that the scores for read. aft are significantly greater than those for read. bef. This analysis supports the conclusion that the intervention or change implemented between the two measurements had a positive and significant impact on reading performance. The high t-value and extremely low p-value reinforce the strength and statistical significance of this finding



Filtered paired T test
```{r }
result_paired_t_test_PAIRE_filtered1 <- t.test(combined_data_filtered$`Oral aft`, combined_data_filtered$`Oral bef`, paired = TRUE, alternative = "greater")

# Print the full result object
print(result_paired_t_test_PAIRE_filtered1)

# Output just the p-value
cat("P-value from the paired t-test comparing 'Oral aft' to 'Oral bef':", result_paired_t_test_PAIRE_filtered1$p.value, "\n")

```

The results from the paired t-test on the filtered dataset for Oral aft compared to Oral bef show a p-value of 0.08191, which, while not traditionally statistically significant, hovers near the threshold that many consider for potential significance.


```{r }
result_paired_t_test_PAIRE_filtered2 <- t.test(combined_data_filtered$`read. aft`, combined_data_filtered$`read. bef`, paired = TRUE, alternative = "greater")

# Print the full result object
print(result_paired_t_test_PAIRE_filtered2)

# Output just the p-value
cat("P-value from the paired t-test comparing 'read. aft' to 'read. bef':", result_paired_t_test_PAIRE_filtered2$p.value, "\n")

```
Based on the results of this paired t-test on the filtered data, there is overwhelming evidence to conclude that the intervention or condition implemented between the two measurements significantly improved reading performance. The increase of 14.33 points in reading scores, validated by a p-value of 0.000156, indicates a powerful effect of the intervention.

```{r }
# Perform paired t-test for the implicit group
result_paired_t_test_implicit <- t.test(implicit_group_data$`Oral aft`, implicit_group_data$`Oral bef`, paired = TRUE, alternative = "greater")

# Print the full result object
print(result_paired_t_test_implicit)

# Output just the p-value
cat("P-value from the paired t-test for the implicit group comparing 'Oral aft' to 'Oral bef':", result_paired_t_test_implicit$p.value, "\n")

# Perform paired t-test for the explicit group
result_paired_t_test_explicit_filtered <- t.test(explicit_group_data_filtered$`Oral aft`, explicit_group_data_filtered$`Oral bef`, paired = TRUE, alternative = "greater")

# Print the full result object
print(result_paired_t_test_explicit_filtered)

# Output just the p-value
cat("P-value from the paired t-test for the explicit group comparing 'Oral aft' to 'Oral bef':", result_paired_t_test_explicit_filtered$p.value, "\n")
```
```{r }
# Perform paired t-test for the implicit group on reading data
result_paired_t_test_implicit_read <- t.test(implicit_group_data$`read. aft`, implicit_group_data$`read. bef`, paired = TRUE, alternative = "greater")

# Print the full result object
print(result_paired_t_test_implicit_read)

# Output just the p-value
cat("P-value from the paired t-test for the implicit group comparing 'read. aft' to 'read. bef':", result_paired_t_test_implicit_read$p.value, "\n")

# Perform paired t-test for the explicit group on reading data
result_paired_t_test_explicit_filtered_read <- t.test(explicit_group_data_filtered$`read. aft`, explicit_group_data_filtered$`read. bef`, paired = TRUE, alternative = "greater")

# Print the full result object
print(result_paired_t_test_explicit_filtered_read)

# Output just the p-value
cat("P-value from the paired t-test for the explicit group comparing 'read. aft' to 'read. bef':", result_paired_t_test_explicit_filtered_read$p.value, "\n")
```
If we consider a significance level of 0.092 as valid, the implicit group would have a slightly more favorable result regarding the rejection of the null hypothesis. This means that, with a significance level of 0.092, there is slightly stronger evidence against the null hypothesis that there is no significant difference between the "Oral aft" and "Oral bef" scores for the implicit group compared to the explicit group. However, still, neither group achieves statistical significance at the 0.05 level. Therefore, while the implicit group may seem slightly more promising in terms of changes in oral scores before and after the course, this difference is still not statistically significant.

For reading, both the implicit and explicit groups showed significant improvements in their reading skills after the course, but the implicit group appears to have experienced an even more pronounced improvement.


# Separating data from letter-related and unrelated groups

Two distinct groups have been separated to provide a clearer distinction between individuals who have some form of contact with the Italian language or grammar-related studies, which we'll refer to as the "Related to Letters" group. The other group consists of individuals who do not have any contact with Italian grammar or languages in their profession.

```{r }

# Filtering the mentioned professions
interesting_professions <- c("Literature student", "english translator", "reviewer", "pedagogue", "Italian teacher")

# Adding an indicator column for the mentioned professional groups
combined_data$Professional_Group <- ifelse(combined_data$Profession %in% interesting_professions, "Related to Letters", "Not Related to Letters")

# Viewing the data
combined_data

```

```{r }

# Filtering the mentioned professions
interesting_professions_filtered <- c("Literature student", "english translator", "reviewer", "pedagogue", "Italian teacher")

# Adding an indicator column for the mentioned professional groups
combined_data_filtered$Professional_Group <- ifelse(combined_data_filtered$Profession %in% interesting_professions_filtered, "Related to Letters", "Not Related to Letters")

# Viewing the data
combined_data_filtered


# Filtering the mentioned professions
interesting_professions_filtered_ex <- c("Literature student", "english translator", "reviewer", "pedagogue", "Italian teacher")

# Adding an indicator column for the mentioned professional groups
explicit_group_data_filtered$Professional_Group <- ifelse(explicit_group_data_filtered$Profession %in% interesting_professions_filtered_ex, "Related to Letters", "Not Related to Letters")

# Viewing the data
interesting_professions_filtered_ex

# Filtering the mentioned professions
interesting_professions_filtered_im <- c("Literature student", "english translator", "reviewer", "pedagogue", "Italian teacher")

# Adding an indicator column for the mentioned professional groups
implicit_group_data$Professional_Group <- ifelse(implicit_group_data$Profession %in% interesting_professions_filtered_im, "Related to Letters", "Not Related to Letters")

# Viewing the data
interesting_professions_filtered_im
```

Performing a multiple linear regression Oral aft and read. aft
```{r }
# Performing a multiple linear regression Oral aft and read. aft
regression_model_multiple1 <- lm(`Oral aft` ~  Professional_Group, data = combined_data)
# Viewing the results
summary(regression_model_multiple1)

# Creating separate boxplots
boxplot(`Oral aft` ~ Professional_Group, data = combined_data, 
        main = "Oral aft by Professional Group",
        xlab = "Professional Group", ylab = "Oral aft", 
        col = c("lightblue", "lightgreen"))

# Adding subtitle
legend = c("Related to Letters", "Not Related to Letters")
       fill = c("lightblue", "lightgreen")
       
       
# Performing a multiple linear regression Oral aft and read. aft
regression_model_multiple_filtered1 <- lm(`Oral aft` ~  Professional_Group, data = combined_data_filtered)
# Viewing the results
summary(regression_model_multiple_filtered1)

# Creating separate boxplots
boxplot(`Oral aft` ~ Professional_Group, data = combined_data_filtered, 
        main = "Oral aft by Professional Group",
        xlab = "Professional Group", ylab = "Oral aft", 
        col = c("lightblue", "lightgreen"))

# Adding subtitle
legend = c("Related to Letters", "Not Related to Letters")
       fill = c("lightblue", "lightgreen")
       
       
# Performing a multiple linear regression Oral aft and read. aft
regression_model_multiple_filtered2 <- lm(`Oral aft` ~  Professional_Group, data = explicit_group_data_filtered)
# Viewing the results
summary(regression_model_multiple_filtered2)

# Creating separate boxplots
boxplot(`Oral aft` ~ Professional_Group, data = explicit_group_data_filtered, 
        main = "Oral aft by Professional Group",
        xlab = "Professional Group", ylab = "Oral aft", 
        col = c("lightblue", "lightgreen"))

# Adding subtitle
legend = c("Related to Letters", "Not Related to Letters")
       fill = c("lightblue", "lightgreen")       
       
# Performing a multiple linear regression Oral aft and read. aft
regression_model_multiple_filtered3 <- lm(`Oral aft` ~  Professional_Group, data = implicit_group_data)
# Viewing the results
summary(regression_model_multiple_filtered3)

# Creating separate boxplots
boxplot(`Oral aft` ~ Professional_Group, data = implicit_group_data, 
        main = "Oral aft by Professional Group",
        xlab = "Professional Group", ylab = "Oral aft", 
        col = c("lightblue", "lightgreen"))

# Adding subtitle
legend = c("Related to Letters", "Not Related to Letters")
       fill = c("lightblue", "lightgreen")       
              
       
```
Based on the adjusted results for a practically significant level of significance of 0.05, we conclude that there is no evidence of a practically significant association between the professional group related to letters and the "Oral aft" score in any of the analyzed data groups. In terms of proximity to significance, model 1, which considered all data without removing outliers, had the lowest p-value (0.0818), although it still did not reach the conventional significance level of 0.05. This suggests that if any of the groups were closer to demonstrating a practically significant association, it would be the group analyzed without removing outliers.



```{r }
# Perform multiple linear regression for "read. aft" and "Professional_Group"
regression_model_multiple1_read <- lm(`read. aft` ~ Professional_Group, data = combined_data)

# View the results
summary(regression_model_multiple1_read)

# Creating separate boxplots
boxplot(`read. aft` ~ Professional_Group, data = combined_data, 
        main = "Read. aft by Professional Group",
        xlab = "Professional Group", ylab = "Read. aft", 
        col = c("lightblue", "lightgreen"))

# Adding subtitle
legend = c("Related to Letters", "Not Related to Letters")
fill = c("lightblue", "lightgreen")

# Perform multiple linear regression for "read. aft" and "Professional_Group" after removing outliers
regression_model_multiple_filtered1_read <- lm(`read. aft` ~ Professional_Group, data = combined_data_filtered)

# View the results
summary(regression_model_multiple_filtered1_read)

# Creating separate boxplots
boxplot(`read. aft` ~ Professional_Group, data = combined_data_filtered, 
        main = "Read. aft by Professional Group",
        xlab = "Professional Group", ylab = "Read. aft", 
        col = c("lightblue", "lightgreen"))

# Adding subtitle
legend = c("Related to Letters", "Not Related to Letters")
fill = c("lightblue", "lightgreen")

# Perform multiple linear regression for "read. aft" and "Professional_Group" for the explicit group after removing outliers
regression_model_multiple_filtered2_read <- lm(`read. aft` ~ Professional_Group, data = explicit_group_data_filtered)

# View the results
summary(regression_model_multiple_filtered2_read)

# Creating separate boxplots
boxplot(`read. aft` ~ Professional_Group, data = explicit_group_data_filtered, 
        main = "Read. aft by Professional Group",
        xlab = "Professional Group", ylab = "Read. aft", 
        col = c("lightblue", "lightgreen"))

# Adding subtitle
legend = c("Related to Letters", "Not Related to Letters")
fill = c("lightblue", "lightgreen")

# Perform multiple linear regression for "read. aft" and "Professional_Group" for the implicit group
regression_model_multiple_filtered3_read <- lm(`read. aft` ~ Professional_Group, data = implicit_group_data)

# View the results
summary(regression_model_multiple_filtered3_read)

# Creating separate boxplots
boxplot(`read. aft` ~ Professional_Group, data = implicit_group_data, 
        main = "Read. aft by Professional Group",
        xlab = "Professional Group", ylab = "Read. aft", 
        col = c("lightblue", "lightgreen"))

# Adding subtitle
legend = c("Related to Letters", "Not Related to Letters")
fill = c("lightblue", "lightgreen")

```

 In all cases, the p-values are greater than 0.05, indicating that there is no significant relationship between the professional group related to letters and the "read. aft" score.



Oral aft T test for Professional group ("Related to Letters", "Not Related to Letters")

```{r }

# Perform Student's t-test
result_t_Professional_Group1 <- t.test(`Oral aft` ~  Professional_Group, data = combined_data)

# Summarize the results
summary(result_t_Professional_Group1)

# Visualize the p-value
result_t_Professional_Group1$p.value

```

This p-value suggests that there is a statistically significant difference in Oral aft scores between the groups at the conventional significance level of 0.05. Therefore, we have evidence to reject the null hypothesis, indicating that there is a significant difference in Oral aft scores between the groups considered.


```{r }

# Perform Student's t-test
result_t_Professional_Group_filtered1 <- t.test(`Oral aft` ~  Professional_Group, data = combined_data_filtered)

# Summarize the results
summary(result_t_Professional_Group_filtered1)

# Visualize the p-value
result_t_Professional_Group_filtered1$p.value

```
The results from the Student's t-tests, both with and without the outlier, consistently demonstrate a statistically significant difference in Oral aft performance between professional groups. This consistent finding, despite the slight alteration in statistical metrics post-outlier removal, suggests a genuine effect of professional background on oral performance capabilities. These insights are valuable for designing more effective educational tools and understanding how professional background influences learning and performance.





```{r }
# Perform Student's t-test
result_t_Professional_Group2 <- t.test(`read. aft` ~  Professional_Group, data = combined_data)

# Summarize the results
summary(result_t_Professional_Group2)

# Visualize the p-value
result_t_Professional_Group2$p.value

```
This p-value indicates that there is no statistically significant difference in read. aft scores between the groups at the conventional significance level of 0.05. Therefore, based on this result, we fail to reject the null hypothesis, suggesting that there is no significant difference in read. aft scores between the groups considered.


```{r }
# Perform Student's t-test
result_t_Professional_Group_filtered2 <- t.test(`read. aft` ~  Professional_Group, data = combined_data_filtered)

# Summarize the results
summary(result_t_Professional_Group_filtered2)

# Visualize the p-value
result_t_Professional_Group_filtered2$p.value
```

The filtered test without outliers, suggest a lack of significant differences in read. aft scores based on professional background. This finding should guide educational planners and trainers to potentially disregard professional background in Letters as a significant factor in tailoring reading interventions or assessments.

In summary, the analysis of read. aft scores across different professional groups reveals no significant impact of being related to Letters on reading performance post-intervention.

Analysis of linear regression on Oral aft and combined data Group (Implicit + Explicit)

```{r }
# (Group = Implicit + Explicit)
# Performing a linear regression on combined data
regression_model_group <- lm(`Oral aft` ~ group, data = combined_data)

# Summarizing the regression results
summary(regression_model_group)
```

Significance: The coefficient for the "groupImplicit" variable is statistically significant at the 0.05 level (p = 0.0299). This suggests that there is evidence to suggest a difference in oral aft scores between the two groups.

```{r }
# (Group = Implicit + Explicit)
# Performing a linear regression on combined data
regression_model_group_filtered1 <- lm(`Oral aft` ~ group, data = combined_data_filtered)

# Summarizing the regression results
summary(regression_model_group_filtered1)
```
Significance: The coefficient for the "groupImplicit" variable is statistically significant at the 0.05 level (p = 0.018). This suggests that there is evidence to suggest a difference in oral aft scores between the two groups, even after filtering outliers.

The regression analyses, both with and without outliers, consistently indicate a statistically significant lower performance in the Implicit group compared to the Explicit group regarding Oral aft scores. The analysis with the filtered dataset not only confirms these findings but also suggests that the effect could be slightly stronger than initially measured, with better overall model metrics (lower residuals and higher R-squared). This underlines the importance of robust outlier management to achieve a clearer understanding of the underlying dynamics between different groups.

```{r }
# (Group = Implicit + Explicit)
# Performing a linear regression on combined data
regression_model_group2 <- lm(`read. aft` ~ group, data = combined_data)

# Summarizing the regression results
summary(regression_model_group2)
```
Significance: The coefficient for the "groupImplicit" variable is not statistically significant (p = 0.689).


```{r }
# (Group = Implicit + Explicit)
# Performing a linear regression on combined data
regression_model_group_filtered2 <- lm(`read. aft` ~ group, data = combined_data_filtered)

# Summarizing the regression results
summary(regression_model_group_filtered2)
```

Significance: The coefficient for the "groupImplicit" variable is not statistically significant (p = 0.726).

The regression analyses for read. aft, both with and without outliers, consistently show that there is no statistically significant effect of group affiliation on reading scores after the intervention or condition. This finding suggests that factors other than group categorization (Implicit vs. Explicit) need to be considered when examining improvements or changes in reading performance.

These results imply that for interventions aimed at improving reading outcomes, focusing solely on the division between these groups may not be effective. Other variables, perhaps related to individual differences, instructional methods, or content-specific factors, might be more predictive of outcomes and should be explored in further studies.


*The code below performed several steps*:

1. Calculated the number of observations in two different groups: one group of professionals related to letters and another group of professionals not related to letters.
2. Determined the size of the smaller group between the two groups.
3. Randomly sampled each group to balance the sizes, ensuring that both groups had the same number of observations.
4. Conducted two separate linear regressions for the sampled groups:
   - One regression for the "Related to Letters" group.
   - Another regression for the "Not Related to Letters" group.
   
```{r }
# Count the number of observations in each group
n_related <- sum(combined_data$Professional_Group == "Related to Letters")
n_not_related <- sum(combined_data$Professional_Group == "Not Related to Letters")

n_related1 <- sum(combined_data_filtered$Professional_Group == "Related to Letters")
n_not_related1 <- sum(combined_data_filtered$Professional_Group == "Not Related to Letters")

# Determine the smaller group size
min_group_size <- min(n_related, n_not_related)

min_group_size <- min(n_related1, n_not_related1)

# Randomly sample from each group to balance the sizes
set.seed(42) # for reproducibility
data_letters_Related_sampled <- combined_data[combined_data$Professional_Group == "Related to Letters", ][sample(n_related, min_group_size), ]
data_letters_Not_sampled <- combined_data[combined_data$Professional_Group == "Not Related to Letters", ][sample(n_not_related, min_group_size), ]

# Randomly sample from each group to balance the sizes
set.seed(42) # for reproducibility
data_letters_Related_sampled1 <- combined_data_filtered[combined_data_filtered$Professional_Group == "Related to Letters", ][sample(n_related1, min_group_size), ]
data_letters_Not_sampled1 <- combined_data_filtered[combined_data_filtered$Professional_Group == "Not Related to Letters", ][sample(n_not_related1, min_group_size), ]
```


Both groups were analyzed with unfiltered and filtered datasets. Let's delve into the outcomes:

```{r }
# Regression # (Professional_Group = "Related to Letters")
# Perform linear regression
regression_model_Related_to_Letters <- lm(`Oral aft` ~ group, data = data_letters_Related_sampled)

# Summarize the regression results
summary(regression_model_Related_to_Letters)

# Regression # (Professional_Group = "Not Related to Letters")
# Perform linear regression
regression_model_Not_Related_to_Letters <- lm(`Oral aft` ~ group, data = data_letters_Not_sampled)

# Summarize the regression results
summary(regression_model_Not_Related_to_Letters)


```

```{r }
# Regression # (Professional_Group = "Related to Letters")
# Perform linear regression
regression_model_Related_to_Letters_filtered <- lm(`Oral aft` ~ group, data = data_letters_Related_sampled1)

# Summarize the regression results
summary(regression_model_Related_to_Letters_filtered)

# Regression # (Professional_Group = "Not Related to Letters")
# Perform linear regression
regression_model_Not_Related_to_Letters_filtered <- lm(`Oral aft` ~ group, data = data_letters_Not_sampled1)

# Summarize the regression results
summary(regression_model_Not_Related_to_Letters_filtered)


```

Impact of Professional Group: The "Related to Letters" analysis shows a consistent pattern where being in the Implicit group appears to significantly lower Oral aft scores, although not reaching conventional levels of statistical significance. This effect persists regardless of outlier filtering, indicating a potentially real but statistically elusive impact.
Model Robustness: The unfiltered and filtered analyses for the "Not Related to Letters" group show that outlier removal can enhance model fit and even reveal stronger effects (nearly significant in the filtered dataset), suggesting that outliers may mask underlying patterns in smaller datasets.
Statistical Significance and Practical Implications: While the changes in the Implicit group's scores are notable, they frequently fall short of statistical significance, which could be due to small sample sizes (as indicated by the degrees of freedom) or variability in the data. This highlights the need for larger sample sizes or more controlled study designs to definitively ascertain the effects.



Replicate to Read.aft

```{r }
# Regression # (Professional_Group = "Related to Letters")
# Perform linear regression
regression_model_Related_to_Letters2 <- lm(`read. aft` ~ group, data = data_letters_Related_sampled)

# Summarize the regression results
summary(regression_model_Related_to_Letters2)

# Regression # (Professional_Group = "Not Related to Letters")
# Perform linear regression
regression_model_Not_Related_to_Letters2 <- lm(`read. aft` ~ group, data = data_letters_Not_sampled)

# Summarize the regression results
summary(regression_model_Not_Related_to_Letters2)


```



```{r }
# Regression # (Professional_Group = "Related to Letters")
# Perform linear regression
regression_model_Related_to_Letters_filtered2 <- lm(`read. aft` ~ group, data = data_letters_Related_sampled1)

# Summarize the regression results
summary(regression_model_Related_to_Letters_filtered2)

# Regression # (Professional_Group = "Not Related to Letters")
# Perform linear regression
regression_model_Not_Related_to_Letters_filtered2 <- lm(`read. aft` ~ group, data = data_letters_Not_sampled1)

# Summarize the regression results
summary(regression_model_Not_Related_to_Letters_filtered2)


```


Impact of Professional Group: Both analyses across different professional backgrounds and datasets consistently show no significant impact of group affiliation (Implicit vs. Explicit) on read. aft scores. This suggests that factors other than the simple dichotomy of professional background may play more critical roles in influencing post-intervention reading outcomes.
Statistical Significance: None of the models across the datasets reached statistical significance concerning the Implicit group's effect, which underscores the need for larger sample sizes or possibly more nuanced grouping or covariate inclusion to detect smaller but potentially meaningful effects.
Model Stability and Robustness: The filtered datasets did not alter the significance or the direction of the effects observed significantly, which suggests robustness in the findings but also indicates that outliers did not substantially distort the overall analysis.



```{r }
# Regression # (Age + Study time (years))
# Perform linear regression on combined data
regression_model_Age_Study<- lm(`Oral aft` ~ Age + `Study time (years)`, data = combined_data)

# Summarize the results of the regression
summary(regression_model_Age_Study)


# Regression # (Age + Study time (years))
# Perform linear regression on combined data
regression_model_Age_Study3<- lm(`read. aft` ~ Age + `Study time (years)`, data = combined_data)

# Summarize the results of the regression
summary(regression_model_Age_Study3)

```


```{r }
# Regression # (Age + Study time (years))
# Perform linear regression on combined data
regression_model_Age_Study_filtered<- lm(`Oral aft` ~ Age + `Study time (years)`, data = combined_data_filtered)

# Summarize the results of the regression
summary(regression_model_Age_Study_filtered)


# Regression # (Age + Study time (years))
# Perform linear regression on combined data
regression_model_Age_Study_filtered3<- lm(`read. aft` ~ Age + `Study time (years)`, data = combined_data_filtered)

# Summarize the results of the regression
summary(regression_model_Age_Study_filtered3)

```

```{r }
library(ggplot2)
library(reshape2)

# Calculating the correlation matrix for selected variables in the dataset
cor_matrix <- cor(combined_data_filtered[, c("Oral aft", "read. aft", "Age", "Study time (years)")])

# Melting the correlation matrix for use with ggplot2
cor_matrix_melted <- melt(cor_matrix)

# Creating the heatmap with annotations for all correlation coefficients
ggplot(cor_matrix_melted, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +  # Adds tiles for heatmap
  geom_text(aes(label = sprintf("%.2f", value)),  # Displays all correlation coefficients with two decimal points
            size = 3.5, color = "black") +  # Adjust text size as needed
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       limit = c(-1, 1), name = "Correlation") +  # Color gradient from blue to red
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),  # Adjust text angle and position for x-axis labels
        axis.text.y = element_text(hjust = 1)) +
  labs(x = "Variables", y = "Variables", title = "Correlation Heatmap") +
  coord_fixed()  # Ensures the tiles are square



```


Impact of Outliers: The comparison between the filtered and unfiltered datasets shows minimal changes in the statistical significance of the predictors across both outcomes (Oral aft and read. aft). This suggests that outliers do not dramatically impact the overall results or interpretations of these particular models.
Effectiveness of Predictors: Across all models, neither Age nor Study time (years) is a significant predictor of post-intervention outcomes for Oral aft or read. aft. This indicates that other factors, not included in the model, might be influencing these outcomes more substantially. These could include variables related to the specific nature of the interventions, individual differences in baseline capabilities, or other environmental and educational factors.
Statistical Power and Model Fit: The consistently low R-squared values across all models suggest that the models have limited power in explaining the variability in the scores. This could be due to the small sample size (indicated by the low degrees of freedom), which also raises concerns about the models' overall robustness and the generalizability of the results.


```{r }
# Regression # (Oral aft + Have you traveled to Italy?)
# Perform linear regression on combined data
regression_model_ht1<- lm(`Oral aft` ~ `Have you traveled to Italy?` , data = combined_data)

# Summarize the results of the regression
summary(regression_model_ht1)


# Regression # (read aft + Have you traveled to Italy?)
# Perform linear regression on combined data
regression_model_ht2<- lm(`read. aft` ~ `Have you traveled to Italy?` , data = combined_data)

# Summarize the results of the regression
summary(regression_model_ht2)

# Convertendo a variável categórica em formato numérico
combined_data_filtered$traveled_to_italy_numeric <- ifelse(combined_data_filtered$`Have you traveled to Italy?` == "Yes", 1, 0)

# Adicionando a variável à matriz de correlação
cor_matrix <- cor(combined_data_filtered[, c("Oral aft", "read. aft", "Age", "Study time (years)", "traveled_to_italy_numeric")])
cor_matrix_melted <- melt(cor_matrix)

# Criando o heatmap com anotações
ggplot(cor_matrix_melted, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(abs(value) > 0.5, ifelse(value > 0, "Positive", "Negative"), "")), 
            size = 3, color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Variables", y = "Variables", title = "Correlation Heatmap") +
  coord_fixed()


```


*Regression Model (Oral aft vs. Have you traveled to Italy?)*
- The intercept coefficient is estimated to be 15.917 with a standard error of 5.185 and a statistically significant t-value of 3.07 (p = 0.00605). This indicates that the mean Oral aft score for individuals who have not traveled to Italy is 15.917.
- The coefficient for "Yes" in the variable "Have you traveled to Italy?" is estimated to be 4.383 with a standard error of 7.691 and a non-significant t-value of 0.57 (p = 0.57508). This suggests that there is no significant difference in Oral aft scores between individuals who have traveled to Italy and those who haven't.
- The adjusted R-squared value is -0.03322, indicating that the model does not explain much of the variance in Oral aft scores.
- The F-statistic has a value of 0.3248 with a p-value of 0.5751, indicating that the overall model is not statistically significant.

*Regression Model (read. aft vs. Have you traveled to Italy?)*
- The intercept coefficient is estimated to be 87.500 with a standard error of 4.941 and a statistically significant t-value of 17.709 (p < 0.001). This indicates that the mean read. aft score for individuals who have not traveled to Italy is 87.500.
- The coefficient for "Yes" in the variable "Have you traveled to Italy?" is estimated to be -6.000 with a standard error of 7.329 and a non-significant t-value of -0.819 (p = 0.423). This suggests that there is no significant difference in read. aft scores between individuals who have traveled to Italy and those who haven't.
- The adjusted R-squared value is -0.01595, indicating that the model does not explain much of the variance in read. aft scores.
- The F-statistic has a value of 0.6702 with a p-value of 0.4226, indicating that the overall model is not statistically significant.

```{r }

# Create a new column called 'Study Context Category'
combined_data$Study_context_category <- ifelse(combined_data$`Study context` == "university", "University", "Not University")

# Check if the new column was added correctly
head(combined_data)

# Create a new column called 'Study Context Category'
combined_data_filtered$Study_context_category <- ifelse(combined_data_filtered$`Study context` == "university", "University", "Not University")

# Check if the new column was added correctly
head(combined_data_filtered)
```

```{r }
# Regression # (Oral aft + Study context)
# Perform linear regression on combined data
regression_model_Study1<- lm(`Oral aft` ~ `Study_context_category` , data = combined_data)

# Summarize the results of the regression
summary(regression_model_Study1)

# Regression # (`read. aft`+ Study context)
# Perform linear regression on combined data
regression_model_Study2<- lm(`read. aft` ~ `Study_context_category` , data = combined_data)

# Summarize the results of the regression
summary(regression_model_Study2)
```

```{r }
# Regression # (Oral aft + Study context)
# Perform linear regression on combined data
regression_model_Study_filtered<- lm(`Oral aft` ~ `Study_context_category` , data = combined_data_filtered)

# Summarize the results of the regression
summary(regression_model_Study_filtered)

# Regression # (`read. aft`+ Study context)
# Perform linear regression on combined data
regression_model_Study_filtered<- lm(`read. aft` ~ `Study_context_category` , data = combined_data_filtered)

# Summarize the results of the regression
summary(regression_model_Study_filtered)
```

*Regression Model (Oral aft vs. Study context)*
- The intercept coefficient is estimated to be 16.273 with a standard error of 5.435 and a statistically significant t-value of 2.994 (p = 0.00717). This indicates that the mean Oral aft score for individuals in the "Not University" category is 16.273.
- The coefficient for "University" in the variable "Study_context_category" is estimated to be 3.273 with a standard error of 7.686 and a non-significant t-value of 0.426 (p = 0.67481). This suggests that there is no significant difference in Oral aft scores between individuals in the "University" category and those in the "Not University" category.
- The adjusted R-squared value is -0.04057, indicating that the model does not explain much of the variance in Oral aft scores.
- The F-statistic has a value of 0.1813 with a p-value of 0.6748, indicating that the overall model is not statistically significant.

*Regression Model (read. aft vs. Study context)*
- The intercept coefficient is estimated to be 86.182 with a standard error of 5.228 and a statistically significant t-value of 16.486 (p < 0.001). This indicates that the mean read. aft score for individuals in the "Not University" category is 86.182.
- The coefficient for "University" in the variable "Study_context_category" is estimated to be -2.818 with a standard error of 7.393 and a non-significant t-value of -0.381 (p = 0.707). This suggests that there is no significant difference in read. aft scores between individuals in the "University" category and those in the "Not University" category.
- The adjusted R-squared value is -0.04243, indicating that the model does not explain much of the variance in read. aft scores.
- The F-statistic has a value of 0.1453 with a p-value of 0.7071, indicating that the overall model is not statistically significant.


