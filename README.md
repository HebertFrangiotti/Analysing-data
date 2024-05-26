---
title: "Análise de Desempenho dos Grupos Explícito e Implícito"
author: "Hebert"
output: html_document
---

knitr::opts_chunk$set(echo = TRUE)
library(readxl)
library(ggplot2)
library(reshape2)
library(GGally)
library(dplyr)
library(car)

file_path <- "C:\\Users\\Admin\\Downloads\\Data_performance.xlsx"
explicit_group_data <- read_excel(file_path, sheet = "Explicit group")
implicit_group_data <- read_excel(file_path, sheet = "Implicit group")



# Renomeando colunas para explicit_group_data
colnames(explicit_group_data) <- c("Score sel.", "read. bef", "read. aft", "aspects bef", "aspects aft", "Oral bef", "Oral aft")
# Renomeando colunas para implicit_group_data
colnames(implicit_group_data) <- c("Score sel.", "read. bef", "read. aft", "aspects bef", "aspects aft", "Oral bef", "Oral aft")



