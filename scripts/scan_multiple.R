---
title: "MaySCAN"
format: revealjs
editor: visual
---

library(googledrive)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(tidyr)
library(readxl)
library(lubridate)


#############################
## Import & Visualize Data ##
#############################
# Load data from Google drive
scan <- googledrive::as_id("https://drive.google.com/drive/folders/1DZktlQUHaot_r4e_fD9ip6zcxHWqslMP")
# List all CSV files in the folder
scan_csvs <- googledrive::drive_ls(path = scan)


# Create empty list to store data frames
scan_list <- list()

# Loop over each file in the `scan_csvs` data frame
for (i in seq_along(scan_csvs$id)) {
  # Define the local file path
  local_path <- file.path("googledrive", scan_csvs$name[i])
  
  # Download the file
  googledrive::drive_download(
    file = scan_csvs$id[i],
    path = local_path,
    overwrite = TRUE
  )
  
  # Read the header row (row 2)
  header <- read_excel(local_path, skip = 1, n_max = 1, col_names = FALSE)
  # Convert the header to a character vector and clean empty names
  col_names <- as.character(unlist(header[1, ]))
  col_names[col_names == ""] <- paste0("X", seq_along(col_names[col_names == ""]))
  
  # Read the data starting from row 4 using the header as column names
  data <- read_excel(local_path, skip = 4, col_names = col_names)
  
  # Store the data in the list
  scan_list[[scan_csvs$name[i]]] <- data
}


##############
## Cleaning ## 
##############
# Rename columns and change to values to numeric
# Loop through each data frame in the list
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  
  # Convert values to numeric and rename
  df$DOC <- as.numeric(df$`DOCeq [mg/l] - Measured value`)
  df$NO3N <- as.numeric(df$`NO3-Neq [mg/l] - Measured value`)
  df$NO3 <- as.numeric(df$`NO3eq [mg/l] - Measured value`)
  df$TOC <- as.numeric(df$`TOCeq [mg/l] - Measured value`)
  df$TSS <- as.numeric(df$`TSSeq [mg/l] - Measured value`)
  df$Temp <- as.numeric(df$`Temperature_19 [°C] - Measured value`)
  
  df$DateTime <- df$`Parameter:`
  
  # Update the data frame in the list
  scan_list[[i]] <- df
}

##############
## Plotting ##
##############

### DOC ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = DOC)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  ggsave(paste0("scan_figs/DOC_", scan_csvs$name[i], ".png"))
}

### NO3 ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = NO3)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  ggsave(paste0("scan_figs/NO3_", scan_csvs$name[i], ".png"))
}

### NO3N ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = NO3N)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  ggsave(paste0("scan_figs/NO3N_", scan_csvs$name[i], ".png"))
}

### TOC ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = TOC)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  ggsave(paste0("scan_figs/TOC_", scan_csvs$name[i], ".png"))
}

### TSS ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = TSS)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  ggsave(paste0("scan_figs/TSS_", scan_csvs$name[i], ".png"))
}

### Temp ###
for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df, aes(x = DateTime, y = Temp)) + 
    geom_line() + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    ggtitle(paste(scan_csvs$name[i])) +
    theme(axis.text.x = element_text(angle=45))
  ggsave(paste0("scan_figs/Temp_", scan_csvs$name[i], ".png"))
}

#######################
## Plot all together ##
#######################

for (i in seq_along(scan_list)) {
  # Access the current data frame
  df <- scan_list[[i]]
  # Plot
  p <- ggplot(data = df) + 
    geom_line(aes(x=DateTime, y=df$Temp, color='Temperature')) +
    geom_line(aes(x=DateTime, y=df$TSS, color='TSS')) +
    geom_line(aes(x=DateTime, y=df$TOC, color='TOC')) +
    geom_line(aes(x=DateTime, y=df$NO3N, color='NO3-N')) +
    geom_line(aes(x=DateTime, y=df$NO3, color='NO3')) +
    geom_line(aes(x=DateTime, y=df$DOC, color='DOC')) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    theme(axis.text.x = element_text(angle=45)) +
    ylab("Measured")
  ggsave(paste0("scan_figs/Measured_", scan_csvs$name[i], ".png"))
}
