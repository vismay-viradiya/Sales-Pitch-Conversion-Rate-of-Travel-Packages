library(ggplot2)
library(readxl)
library(gplots)


df = read_excel("C:/Users/vbvir/Desktop/MPS ANALYTICS/QUARTER 2 APR-JUN/ALY 6070 COMMUNICATION AND VISUALIZATION IN ANALYTICS/DATASET/Tourism.xlsx",sheet = 2)
head(df)

colnames(df)


#========================FIRST GRAPH========================
# Create a table of value counts
product_taken_count <- table(df$ProdTaken)

# Convert the table to a data frame
df_filtered <- as.data.frame(product_taken_count)

# Rename the values
df_filtered$Var1 <- ifelse(df_filtered$Var1 == "0", "No", "Yes")

# Print the modified table
df_filtered

# Calculate percentage values
total <- sum(df_filtered$Freq)
df_filtered$Percentage <- df_filtered$Freq / total * 100
# Create custom colors
colors = c("lightcoral","lightgreen")
# Create a bar plot with custom colors and percentage values
colors2 = c("darkred","darkgreen")

# Create the bar plot with hidden y-axis
barplot(df_filtered$Percentage, 
        names.arg = df_filtered$Var1,
        col = colors,
        main = "Portions of successful product sell", 
        xlab = "Taken",
        ylim = c(0, 100), 
        yaxt = "n", 
        border = NA,cex.main = 1.5)
par(yaxt = "n")
# Add labels inside the bars with custom color
text(x = barplot(df_filtered$Percentage,
                 col = colors, 
                 add = TRUE,
                 border = NA), 
     y = df_filtered$Percentage, 
     labels = paste0(round(df_filtered$Percentage, 1), "%"), 
     cex = 2, col = colors2, pos = 3)



#========================SECOND GRAPH========================

# Create a table of value counts
product_taken_count2 <- table(df$ProdTaken,df$PitchSatisfactionScore)

percentage_col <- prop.table(product_taken_count2, margin = 2) * 100

percentage_col = percentage_col[2,]

# Convert the table to a data frame
percentage_df <- data.frame(Column = as.numeric(names(percentage_col)),
                            Percentage = as.numeric(percentage_col))

ggplot(percentage_df, aes(x = Column, y = Percentage)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), 
            vjust = -0.5, size = 4) +
  xlab("Pitch Satisfaction") +
  ylab("% Success of product getting sold") +
  ggtitle("Relationship Between Pitch Satisfaction 
          and Success of Product Sales") +
  ylim(0, 30) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line())



#========================THIRD GRAPH========================
# Assuming your dataset is named 'df'
# Creating age bins
df$AgeBin <- cut(df$Age,
                 breaks = c(0, 19, 24, 29, 34, 39, 44, 49, 54, 59, Inf),
                 labels = c("Teenagers", "Early 20s", "Late 20s", "Early 30s", "Late 30s",
                            "Early 40s", "Late 40s", "Early 50s", "Late 50s", "Elders"),
                 include.lowest = TRUE)

# View the updated dataset with age bins
head(df)

age_counts <- table(df$AgeBin, df$ProdTaken)
age_counts <- cbind(age_counts, prop.table(age_counts, margin = 1) * 100)
age_counts

average_income_age <- aggregate(MonthlyIncome ~ AgeBin, data = df, FUN = mean)


# Create the plot of means without y-axis tick marks
plotmeans(df$MonthlyIncome ~ df$AgeBin, data = df, xlab = "Age Group", 
          ylab = "Average Income", main = "Average Income by Age Group", 
          cex.axis = 0.8,n.label = FALSE)

# Add custom y-axis ticks and labels
yticks <- seq(0, 10000, by = 2000)
ylabels <- paste0(yticks, "%")
axis(side = 2, at = yticks, labels = ylabels)

# Add annotations for percentage of 1 from age_counts
age_counts <- table(df$AgeBin, df$ProdTaken)
age_counts <- cbind(age_counts, prop.table(age_counts, margin = 1) * 100)

ticks <- age_counts[,"1"]
age_labels <- row.names(age_counts)

text(x = rep(1, length(ticks)), y = ticks, labels = paste0(ticks, "%"), pos = 3, col = "red")
