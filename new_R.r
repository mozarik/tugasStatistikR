csv <- read.csv('C:/Users/zein/Belajar R/multipleChoiceResponses.csv')
df <- as.data.frame(csv)

head(csv)

library('dplyr')
df <- df %>% select(
    gender    = Q1,
    age       = Q2,
    major     = Q5)

# Class definition
num <- setRefClass(
    "num", # Name of referenced class
    #------- Attributes definition -------#
    fields = list(df = "data.frame", var_num = "data.frame"), # Fields used in the method
    methods = list( # Methods definition
        group_values = function(var){ # Method name (in fact, it's a function)
            var_num <<- df %>% count(df[[var]]) # Create a resume of the attribute name
            colnames(var_num) <<- c(var, "n") # Replace the name of the columns
            var_num <<- var_num %>% filter(n > 2) # Remove the first option of the data frame
        }
    )
)

# This is the constructor, we pass the values for field declared in the class definition.
num_father <- num$new(df = df)
# We start the preparation for all interested variables
for (col_name in colnames(df)){
    assign(paste("num.", col_name, sep = ""), num_father$group_values(col_name))
}

library(plotly)
library(ggplot2)

bar_age <- plot_ly(
        num.age,
        name = "Age",
        x = ~ age,
        y = ~ n,
        type = 'bar',
        marker = list(
            color = 'rgb(158,456,225)',
            line = list(color = 'rgb(55,123,69)',
                        width = 1.5)
        )
) %>% layout (
        title = 'Ages',
        xaxis = list(title = ""),
        yaxis = list(title = "")
)
box_age <- plot_ly(
        num.age,
        name = "Age",
        x = ~ n,
        type = 'box'
)
subplot(bar_age)

plot_ly(num.gender,
        labels = ~ gender,
        values = ~ n,
        type = 'pie') %>%
layout(
    title = "Gender",
    xaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    ),
    yaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    )
  )


