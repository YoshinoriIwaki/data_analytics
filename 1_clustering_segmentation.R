#ex1
library(data.table)
library(foreach)
library(dplyr)

click.data <- as.data.frame(fread("click_data_sample.csv"))
head(click.data)

#ex2
click.user.data <- 
  click.data %>%
  group_by(user.id) %>%
  summarize(click.num = length(click.at)) %>%
  as.data.frame() %>%
  arrange(desc(click.num)) %>%
  mutate(no = 1:length(user.id))

head(click.user.data)

#ex3
library(ggplot2)
ggplot(click.user.data, aes(x=no, y=click.num)) + geom_line() + geom_point() + xlab("user") + ylab("Clicks") + theme_bw()