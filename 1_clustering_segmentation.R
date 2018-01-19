#ex1
library(data.table)
library(foreach)
library(dplyr)

click.data <- as.data.frame(fread("./R/data_analytics/click_data_sample.csv"))
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

#ex4
ggplot(click.user.data, aes(x=no, y=click.num)) + geom_line() + geom_point() + xlab("user") + ylab("Clicks") + theme_bw()

#ex5
library(ykmeans)
click.user.data <-
  ykmeans(click.user.data, "click.num", "click.num", 3)
table(click.user.data$cluster)

#ex6
ggplot(click.user.data[1:5000,], aes(x=no, y=click.num, col=as.factor(cluster), 
                                     shape=as.factor(cluster))) + 
  geom_line() +
  geom_point() +
  xlab("user") +
  ylab("Clicks") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Paired")

#ex7
target.click.user <- 
  click.user.data %>%
  filter(cluster >= 2)

click.data <-
  click.data %>%
  filter(user.id %in% target.click.user$user.id)

#ex8
click.data.campaign <-
  click.data %>%
  group_by(user.id, campaign.id) %>%
  summarise(click.num=length(click.at)) %>%
  as.data.frame()

click.data.cast <- as.data.frame(
  dcast.data.table(data = as.data.table(click.data.campaign),
                   formula = user.id~campaign.id,
                   value.var = "click.num",
                   fun.aggregate = sum)
  )

click.data.cast$total <- rowSums(click.data.cast[, -1])
head(click.data.cast, 2)

)