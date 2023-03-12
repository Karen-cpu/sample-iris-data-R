library(magrittr)
library(dplyr)

data(iris)
iris %>% summary

tibble(iris)

#picking a column
iris %>% tibble %>% select(Petal.Width) %>% head(5)

#picking several columns
iris %>% tibble %>% select(Species, Petal.Width) %>% head(5)

iris %>% tibble %>% select(starts_with("Petal")) %>% head(5)

#add value to dataframe
iris %>% tibble %>% mutate(Petal.WL = Petal.Width + Petal.Length) %>%
  select(Species, Petal.WL) %>% head(5)

#add more columns
iris %>% tibble %>% 
  mutate (Petal.WL = Petal.Width + Petal.Length,
          Sepal.WL = Sepal.Width + Sepal.Length) %>% 
  select(Species,Petal.WL, Sepal.WL) %>%
  head(5)

#reorder dataframe
iris %>% tibble %>% arrange(desc(Sepal.Length)) %>% head(5)


#iris with sepal length greater than 5
iris %>% tibble %>%
  filter(Sepal.Length > 5 & Species == "setosa") %>%
  select(Species, Sepal.Length) %>%
  head(5)

#group by species 
iris %>% tibble %>% group_by(Species) %>% 
  summarise(Mean.Petal.Length = mean(Petal.Length))

#number of observations

iris %>% group_by(Species) %>% 
  summarise(observations = n())

#boxplot
library(ggplot2)

iris %>% select(Species, Petal.Width) %>%
  qplot(Species, Petal.Width, geom = "boxplot", data = . )

#plotting attributes against dataset
library(tidyr)
iris %>% gather(key = Attribute, value = Measurement,
                Petal.Length, Petal.Width)%>%
  select(Species, Attribute, Measurement) %>%
  qplot(Attribute, Measurement, geom = "boxplot", facets = . ~ Species, data = .)

iris %>% qplot(Petal.Width, Petal.Length , color = Species, data = .)

iris %>% gather(Measurement, Value, -Species) %>%
  ggplot(aes(x = Species, y = Value)) +
  geom_boxplot() +
  facet_grid(Measurement ~ . )

iris %>% ggplot(aes(x = Species, y = Petal.Length)) +
  geom_boxplot() + geom_jitter(width = 0.1, height = 0.1)


label_map <- c(Petal.Width = "Petal Width",
               Petal.Length = "Petal Length",
               Sepal.Width = "Sepal Width",
               Sepal.Length = "Sepal Length")

iris %>% gather(Measurement, Value, -Species) %>%
  ggplot(aes(x = Species, y = Value, fill = Species)) +
  geom_boxplot() +
  facet_grid(Measurement ~ ., scale = "free_y",
             labeller = labeller(Measurement = label_map)) +
  theme(strip.background = element_blank()) +   theme(legend.position="top")
  
#multiple plots
petal <- iris %>% ggplot() +
  geom_point(aes(x = Petal.Width, y = Petal.Length,
                 color = Species)) +
  theme(legend.position="none")
sepal <- iris %>% ggplot() +
  geom_point(aes(x = Sepal.Width, y = Sepal.Length,
                 color = Species)) +
  theme(legend.position="none")

library(gridExtra)

grid.arrange(petal, sepal, ncol = 2)

library(cowplot)

plot_grid(petal, sepal, labels = c("A", "B"))
