#install.packages("gapminder")
#install.packages("tidyverse")
library(gapminder)
library(tidyverse)
data(gapminder)

str(gapminder)
summary(gapminder)
colSums(is.na(gapminder))
