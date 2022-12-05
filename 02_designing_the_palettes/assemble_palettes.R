source("02_designing_the_palettes/assemble_functions/assemble_functions.R")

# display all
display_all()

# make example plots to check usability

library(palmerpenguins)
library(ggplot2)
library(dplyr)
library("gridExtra")

head(penguins)

ggplot(penguins, aes(x = bill_length_mm, y = flipper_length_mm))+
  theme_bw()+
  geom_point(size = 3, aes(color = species))

head(diamonds)

names(GrisPalettes[1])

grisplots <- list()

i <- 17

for (i in 1:length(GrisPalettes)) {
  
pal <- grisbrewer(names(GrisPalettes[i]))

p <- diamonds %>%
    filter(color == "E") %>%
    sample_n(., 200, seed = 13) %>%
    ggplot(., aes(x = clarity , y = depth))+
    theme_bw()+
    geom_point(size = 5, shape = 21, aes(fill = cut), position = position_jitter(width = 0.2))+
    scale_fill_manual(values = pal)+
    ylim(55,65)+
    ggtitle(names(GrisPalettes[i]))

print(p)

}


