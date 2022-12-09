
library(palmerpenguins)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggstream)

source("02_designing_the_palettes/assemble_functions/assemble_functions.R")

# display all
display_all()

# make example plots to check usability


head(penguins)

ggplot(penguins, aes(x = bill_length_mm, y = flipper_length_mm))+
  theme_bw()+
  geom_point(size = 3, aes(color = species))

# run all palettes through scatter plots to check colors are distinguishable

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

# make stream graph examples

# Library

for (i in 1:length(GrisPalettes)) {
  
streampal <- grisbrewer(names(GrisPalettes[i]))

p <- ggplot(blockbusters, aes(x = year, y = box_office, fill = genre)) +
  geom_stream(extra_span = 0.2) +
  geom_stream(extra_span = 0.2, true_range = "none") +
  scale_fill_manual(values = streampal) +
  theme_void()+
  theme(legend.position = "", 
        plot.title = element_text(size = 40, face = "italic", family = "serif", hjust = 0.5, vjust = -2))+
  ggtitle(names(GrisPalettes[i]))

print(p)

}

# very good palettes for stream graph so far: Starlight, DoubleJump
# good: Healing, Cloudpath, Lightguide, Turtlelight, Gris
# mayber reorder: Flowerbridge, Lightguide, TunnelChase, GiantTree, DesertPath, Denial (try putting lightest or darkest in middle)
# notes: healing grey should be lighter, Gris more saturated, grey in Turtle light needs to be lighter
# notes: Forestfriend greens clash and orange is ugly, middle Brown in Windswept is ugly


# Gris streamgraph

streampal <- grisbrewer(names(GrisPalettes[1]))

ggplot(blockbusters, aes(x = year, y = box_office, fill = genre)) +
  geom_stream(extra_span = 0.2) +
  geom_stream(extra_span = 0.2, true_range = "none") +
  scale_fill_manual(values = streampal) +
  theme_void()+
  theme(legend.position = "", 
        plot.title = element_text(size = 40, face = "italic", family = "serif", hjust = 0.5, vjust = -2))+
  ggtitle(names(GrisPalettes[1]))


