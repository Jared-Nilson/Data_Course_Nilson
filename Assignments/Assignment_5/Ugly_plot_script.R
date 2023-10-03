library(tidyverse)
iris1 <- ggplot(iris,aes(x=Petal.Length, 
                y=Petal.Width, 
                color=Species)) +
 
   geom_point(alpha=.75, size = .25)+
  geom_smooth(method = lm, se = FALSE) +
   geom_smooth(method = lm, color = "black", linetype=2) +
  theme(
    
    plot.background = element_rect(fill = "lightyellow"),
    plot.title = element_text(size = 30, hjust = 0.25),
    plot.subtitle = element_text(size = 20, hjust = 0.75, color = "mediumvioletred", family = "serif"),
    plot.caption = element_text(size = 10, face = "italic", angle = 25),
    
    panel.background = element_rect(fill = '#3F1F88', colour = 'darkred', size = 4),
    panel.border = element_rect(fill = NA, color = "green", size = 2),
    panel.grid.major.x = element_line(color = "purple", linetype = 2),
    panel.grid.minor.x = element_line(color = "orange", linetype = 3),
    panel.grid.minor.y = element_blank(),
    
    axis.title.x = element_text(face = "bold.italic", color = "blue"),
    axis.title.y = element_text(family = "mono", face = "bold", size = 20, hjust = 0.25),
    axis.text = element_text(face = "italic", size = 15),
    axis.text.x.bottom = element_text(angle = 180), # note that axis.text options from above are inherited
    
    strip.background = element_rect(fill = "magenta"),
    strip.text.y = element_text(color = "white"),
    strip.placement = "outside",
    
    legend.background = element_rect(fill = "#FF00BD"), # generally will want to match w plot background
    legend.key = element_rect(fill = "orange"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.justification = "left",
    legend.title = element_text(family = "serif", color = "white"),
    legend.text = element_text(family = "mono", face = "italic", color = "limegreen")
    
  )
iris1
devtools::install_github('thomasp85/patchwork')
#mapping column names. after giving the ggplot a data set, 
#then give it geoms (things to draw
install.packages('Rtools')
glimpse(mpg)
mpg
mpg1<-ggplot(mpg,aes(x=cyl, y=hwy, color = year)) +
geom_point(size = 2) +
facet_wrap("drv", scales ='free') +  theme(
  
  plot.background = element_rect(fill = "lightyellow"),
  plot.title = element_text(size = 30, hjust = 0.25),
  plot.subtitle = element_text(size = 20, hjust = 0.75, color = "mediumvioletred", family = "serif"),
  plot.caption = element_text(size = 10, face = "italic", angle = 25),
  
  panel.background = element_rect(fill = '#3F1F88', colour = 'darkred', size = 4),
  panel.border = element_rect(fill = NA, color = "green", size = 2),
  panel.grid.major.x = element_line(color = "purple", linetype = 2),
  panel.grid.minor.x = element_line(color = "orange", linetype = 3),
  panel.grid.minor.y = element_blank(),
  
  axis.title.x = element_text(face = "bold.italic", color = "blue"),
  axis.title.y = element_text(family = "mono", face = "bold", size = 20, hjust = 0.25),
  axis.text = element_text(face = "italic", size = 15),
  axis.text.x.bottom = element_text(angle = 180), # note that axis.text options from above are inherited
  
  strip.background = element_rect(fill = "magenta"),
  strip.text.y = element_text(color = "white"),
  strip.placement = "outside",
  
  legend.background = element_rect(fill = "#FF00BD"), # generally will want to match w plot background
  legend.key = element_rect(fill = "orange"),
  legend.direction = "horizontal",
  legend.position = "bottom",
  legend.justification = "left",
  legend.title = element_text(family = "serif", color = "white"),
  legend.text = element_text(family = "mono", face = "italic", color = "limegreen")
  
)
mpg1
install.packages('ggplot2')
install.packages('ggimage')
install.packages('palmerpenguins')
install.packages('colorblindr')
install.packages('devtools')
install.packages('GGally')
library(tidyverse)
library(colorblindr)
library(palmerpenguins)
library(devtools)
library(ggimage)
mpg1
pal = c("#6DF9EA","#88109B","#530F0F","#5D5D5D","#00FF00","#B0F54C","#B0F54C","#6D6508","#EBBA85","#894e7d","#a17fc1","#262a8e","#abb5b5")


p2 <- iris1
  
p2

glimpse(penguins)
library(patchwork)
#find functions with ::
#plots everything against everything
p3 <- GGally::ggpairs(penguins) 

penguins %>% names

p <- penguins %>% 
  ggplot(aes(x= species, y=flipper_length_mm, color = species )) + 
  scale_color_manual(values=pal)+
  geom_point(size = 1.5) +
  geom_violin(fill = "#FFF300", aes(lwd = 1.5))+
  facet_wrap(~ island, scales= 'free') + 
  theme(legend.position = 'bottom',
        strip.text.x = element_text(size = 12, face="bold"),
        strip.background = element_rect(fill = "Red")) +
  theme(
    
    plot.background = element_rect(fill = "lightyellow"),
    plot.title = element_text(size = 30, hjust = 0.25),
    plot.subtitle = element_text(size = 20, hjust = 0.75, color = "mediumvioletred", family = "serif"),
    plot.caption = element_text(size = 10, face = "italic", angle = 25),
    
    panel.background = element_rect(fill = '#3F1F88', colour = 'darkred', size = 4),
    panel.border = element_rect(fill = NA, color = "green", size = 2),
    panel.grid.major.x = element_line(color = "purple", linetype = 2),
    panel.grid.minor.x = element_line(color = "orange", linetype = 3),
    panel.grid.minor.y = element_blank(),
    
    axis.title.x = element_text(face = "bold.italic", color = "blue"),
    axis.title.y = element_text(family = "mono", face = "bold", size = 20, hjust = 0.25),
    axis.text = element_text(face = "italic", size = 15),
    axis.text.x.bottom = element_text(angle = 180), # note that axis.text options from above are inherited
    
    strip.background = element_rect(fill = "magenta"),
    strip.text.y = element_text(color = "white"),
    strip.placement = "outside",
    
    legend.background = element_rect(fill = "#FF00BD"), # generally will want to match w plot background
    legend.key = element_rect(fill = "orange"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.justification = "left",
    legend.title = element_text(family = "serif", color = "white"),
    legend.text = element_text(family = "mono", face = "italic", color = "limegreen")
    
  ) +
  labs(title = "oopsy poopsy",
       subtitle = "boing boing bong bing",
       x = "my x axis",
       y = "my y axis",
       caption = "Banananananananananananannananananna",
       col = "DEAD CHILDREN") 
 
  
  
   theme(axis.title.x = element_text(face= 'bold',
                                    size = 30,
                                    vjust = 3.5,
                                    angle = 45),
        legend.title = element_text(color = '#FF00BD'),
        legend.background = element_rect(fill= 'yellow', size = 50, color = '#FF00BD'))
p
   
p + p2 

(p | mpg1 /p2)
   
   
   
is.na(df$x)
