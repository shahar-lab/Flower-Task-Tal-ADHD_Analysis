library(ggpubr)
mytheme=
  theme_pubclean()+
  theme(panel.border   = element_blank(), 
        axis.line      = element_line(color='gray'),
        text           = element_text(size=14,  family="serif"),
        axis.title     = element_text(size=14),
        legend.position= "right",
        plot.title     = element_text(hjust = 0.5))
