
################################################

rm(list=ls())
library(ggplot2)
library(cowplot)
marginal_distribution <- function(x, var) {
  ggplot(x, aes_string(x = var)) +
    geom_density(position = "identity", color=NA,
                   fill="blue",alpha=.5) +
    theme_void() +
    theme(plot.margin = margin())
}

################################################

set.seed(0)

N = 10000
d = 2
df = data.frame(x=c(rnorm(N,0,1),rnorm(N,d,1)),
                y=c(rnorm(N,0,1),rnorm(N,d,1)))

# compute euclidean distance
sqrt((d-0)^2+(d-0)^2)

# plot dots
ggscatter = ggplot(df,aes(x=x,y=y))+
  geom_point(size=14,color="blue",alpha=.025)+
  theme(text=element_text(size=100))+
  stat_density_2d(size=3,color="black")+
  coord_cartesian(xlim=c(-2.5,4.5),ylim=c(-2.5,4.5))+
  scale_x_continuous(breaks=seq(-6,6,2))+
  scale_y_continuous(breaks=seq(-6,6,2))
x_hist = marginal_distribution(df,"x")
y_hist = marginal_distribution(df,"y")+coord_flip()
aligned_x_hist <- align_plots(x_hist, ggscatter, align = "v")[[1]]
aligned_y_hist <- align_plots(y_hist, ggscatter, align = "h")[[1]]

gggrid = plot_grid(
  aligned_x_hist
  , NULL
  , ggscatter
  , aligned_y_hist
  , ncol = 2
  , nrow = 2
  , rel_heights = c(0.2, 1)
  , rel_widths = c(1, 0.2)
) 

png("biviariate-scatter-plot.png",width=2400,height=1600,units="px")
gggrid
dev.off()

################################################



