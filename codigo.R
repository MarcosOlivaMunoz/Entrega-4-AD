library(tidyverse, warn.conflicts = FALSE)
library(plot3D)
library(plotly)
library(mvnormtest)

set.seed(2023)
df = data.frame(x=c(), y=c()) %>% as.tibble()
for (i in 1:200) {
  vec = data.frame("x"=runif(80),"y"=runif(80))
  df = rbind(df, c(mean(vec$x), mean(vec$y)))
}
df_original <- df %>% rename(.,x=1,y=2)
df <- df %>% rename(.,x=1,y=2) %>% mutate(., x=cut(x,35)) %>% mutate(., y=cut(y,35))

z = table(df)

hist3D(z=z, border="black")

grafico1 <- ggplot(data = df, aes(x,y)) + geom_bin2d(binwidth= c(0.25,500)) +
  scale_fill_gradient2(low = "#0088ff", high = "#ff0000", mid = "#ffff00", midpoint = 2) + 
  theme_classic() + theme(axis.text.x = element_text(angle = 90)); grafico1

x = colnames(z)
y = rownames(z)
grid <- list(
  gridcolor='rgb(255, 255, 255)',
  zerolinecolor='rgb(255, 255, 255)',
  showbackground=TRUE,
  backgroundcolor='rgb(230, 230,230)'
)
grafico3 <- plot_ly(x = x, y = y, z = z, type = "mesh3d") %>% add_surface(showscale=FALSE) %>% 
  layout(title = "Histograma", scene = list(xaxis=grid,yaxis=grid,zaxis=grid)); grafico3


df_original %>% t() %>% mshapiro.test()

c(mean(df_original[,1]), mean(df_original[,2]))

var(df_original)
