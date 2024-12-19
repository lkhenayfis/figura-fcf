library(htmlwidgets)

source("R/function-generators.r")
source("R/visualizacao.r")

# FCF pura

f <- fcf_generator(0, -2, -1)
p <- plota_fcf(f)
saveWidget(p, "./fcf.html")

# CORTES

x_corte1 <- c(.15, .6)
x_corte2 <- c(.85, .6)
p <- plota_corte(f, list(x_corte1, x_corte2)) %>%
    layout(scene = list(zaxis = list(range = c(0, 1))))
saveWidget(p, "./cortes.html")

