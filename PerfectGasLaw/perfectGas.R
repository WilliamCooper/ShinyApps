library(plotly)
Rd <- Ranadu::StandardConstant('Rd')
airDensity <- function (p, TT) {
  return (p*100 / (Rd * (TT+273.15)))
}
p <- seq (40, 1000, by=10)
TC <- seq (-85, 40, by=5)
TV <- TC
eps <- Ranadu::StandardConstant('MWW') / Ranadu::StandardConstant('MWD')
e <- Ranadu::MurphyKoop(TC)

rho <- matrix(nrow=length(p), ncol=length(TC))
rhos <- matrix(nrow=length(p), ncol=length(TC))
hvr <- matrix(nrow=length(p), ncol=length(TC))
for (i in 1:length(p)) {
  for (j in 1:length(TC)) {
    rho[i,j] <- airDensity (p[i], TC[j])
    r <- eps * e[j] / (p[i] - e[j])
    TV[j] <- (TC[j] + 273.15) * (1 + r / eps) / (1 + r) - 273.15
    rhos[i,j] <- airDensity (p[i], TV[j])
    hvr[i,j] <- sprintf('%.3f %.3f', rho[i,j], rhos[i,j])
  }
}
# sample constant-p line:
pline <- 700
Tline <- 20
rholine <- airDensity(pline, Tline)
Dp <- data.frame(x=TC, y=rep(pline, length(TC)), z=airDensity(pline, TC))
Dp2 <- data.frame(x=rep(20, length(p)), y=p, z=airDensity(p, 20))
Dp3 <- data.frame(x=TC, y=rholine*Rd*(TC+273.15)/100, z=rholine)
q <- plot_ly(z = ~rho, x = ~TC, y = ~p, type='surface', text=hvr, hoverinfo='text+x+y') %>%
add_paths(data=Dp, x=Dp$x, y=pline, z=Dp$z, line=list(color = 'blue', width = 5), name='p=700') %>%
add_paths(q, data=Dp2, x=Tline, y=Dp2$y, z=Dp2$z, line=list(color = 'red', width = 5), name='T=20') %>%
add_paths(q, data=Dp3, x=Dp3$x, y=Dp3$y, z=rholine, line=list(color = 'brown', width = 5), name='rho=constant')

# q <- plot_ly(z = ~rho, x = ~TC, y = ~p, type='surface', text=hvr, hoverinfo='text+x+y') %>%
#   add_lines(data=Dp, x=Dp$x, y=Dp$y, z=Dp$z, line=list(color = 'red', width = 5)) #%>%
  #add_lines(data=Dp2, x=Dp2$x, y=Dp2$y, z=Dp2$z, line=list(color = 'red', width = 5))
    # %>% add_surface(z = ~rhos) 
fnt <- list(family = "Arial", size = 18, color = "#444")
ylab <- list(title = "P", titlefont = fnt, showgrid=TRUE)
xlab <- list(title = "T", titlefont = fnt)
zlab <- list(title=" density", titlefont = fnt)
q <- layout(q, scene=list(xaxis=xlab, yaxis=ylab, zaxis=zlab, camera=list(eye=list(x=-1.25, y=-1.5, z=0.03))), 
       title='The Ideal-Gas Law') 
pdf(file='IdealGasLaw.pdf')
print(q)
dev.off()

        #%>% 
  # add_annotations(text=expression(paste('rho ',rho,' mu ',mu)), textfont=fnt, size=18, xref='paper', yref='paper', x=0.25, y=0.5)
