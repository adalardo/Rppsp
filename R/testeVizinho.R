entorno <- function (x, y, xc, yc, r) 
{
    ((x^2) + (y^2) - (2 * xc * x) - (2 * yc * y) + ((xc^2) + (yc^2) - (r^2))) <= 
        0
}

xc <- 10
yc <- 10

r <- 5

x<- 0:20
y<- 0:20

plot(y~x)
entVF<- entorno(x, y, xc, yc, r)
cbind(x,y, entVF)


############# desenhar um circulo
circle <- function(xc, yc, r, ...) { 
    polygon( cos(seq(0, 2*pi, pi/180)) * r + xc, sin(seq(0, 2*pi, pi/180)) * r + yc ,... )
  }


circle(xc,yc, r, col = rgb(1,0,0,0.3))

#plot.circle(100,100, 10, col="red")


# funciona só para uma figura de cada vez, para várias deve usar apply

########################
insideCircle <- function(x, y, xc, yc, r)
{
(x - xc)^2 + (y - yc)^2 < r^2
}


