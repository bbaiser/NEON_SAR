library(tidyverse)
library(vegan)

beetle<-read.csv("Data/sp_rich_beetle.csv", row=1)|> 
        filter(siteID=="ABBY")|>
        select(plotID,n_sp)

expand.grid(beetle[,2])

combinations(3,2,letters[1:3])
combinations(3,2,letters[1:3],repeats=TRUE)


x <- seq(0, 10, length.out = 10)
y <- seq(-1, 1, length.out = 20)
d1 <- expand.grid(x = x)
d2 <- expand.grid(x = x ,KEEP.OUT.ATTRS = FALSE)
object.size(d1) - object.size(d2)

data(BCI)
sp1 <- specaccum(BCI)
sp2 <- specaccum(beetle[,2], "random")
sp2
summary(sp2)


plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp2, col="yellow", add=TRUE, pch="+")


## Fit Lomolino model to the exact accumulation
mod1 <- fitspecaccum(sp1, "lomolino")
coef(mod1)
fitted(mod1)
plot(sp1)
## Add Lomolino model using argument 'add'
plot(mod1, add = TRUE, col=2, lwd=2)
## Fit Arrhenius models to all random accumulations
mods <- fitspecaccum(sp2, "arrh")
plot(mods, col="hotpink")
boxplot(sp2, col = "yellow", border = "blue", lty=1, cex=0.3, add= TRUE)
## Use nls() methods to the list of models
sapply(mods$models, AIC)