#-Creating Visualizations Using ggplot()----------install.packages("ggplot2")
library(ggplot2)
# --------Plotting using ggplot2: mtcars data frame----------
# mtcars has observations of 32 cars with miles per gallon, cylinders, weight, no of gears etc.
mtcar<-mtcar
View(mtcar)
# Check structure of mtcars 
str(mtcar)
# This is the first plot using ggplot2 using the built-in data frame mtcars.
# Plot cyl on x and mpg on y axis
ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
ggplot(mtcars, aes(x=factor(cyl), y=mpg)) + geom_point()
ggplot(mtcars, aes(x=wt, y=mpg, size=hp)) + geom_point()
ggplot(mtcars, aes(x=wt, y=mpg, col=factor(cyl))) + geom_point(shape=4, size=4)
ggplot(mtcars, aes(x=wt, y=mpg, col=hp, size=disp)) + geom_point()+geom_text(aes(label=cyl))
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_text(aes(label=cyl))+geom_point()

ggplot(mtcars, aes(x=cyl, y=wt)) + geom_point()
ggplot(mtcars, aes(x=cyl, y=wt)) + geom_point(position="jitter")
jitter_posn<-position_jitter(width = 0.5)
position_jitter(): 
  jitter_posn<-position_jitter(width = 0.5)
ggplot(mtcars,aes(x=cyl,y=wt))+geom_jitter(position = jitter_posn)
                                           
                                           