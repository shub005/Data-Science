
##-------------Bar Charts in ggplot--------------------
install.packages("ggplot2")
library(ggplot2)
mtcars<-mtcars
View(mtcars)

# setting the ggplot object
ggplot(mtcars,aes(x=cyl))+geom_bar()
# The base layer is available : cyl.am

# Add geom (position = "stack" by default)
ggplot(mtcars,aes(x=cyl,fill=am))+geom_bar(position = "stack")
ggplot(mtcars,aes(x=cyl,fill=factor(am)))+geom_bar(position = "stack")
ggplot(mtcars,aes(x=cyl,fill=factor(am)))+geom_bar(position = "dodge")
# Fill - show proportion

## Frequency Polygon
ggplot(mtcars,aes(x=mpg,fill=factor(cyl)))+geom_histogram(binwidth = 1)
ggplot(mtcars,aes(x=mpg,col=cyl))+geom_freqpoly(binwidth = 1)
ggplot(mtcars,aes(x=mpg,col=cyl))+geom_freqpoly(binwidth = 2)      

myBlue<-"blue"
ggplot(mtcars, aes(x=mpg))+geom_histogram(binwidth = 1,fill=myBlue)
