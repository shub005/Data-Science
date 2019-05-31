##Jitter 
install.packages("ggplot2")
library(ggplot2)
mtcars<-mtcars
View(mtcars)

# Plot the cyl on the x-axis and wt on the y-axis
ggplot(mtcars, aes(x=cyl, y=wt)) + geom_point()


# Note that points on 3 vertical lines are not easy to read
# Use geom_jitter() instead of geom_point(); Jitter is a type of point plot used to avoid overplotting,
# especially for categorical variables like cyl. Jitter can be written as a position in the geom_point layer,
# which means the code below is same as geom_point(position = "jitter") 
# Note that jitter plot scatters the points a little too much
ggplot(mtcars, aes(x=cyl, y=wt)) + geom_point(position="jitter")

# Let's avoid the random scatter made by jitter using a width argument in jitter
# You can define a position object and put it inside 
# Define a position object using position_jitter(): 
jitter_posn<-position_jitter(width = 0.5)

# Use jitter_posn object inside the geom_point in geom_point() or geom_jitter
ggplot(mtcars,aes(x=cyl,y=wt))+geom_jitter(position = jitter_posn)
# Both commands below are exactly the same







