library(ggplot2)

mydata <- iris
head(mydata)

unique(mydata$Species)
dim(mydata)

ggplot(data = mydata, aes(x = Sepal.Length,
                          y = Sepal.Width)) +
  geom_point(aes(col = Species))

library(ggplot2)

ggplot(mydata) +
 aes(x = Sepal.Length, y = Sepal.Width, colour = Species) +
 geom_point(shape = "circle", 
 size = 1.5) +
 scale_color_hue(direction = 1) +
 theme_minimal()

x <- seq(1, 10, by = 0.1)
plot_data <- data.frame(x = x,
                        y = 10 *sin(x)^2 +x^2)
with(plot_data, plot(x = x, y = y, type = "l",
                     main = "this ft. is f(x) = 10*sin(x)^2 + x^2"))

# 원래방식
with(plot_data, plot(x = x, y = y, type = "l",
                     main = expression(f(x) == 10*sin(x)^2 + x^2)))

library(latex2exp)
#expression 넣고 싶은 곳에
#TeX() 사용 가능
with(plot_data, plot(x = x, y = y, type = "l",
                     main = TeX("$f(x) = 10 sin^{2}(x) + x^2$")))

ggplot(data = plot_data, aes(x = x, y = y)) +
  geom_line() + theme_bw() + 
  labs(title = TeX("$f(x) = 10 sin^{2}(x) + x^2$")) +
  annotate("text", x = 2.5, y = 75, 
           label = TeX("$f(x) = \\sum_{i=1}^{n}(x_i - \\mu)^2$"))

library(scatterplot3d)
library(magritter)
