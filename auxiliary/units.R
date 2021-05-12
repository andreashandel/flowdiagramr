size = 10
fontsize = size * ggplot2:::.pt


string <- "this is it"
ttt <- strwidth(string)


df <- data.frame(x = 0, y = 0, lb = string)
nodes <- diagram_list$nodes

ggplot() +
  # geom_rect(data = nodes[1, ],
            # aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  geom_text(data = df, aes(x =x , y=y, label = string), size = 10) +
  coord_equal()


unit(0.5, "npc")
unit(0.5, "inches")





Labels <- c("Alabama", "Alaska", "Arizona", "Arkansas", 
            "Pennsylvania + California")
TextFrame <- data.frame(X = 4:8, Y = 4:8, LAB = Labels)
TextFrame <- transform(TextFrame,
                       w = strwidth(LAB, 'inches') * 2,
                       h = strheight(LAB, 'inches') * 2
)
SampleFrame <- data.frame(X = 1:10, Y = 1:10)

ggplot(data = SampleFrame,aes(x = X, y = Y)) + 
  geom_point(size = 20) +
  geom_rect(data = TextFrame, aes(xmin = X - w/2, xmax = X + w/2, 
                                  ymin = Y - h/2, ymax = Y + h/2), fill = "grey80") +
  geom_text(data = TextFrame,aes(x = X, y = Y, label = LAB), size = 4) +
  coord_equal()
