# bar chart
library(aRt)
library(dplyr)
library(ggplot2)
num_circles = 20
main_col = "lightgrey"
col_palette = rcartocolor::carto_pal(n = 12, "Bold")
bg_col = "white"
s = 123
set.seed(s)
x0 <- sample(1:(4 * num_circles), size = num_circles, replace = FALSE)
y0 <- sample(1:(4 * num_circles), size = num_circles, replace = FALSE)
r <- sample(1:(0.75 * num_circles), size = num_circles, replace = TRUE)
plot_data <- data.frame(x = c(), y = c(), group = c(), group_circle = c())
for (i in 1:num_circles) {
  k <- aRt:::draw_ellipse_in_circle(x0 = x0[i], y0 = y0[i], r = r[i]) %>%
    dplyr::mutate(group_circle = i,
                  circle_col = as.character(sample(1:13, size = 1, prob = c(rep(0.01, 12), 0.78))))
  plot_data <- rbind(plot_data, k)
}
plot_data <- tidyr::unite(plot_data, col = "new_group", .data$group:.data$group_circle, sep = ":", remove = FALSE)
barchart <- plot_data %>% 
  select(-y) %>% 
  distinct() %>% 
  ggplot(mapping = aes(x = group, 
                       y = x, 
                       fill = factor(circle_col))) +
  geom_col(position = "stack") +
  labs(x = "x", 
       y = "", 
       title = "Bar chart", 
       subtitle = "Subtitle telling you something about the chart") +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"))
barchart
ggsave(barchart, filename="2022/2022-07-useR-generative-art/images/barchart.jpeg", width = 4, height = 4, unit = "in")

# bubbles
bubbles1 = bubbles(num_circles = 20, main_col = "lightgrey", col_palette = rcartocolor::carto_pal(n = 12, "Bold"), bg_col = "white", s = 123)
bubbles1
ggsave(bubbles1, filename="2022/2022-07-useR-generative-art/images/bubbles1.jpeg", width = 4, height = 4, unit = "in")

# bar chart two axis
library(patchwork)
set.seed(2206)
df1 <- data.frame(x = 1:10,
          y = LETTERS[1:10], 
          z = rnorm(10))
df1
p1 <- ggplot(df1, 
             aes(x, y)) +
  geom_col() +
  xlim(0, 10) +
  labs(x = "", y = "", title = "") +
  #theme(plot.margin = unit(c(0, 0.5, 0, 0), unit = "cm"))
  theme(plot.margin = unit(c(0, 1.7, 0, 0), unit = "cm"))
p1
ggsave(p1, filename="2022/2022-07-useR-generative-art/images/p1.jpeg", width = 4, height = 4, unit = "in")

p2 <- ggplot(df1, 
             aes(x, z)) +
  geom_line(colour = "blue", size = 2) +
  xlim(0, 10) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * 1, name = "Axis 2")
  ) +
  labs(x = "", y = "", title = "") +
  theme(axis.ticks.y.left = element_blank(), 
        axis.text.y.left = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y.right = element_text(colour = "blue", margin = margin(l = 10)), 
        axis.title.y.right = element_text(colour = "blue"), 
        axis.ticks.y.right = element_line(colour = "blue"))
p2
ggsave(p2, filename="2022/2022-07-useR-generative-art/images/p2.jpeg", width = 4, height = 4, unit = "in")

p = p1 + inset_element(p2 + theme( 
                   plot.background = element_blank(), 
                   panel.background = element_blank(), 
                   panel.grid = element_blank()), 
                   left = 0, bottom = 0, right = 1, top = 1, 
                   align_to = "full")
p
ggsave(p, filename="2022/2022-07-useR-generative-art/images/p.jpeg", width = 4, height = 4, unit = "in")

p = polygons(n_x=15, n_y=15, gap_size=0.5, deg_jitter=0.5, colours=rcartocolor::carto_pal(7, "Teal"), 
             rand = TRUE, bg_col="gray7")
p
ggsave(p, filename="2022/2022-07-useR-generative-art/images/polygons.jpeg", width = 4, height = 4, unit = "in")

p = polygons(n_x=15, n_y=15, gap_size=0.5, deg_jitter=0.5, colours=rcartocolor::carto_pal(7, "Teal"), 
             rand = TRUE, bg_col="#dae5e7")
p
ggsave(p, filename="2022/2022-07-useR-generative-art/images/polygons2.jpeg", width = 4, height = 4, unit = "in")


