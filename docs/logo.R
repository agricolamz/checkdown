library(tidyverse)

tibble(x = 1, y = 6:1, label = paste0(letters[1:6], ") ...............")) %>%
  mutate(label = c(label[-c(4, 5, 6)], "", "", "")) %>%
  ggplot(aes(x, y, label = label))+
  geom_text(size = 17, hjust = 0)+
  annotate(geom = "text", label = c("\u25A1", "\u25A1", "\u25A1", "", "", ""), x = 2, y = 6:1, size = 17)+
  annotate(geom = "text", label = "> hint!", x = 1.7, y = 2.5, size = 17)+
  theme_void() ->
  p

library(showtext)
# font_add_google("Noto Serif")

library(hexSticker)
sticker(p,
        package="checkdown", p_size=20, s_x=.8, s_y=.6, s_width=1.4, s_height=1.2, h_fill = "lightblue", h_color = "lightblue",
        p_family = "Noto Serif",
        filename = "man/figures/logo.png")
