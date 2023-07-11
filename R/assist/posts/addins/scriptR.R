library(ggplot2)

ggplot(P3_Menages) +
  aes(x = hv000) +
  geom_bar(fill = "#FF6992", color="black") +
  labs(
    x = "Enquête",
    y = "Nombre de ménages",
    title = "Ménages par pays et vagues d'enquête",
    caption = "Source: DHS 2017"
  ) +
  coord_flip() +
  theme_light()



