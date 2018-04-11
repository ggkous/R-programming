histomgram_bar <- function(title, x, y, fill) {
  same = list(
    geom_bar(stat = "identity", width = 0.6),
    coord_cartesian(ylim = c(0, 100)),
    labs(
      title = title,
      x = x,
      y = y,
      fill = fill
    )
  )
}

scale_fill_color <- function() {

  color = list(scale_fill_manual(
    #breaks=c("ANGRY","CONFUSED","HAPPY","CALM","SAD","DISGUSTED","SURPRISED"),
    values = c(
      "#F8766D",
      "#C49A00",
      "#53B400",
      "#00C094",
      "#00B6EB",
      "#A58AFF",
      "#FB61D7"
    ),
    limits = c(
      "ANGRY",
      "CONFUSED",
      "HAPPY",
      "CALM",
      "SAD",
      "DISGUSTED",
      "SURPRISED"
    )
  ))
}

scale_manual_color <- function(){
  color = list(scale_color_manual(
    #breaks=c("ANGRY","CONFUSED","HAPPY","CALM","SAD","DISGUSTED","SURPRISED"),
    values = c(
      "#F8766D",
      "#C49A00",
      "#53B400",
      "#00C094",
      "#00B6EB",
      "#A58AFF",
      "#FB61D7"
    ),
    limits = c(
      "ANGRY",
      "CONFUSED",
      "HAPPY",
      "CALM",
      "SAD",
      "DISGUSTED",
      "SURPRISED"
    )
  ))
}
theme_text_size <- function(axis.text.x) {
  theme(
    rect = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "transparent"),
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.text = element_text(size = 8),
    axis.text.x = axis.text.x,
    # axis.text.x = element_text(size = rel(1.5),face="bold")
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title.y = element_text(size = 8),
    axis.title.x = element_text(size = 8),#margin = margin(t = 0, r = 0, b = 0, l = 20)
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    plot.margin =  unit(c(1,1,1,1), "cm") #3:bottom
  )
}

#one graph version - is bigger
# theme_text_size <- function(axis.text.x) {
#   theme(
#     rect = element_rect(fill = "transparent"),
#     panel.background = element_rect(fill = "transparent"),
#     plot.title = element_text(hjust = 0.5, size = rel(1.8)),
#     axis.text = element_text(size = rel(1)),
#     axis.text.x = axis.text.x,
#     # axis.text.x = element_text(size = rel(1.5),face="bold")
#     axis.text.y = element_text(size = rel(1.5), face = "bold"),
#     axis.title.y = element_text(size = rel(1.5)),
#     axis.title.x = element_text(size = rel(1.5)),#margin = margin(t = 0, r = 0, b = 0, l = 20)
#     legend.title = element_text(size = rel(1.5)),
#     legend.text = element_text(size = rel(1.5)),
#     plot.margin = unit(c(0.3,0.3,0.3, 0.3), "cm") #3:bottom
#   )
# }