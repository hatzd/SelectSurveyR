#' SSov_plot
#'
#' @param qn Name of the object with the question from the overview to plot.
#'
#' @return A plot of the results from the overview for that question.
#' @export
#'
#' @examples
#' SSov_plot(Q_1)
SSov_plot <- function(qn){
  invisible(require(ggplot2,
                    quietly = TRUE,
                    warn.conflicts = FALSE))
  invisible(require(stringr,
                    quietly = TRUE,
                    warn.conflicts = FALSE))
  invisible(require(grid,
                    quietly = TRUE,
                    warn.conflicts = FALSE))
  invisible(require(gridExtra,
                    quietly = TRUE,
                    warn.conflicts = FALSE))
  obj_name <- function(x) {
    deparse(substitute(x))
  }
  qn$Percent <- as.numeric(qn$Percent)
  # Name graph
  p_name <- paste(obj_name(qn), "Ov_plot.png", sep = "_")
  # Make plot
  p <-  ggplot(qn[3:(NROW(qn) - 1),], aes(x = Variables, y = as.numeric(Percent)*100, , fill = "transparent")) +
    geom_col(fill = "grey") +
    scale_y_continuous(limits = c(0, max(as.numeric(qn[3:(NROW(qn) - 1), "Percent"])*100) + max(as.numeric(qn[3:(NROW(qn) - 1), "Percent"])*100)/4)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5),
          #text = element_text(size = 20),
          plot.margin = unit(c(0.2,0.5,0,0.5),"cm"),
          # axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          rect = element_rect(fill = "transparent"))  +
    geom_text(stat = "identity",
              aes(label = paste(Number.of.responders, "(", round(as.numeric(Percent)*100, digits = 1), "%)"),
                  y = as.numeric(Percent)*100) , vjust = 0.2, hjust = -0.2) +
    ylab("Responders (%)") +
    ggtitle(str_wrap(qn[1,1], width = 40), paste("\n Total responders:", qn[NROW(qn),"Totals"])) +
    coord_flip()
  # Change grob positions
  g <- ggplotGrob(p)
  g$layout$l[g$layout$name == "title"] <- 1
  g$layout$l[g$layout$name == "subtitle"] <- 1
  g$layout$b[g$layout$name == "subtitle"] <- 2
  g$layout$z[g$layout$name == "subtitle"] <- 15
  grid::grid.draw(g)

  ggsave(p_name, plot = grid::grid.draw(g), device = "png",
         scale = 1, width = 22, height = 25, dpi = 300,
         units = "cm", ,  bg = "transparent")
  print(paste("Your barplot is saved in", getwd(), "as", p_name))
  invisible(gc())
  #assign(p_name, g, envir = .GlobalEnv)
}
