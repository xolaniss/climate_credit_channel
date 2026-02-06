borderlines <- function(table) {
  table %>%
    vline_left(border = fp_border(color = "white"), part = "all") %>%
    vline_right(border = fp_border(color = "white"), part = "all") %>%
    vline(j = c(1, 3, 5),
          border = border, # set border upfront
          part = c("body")) %>%
    vline(j = c(1, 3, 5),
          border = border,
          part = c("header")) %>%
    hline(border = fp_border(color = "white"), part = "footer") %>%
    hline_bottom(border = border, part = "footer")
  
}

borderlines_big_table <- function(table) {
  table %>%
    vline_left(border = fp_border(color = "white"), part = "all") %>%
    vline_right(border = fp_border(color = "white"), part = "all") %>%
    vline(j = c(1, 3, 5, 7, 9, 11 ),
          border = border,
          part = c("body")) %>%
    vline(j = c(1, 3, 5, 7, 9, 11),
          border = border,
          part = c("header")) %>%
    hline(border = fp_border(color = "white"), part = "footer") %>%
    hline_bottom(border = border, part = "footer")
  
}

