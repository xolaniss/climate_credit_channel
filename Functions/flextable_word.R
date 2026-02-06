flextable_word <- function(data,
                           caption = "Add title",
                           caption_style = "normal",
                           width_add = 0,
                           heigth_add = 0) {
  flextable(data = data, theme_fun = theme_booktabs) %>%
    colformat_double() %>% 
    set_caption(caption = caption, style = caption_style) %>%
    border_inner(border = fp_border(color = "white"), part = "all") %>%
    vline_left(border = fp_border(color = "white"), part = "all") %>%
    vline_right(border = fp_border(color = "white"), part = "all") %>%
    autofit(add_w =  width_add , add_h = heigth_add, unit = "cm") %>% 
    align(align = "center", part = "all")
}


