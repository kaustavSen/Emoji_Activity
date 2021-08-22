library(tidyverse)
library(gt)

groups_to_include <- c(
  "person-sport",
  "face-positive",
  "face-negative",
  "money",
  "event",
  "office",
  "emotion",
  "person-gesture",
  "music",
  "transport-air",
  "computer",
  "place-map",
  "mail",
  "phone"
)

emoji_df <-
  emo::jis %>% 
  filter(vendor_google, !is.na(unicode_version)) %>% 
  filter(subgroup %in% groups_to_include) %>% 
  group_by(subgroup) %>% 
  add_count() %>% 
  mutate(group_id = seq(n)) %>% 
  filter(group_id <= 5) %>% 
  ungroup() %>% 
  rownames_to_column(var = "id") %>% 
  select(id, emoji, name)

gt(emoji_df) %>% 
  cols_label(id = "S.No.",
             emoji = "Emoji",
             name = "Emoji meaning") %>% 
  cols_width(vars(id) ~ pct(15), 
             vars(emoji) ~ pct(15), 
             vars(name) ~ pct(70)) %>% 
  cols_align(align = "center") %>% 
  tab_options(
    table.font.names = "Poppins",
    table.width = px(600),
    table.font.size = "24px",
    column_labels.font.weight = "bold"
  ) %>% 
 gtsave("full.html")

create_group_table <- function(df = emoji_df, icons_selected, group_name) {
  df %>% 
  filter(id %in% icons_selected) %>% 
  gt() %>% 
    cols_label(id = "S.No.",
               emoji = "Emoji",
               name = "Emoji meaning") %>% 
    cols_width(vars(id) ~ pct(15), 
               vars(emoji) ~ pct(15), 
               vars(name) ~ pct(70)) %>% 
    cols_align(align = "center") %>% 
    tab_options(
      table.font.names = "Poppins",
      table.width = px(600),
      table.font.size = "24px"
    ) %>% 
    gtsave(paste0({{ group_name }}, ".html"))
}

create_group_table(icons_selected = c(3, 8, 26, 40, 52, 66), group_name = "inglorious_cat_thieves")
create_group_table(icons_selected = c(), group_name = "the_artful_dodgers")
create_group_table(icons_selected = c(), group_name = "ict_sangha")
create_group_table(icons_selected = c(), group_name = "unpredictables")
