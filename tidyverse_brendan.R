# leanring the tidyverse with brendan ------------------------------------------

require(tidyverse)

# dplyr review working with df objects with fast and readable functions --------

count(starwars, hair_color)

starwars %>%
  pivot_longer(hair_color:eye_color, 
               names_to = "attribute", 
               values_to = "color") %>%
  count(color)


starwars %>%
  pivot_longer(contains("color"), 
               names_to = "attribute", 
               values_to = "color") %>%
  count(color) %>%
  ggplot(aes(x = color, y = n)) +
  geom_col() +
  coord_flip()


# short introduction to the stringr package ------------------------------------
# stings, regular expressions

x <- c("ab", "123")

str_sub(x, start = 1, end = 2)

str_dup(x, 2)

str_replace(fruit, ".$", "ahhh")
str_replace(fruit, "^.", "ahhh")

str_c(sentences, str_to_title(fruit), sep = " ")

str_c("some numbers",
      str_glue("pi is {pi}"),
      str_glue("the date is {format(Sys.time(), '%a %b %d %Y')}"),
      sep = " ")


# introduction to the purrr package --------------------------------------------

# we might be able to use this for the mate selection part??

# map family of functions 

my_func <- function(x, a = 3, b = 4) {
  print(str_glue('eq {x} * {a} - {b}'))
  x * a - b
} 

map(1:3, my_func)

# but you can specify the data type that the output is in
# this is also type stable 

map_chr(1:3, my_func)
map_dfc(1:3, my_func)


# MAP WITH SPLIT! seems very useful
starwars %>%
  add_count(homeworld) %>%
  filter(n > 5) %>%
  split(.$homeworld) %>%
  map(function(df) lm(height ~ mass, data = df))


# make it a bit fancier
starwars %>%
  add_count(homeworld) %>%
  filter(n > 5) %>%
  split(.$homeworld) %>%
  map(~ lm(height ~ mass, data = .))


# pmap parallel map 

# make a little tibble
params <- tribble(
    ~mean, ~sd, ~n,
    1, .5, 1,
    5, 5, 10,
    -1, 10, 100
)


params %>%
  pmap(rnorm)  




