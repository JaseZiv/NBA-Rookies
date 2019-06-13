library(tidyverse)
library(rvest)


a <- read_html("https://www.espn.com/mens-college-basketball/playbyplay?gameId=401123394")


b <- html_node(a, ".shot-chart")

f <- html_nodes(b, ".away-team")

f_text <- html_nodes(f, "li") %>% html_text()

aaa <- length(html_nodes(f, "li"))

c %>% html_text()


xml_attrs(xml_child(c[[1]], 1))[["style"]]

d <- c()

for(i in 1:aaa) {
  d <- c(d, xml_attrs(xml_child(f[[1]], i))[["style"]])
}




df <- cbind(f_text, d) %>% data.frame()

df$left <- gsub(".*left","", df$d) %>% gsub("top.*", "", .) %>% gsub(":", "", .) %>% gsub("%", "", .) %>% gsub(";", "", .) %>% as.numeric()
df$top <- gsub(".*top:","", df$d) %>% gsub("%;", "", .) %>% as.numeric()

df$left <- 1 - (df$left / 100)
df$top <- 1 - (df$top / 100)

df %>% 
  ggplot(aes(x= left, y=top)) +
  geom_point()
