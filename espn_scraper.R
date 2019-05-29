library(rvest)
library(httr)
library(dplyr)


# Scrape the Team’s schedule ----------------------------------------------

espn <- "http://www.espn.com"

team_site <- read_html("http://www.espn.com/mens-college-basketball/team/_/id/93/murray-state-racers")

team_name <- team_site %>% html_node(".ClubhouseHeader__Location") %>% html_text()
team_knickname <- team_site %>% html_node(".ClubhouseHeader__DisplayName") %>% html_text()


game_refs <- team_site %>% html_node(".club-schedule") %>% html_nodes("a") %>% html_attr("href")
game_refs <- game_refs[-1] 
game_refs <- game_refs[-length(game_refs)]

game_ref <- c()
for (i in game_refs) {
  each_game_ref <- paste0(espn, i)
  game_ref <- c(game_ref, each_game_ref)
}






# Play-by-play logs for selected game -------------------------------------

play_by_play_parser <- function(page_url) {
  
  site <- read_html(page_url)
  
  
  test <- html_node(site, ".content")
  
  halves <- html_nodes(test, ".webview-internal") %>% html_text()
  
  # NOTE: Need to still get the image logo to determine which team it is
  play_by_play <- data.frame(half=as.character(), logo=as.character(), times=as.character(), event=as.character(), score=as.character())
  
  for(i in 1:length(halves)) {
    which_half <- test %>% html_node(paste0("#gp-quarter-", i))
    logo <- which_half %>% html_nodes(".team-logo")
    times <- which_half %>% html_nodes(".time-stamp")%>% html_text()
    event <- which_half %>% html_nodes(".game-details") %>% html_text()
    score <- which_half %>% html_nodes(".combined-score") %>% html_text()
    half <- rep(halves[i], times = length(score))
    
    df <- cbind(half, logo, times, event, score)
    
    play_by_play <- rbind(play_by_play, df)
    
  }
  
  return(play_by_play)
}



logo <- test %>% html_nodes(".team-logo")


aaa <- sub("game?","playbyplay", game_ref[1])

aaa <- "http://www.espn.com/mens-college-basketball/playbyplay?gameId=401123394"
  
a<-  play_by_play_parser(page_url = aaa)






test <- read_html("http://www.espn.com/mens-college-basketball/playbyplay?gameId=401123394") 


test %>% html_nodes(".away .long-name") %>% html_text()
test %>% html_nodes(".home .long-name") %>% html_text()
# away logo
away_logo <- test %>% html_node(".away .team__banner__wrapper img")
away_logo <- xml_attrs(away_logo)[["src"]]


# home logo
test %>% html_node(".home .team__banner__wrapper img")



test %>% html_node(".icon-font-before") %>% html_text()
test %>% html_node("#gamepackage-matchup-wrap .icon-font-after") %>% html_text()









# Scrape Game Scedules ----------------------------------------------------

url <- read_html("http://www.espn.com/mens-college-basketball/schedule/_/date/20190101/group/50")

# date variable (ideally, i'd get this for the date: class="calendar-container" data-thisdate="20190101")
url %>% html_node(".table-caption") %>% html_text()

# all teams on the page
url %>% html_nodes(".team-name") %>% html_text()
# home teams
url %>% html_nodes(".home") %>% html_nodes(".team-name") %>% html_text()


url %>% html_nodes(".team-name") %>% html_nodes("span:nth-of-type(1n+1)") %>% html_text()


games_id <- url %>% html_nodes(".home+ td a") %>% html_attr("href")















