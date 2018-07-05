library(rvest)
library(dplyr)

#sample house vote <- https://www.govtrack.us/congress/votes/114-2016/h622
#sample senate vote <- https://www.govtrack.us/congress/votes/114-2016/s163

#Goal of Function is to extract information about every vote taken by the House and Senate as far back as GovTrack allows.
#Can all using congress numbers, years, and s/h votes.

#Could organize using a TryCatch system to loop through an increasing number of votes. Probably easiest, not sure how fast it will go.



govtrack_vote_info <- function (congress.num, congress.year, house.senate, vote.number) {
  url <- paste('https://www.govtrack.us/congress/votes/', congress.num, '-', congress.year, '/', house.senate, vote.number, sep='') #working
  print(url)
  wp <- read_html(url)
  #Voting Information
  rep.yay <- html_nodes(wp, 'tr:nth-child(1) td:nth-child(4) > div:nth-child(1)') %>% html_text()
  rep.nay <- html_nodes(wp, 'tr:nth-child(2) td:nth-child(4) > div:nth-child(1)') %>% html_text()
  #rep.novote <- html_nodes(wp, 'tr:nth-child(3) td:nth-child(4) > div:nth-child(1)') %>% html_text()

  dem.yay <- html_nodes(wp, 'tr:nth-child(1) td:nth-child(5) > div:nth-child(1)') %>% html_text()
  dem.nay <- html_nodes(wp, 'tr:nth-child(2) td:nth-child(5) > div:nth-child(1)') %>% html_text()
  #dem.novote <- html_nodes(wp, 'tr:nth-child(3) td:nth-child(5) > div:nth-child(1)') %>% html_text()

  ind.yay <- html_nodes(wp, 'tr:nth-child(1) td:nth-child(6) > div:nth-child(1)') %>% html_text()
  ind.nay <- html_nodes(wp, 'tr:nth-child(2) td:nth-child(6) > div:nth-child(1)') %>% html_text()
  #ind.novote <- html_nodes(wp, 'tr:nth-child(3) td:nth-child(6) > div:nth-child(1)') %>% html_text()
  
  total.yay <- html_nodes(wp, 'tr:nth-child(1) .color_I+ div') %>% html_text()
  total.nay <- html_nodes(wp, 'tr:nth-child(2) .color_I+ div') %>% html_text()
  #total.novote <- html_nodes(wp, 'tr:nth-child(3) .color_I+ div') %>% html_text()
  total.novote <- sum(as.integer(rep.yay), as.integer(rep.nay), as.integer(dem.yay), as.integer(dem.nay),
                      as.integer(ind.yay), as.integer(ind.nay))
  total.novote <- 100 - total.novote
  
  
  #Other Bill/Vote Information
  date <- html_nodes(wp, '.vote-meta-info p:nth-child(1)') %>% html_text()
  date <- substring(date, 7)
  bill.title <- html_nodes(wp, '.long') %>% html_text()

  one_vote <-data.frame(date, bill.title, total.yay, total.nay, rep.yay, dem.yay, ind.yay, rep.nay, dem.nay, ind.nay, total.novote)
  #one_vote <- data.frame(date, bill.title, total.yay, total.nay, total.novote, rep.yay, dem.yay, ind.yay, rep.nay, dem.nay, ind.nay, rep.novote, dem.novote, ind.novote)
  return(one_vote)
}

one_vote <- govtrack_vote_info(congress.num=114, congress.year=2016, house.senate='s', vote.number=104)

all_govtrack_vote_info <- function() {
  all_votes <- c(1:2)
  df_all_votes <- NULL
  for (vote in all_votes) {
    one_year <- govtrack_vote_info(114, 2016, 's', vote)
    df_all_votes <- rbind(df_all_votes, one_year)
  }
  return(df_all_votes)
}

all_govtrack_votes <- all_govtrack_vote_info()
