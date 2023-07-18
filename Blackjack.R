library(dplyr)


result <- data.frame()

draw_card <- function(){
  cards <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10)
  sample(cards, 1)
}


Hit <- function(you, dealer){
  
  # draw card for player
  you <- you + draw_card()
    
  # draw cards for dealer
    while (dealer < 17) {
      dealer <- dealer + draw_card()
    
    }

  # test if win or lose
  if(you > 21){result <- 'lose'}
  else if(dealer > 21){result <- 'win'}
  else if(you > dealer){result <- 'win'}
  else if(you < dealer){result <- 'lose'}
  else{result <- 'tie'}
}




Stay <- function(you, dealer){
  
  # draw card for player
  you <- you
  
  # draw cards for dealer
  while (dealer < 17) {
    dealer <- dealer + draw_card()
  }
  
  # test if win or lose
  if(you > 21){result <- 'lose'}
  else if(dealer > 21){result <- 'win'}
  else if(you > dealer){result <- 'win'}
  else if(you < dealer){result <- 'lose'}
  else{result <- 'tie'}
}


Blackjack <- function(you, dealer, sims){
  results_hit <- replicate(sims, Hit(you, dealer))
  result_stay <- replicate(sims, Stay(you, dealer))
  index <- 1:sims
  wins_freq_hit <- cumsum(results_hit == 'win') / index
  wins_freq_stay <- cumsum(result_stay == 'win') / index
  
  par(mfrow = c(1,2))
  
  plot_hit <- plot(wins_freq_hit,
       type = 'l',
       lwd = 2.5,
       col = 'blue',
       ylim = c(0,1),
       ylab = "Percent of Hands Won",
       xlab = "Number of Hands",
       main = "Hit!",
       log = 'x')
  
  plot_stay <- plot(wins_freq_stay,
                type = 'l',
                lwd = 2.5,
                col = 'blue',
                ylim = c(0,1),
                ylab = "Percent of Hands Won",
                xlab = "Number of Hands",
                main = "Stay",
                log = 'x')
  
  wins_freq_hit <- sum(results_hit == 'win') / (sum(results_hit == 'win') + sum(results_hit == 'lose'))
  wins_freq_stay <- sum(result_stay == 'win') / (sum(result_stay == 'win') + sum(result_stay == 'lose'))
  
  hit_if_positive <- round((wins_freq_hit - wins_freq_stay), digits = 3)
  
  if(hit_if_positive >= 0){
    print(paste0("Hit. % diff between Hit and Stay: ", hit_if_positive, "."))
  }
  else{print(paste0("Stay. % diff between Hit and Stay: ", hit_if_positive, "."))
  }
}

Blackjack(12,4,5000)


your_cards <- seq(12, 20, 1)
dealers_cards <- seq(2, 10, 1)

results_all <- array(rep(NA,81),dim = c(9,9))
for (i in your_cards){
  for (j in dealers_cards){

    results_all[i-11,j-1] <- Blackjack(i, j, 1000)

  }
}

final_results <- as.data.frame(results_all)
colnames(final_results) <- c("Dealer_show_2",
                     "Dealer_show_3",
                     "Dealer_show_4",
                     "Dealer_show_5",
                     "Dealer_show_6",
                     "Dealer_show_7",
                     "Dealer_show_8",
                     "Dealer_show_9",
                     "Dealer_show_10")

final_results$Your_Cards <- seq(12, 20, 1)

col_idx <- grep("Your_Cards", names(final_results))
final_results <- final_results[, c(col_idx, (1:ncol(final_results))[-col_idx])]
names(final_results)
View(final_results)
