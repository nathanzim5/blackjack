#Functions for generating basic blackjack strategy and strategy for the red 7 card counting system
#Author: Nathan Zimmerman


setwd("~/blackjack")
source("play.R")

#generate strategy
gen_strat <- function(n_shoes = 1000, 
                      n_decks = 6,
                      H17 = 1,
                      strat_play = "basic") {
  sp_file <- paste0("strat_play_", n_decks, "d_", ifelse(H17 == 1, "H17_", ""), strat_play, ".txt")
  sp_vec <- c(rep(NA, dec_length_sum), rep("N", r7_length))
  for(i in 1:length(dec_val)) {
    sp_vec[sum(dec_length_cumsum[i - 1], 1):dec_length_cumsum[i]] <- dec_choices[[i]][1]
  }
  write(sp_vec, file = sp_file, ncolumns = 1)
  for(i in 1:length(dec_val)) {
    for(j in dec_val[[i]]) {
      for(k in 1:10) {
        cat(paste0(paste(i, j, k, sep = "-"), "\n"))
        sp_names <- paste0(strat_play, "_w", dec_choices[[i]])
        sp_files_w <- paste0("strat_play_", n_decks, "d_", ifelse(H17 == 1, "H17_", ""), sp_names, ".txt")
        sp_vec <- vector("list", length(dec_choices[[i]]))
        for(l in 1:length(sp_vec)) {
          sp_vec[[l]] <- scan(sp_file, what = "character", quiet = TRUE)
          sp_vec[[l]][sum(dec_length_cumsum[i - 1]) + 
                        r7_length * 10 * (j - dec_minval[i]) / dec_div[i] +
                        r7_length * (k - 1) + r7_count - r7_min + 1] <- dec_choices[[i]][l]
          write(sp_vec[[l]], file = sp_files_w[l], ncolumns = 1)
        }
        p1 <- play(n_shoes = n_shoes,
                   n_decks = n_decks,
                   H17 = H17,
                   n_players = length(sp_names),
                   strat_play = sp_names,
                   spec_type = i,
                   spec_tot = j,
                   spec_d = k)
        cat("\n")
        cat(p1$avg)
        write(sp_vec[[which.max(p1$avg)]], file = sp_file, ncolumns = 1)
        cat("\n")
      }
    }
  }
  
  #set insurance strategy
  cat("insurance \n")
  ins_choices <- c("I", "N")
  sp_names <- paste0(strat_play, "_w", ins_choices)
  sp_files_w <- paste0("strat_play_", n_decks, "d_", ifelse(H17 == 1, "H17_", ""), sp_names, ".txt")
  sp_vec <- vector("list", 2)
  for(l in 1:length(sp_vec)) {
    sp_vec[[l]] <- scan(sp_file, what = "character", quiet = TRUE)
    sp_vec[[l]][dec_length_sum + r7_count - r7_min + 1] <- ins_choices[l]
    write(sp_vec[[l]], file = sp_files_w[l], ncolumns = 1)
  }
  p1 <- play(n_shoes = n_shoes,
             n_decks = n_decks,
             H17 = H17,
             n_players = length(sp_names),
             strat_play = sp_names,
             spec_ins = 1)
  cat("\n")
  cat(p1$avg)
  write(sp_vec[[which.max(p1$avg)]], file = sp_file, ncolumns = 1)
  cat("\n")
  
  #clean up intermediate files
  file.remove(paste0("strat_play_", n_decks, "d_", ifelse(H17 == 1, "H17_", ""), "basic_w", c("H", "S", "D", "P", "I", "N"), ".txt"))
}

gen_strat()




