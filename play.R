setwd("~/blackjack")

#define deck of cards
deck_rank <- rep(c(2:10, "J", "Q", "K", "A"), 4)
deck_suit <- rep(c("C", "D", "H", "S"), 13)
deck_col <- ifelse(deck_suit %in% c("D", "H"), "R", "B")
deck_val <- deck_rank
deck_val[which(deck_rank == "A")] <- 1
deck_val[which(deck_rank %in% c("J", "Q", "K"))] <- 10
deck_val <- as.numeric(deck_val)
deck_r7 <- rep(0, 52)
deck_r7[deck_val %in% c(1, 10)] <- -1
deck_r7[deck_val %in% 2:6 | (deck_val == 7 & deck_col == "R")] <- 1

#define r7
r7_count <- -120:132
r7_min <- min(r7_count)
r7_length <- length(r7_count)
r7_bet_ramp <- c(rep(0, 100), rep(1, 21), 1, 2, 2, 4, 6, 8, 12, 16, 20, 20, rep(20, 122))

dec_choices <- list(c("H", "S"),
                    "H",
                    c("H", "S"),
                    "H",
                    c("H", "D", "S"),
                    c("H", "D"),
                    c("H", "D", "S"),
                    c("H", "D"),
                    c("H", "D", "S"),
                    c("P", "H", "D", "S"),
                    c("P", "H", "D"),
                    c("P", "H", "D"),
                    c("P", "S"))
dec_val <- list(20:12,
                11:6,
                10:7,
                6:3,
                19:12,
                11:5,
                10:7,
                6:3,
                20,
                seq(18, 12, by = -2),
                seq(10, 4, by = -2),
                2,
                2)
dec_val_count <- sapply(dec_val, length)
dec_length <- r7_length * 10 * dec_val_count
dec_length_cumsum <- cumsum(dec_length)
dec_length_sum <- sum(dec_length)
dec_minval <- sapply(dec_val, min)
dec_div <- c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2)

#play blackjack
play <- function(n_shoes = 1,
                 n_decks = 6,
                 H17 = 1,
                 n_players = 1,
                 pen = 1,
                 strat_play = rep("basic", n_players),
                 strat_bet = rep("flat", n_players)
) {
  out_p <- rep(0, n_players)
  
  #define shoe
  shoe_val <- rep(deck_val, n_decks)
  shoe_r7 <- rep(deck_r7, n_decks)
  n_cards <- length(shoe_val)
  
  #load strategy
  sp_file <- paste0("strat_play_", n_decks, "d_", ifelse(H17 == 1, "H17_", ""), strat_play, ".txt")
  sp_vec <- vector("list", n_players)
  for(i in 1:n_players) {
    sp_vec[[i]] <- scan(sp_file[i], what = "character", quiet = TRUE)
  }

  #loop through shoes
  for(i in 1:n_shoes) {
    
    #update progress bar every 1000 shoes
    if(i %% 1000 == 0) {
      cat("*")
    }
    
    #shuffle shoe
    sam <- sample(1:n_cards, n_cards)
    shoe_val <- shoe_val[sam]
    shoe_r7 <- shoe_r7[sam]
    dlt <- 1
    r7 <- -2 * n_decks
    
    #loop through hands
    while(dlt < n_cards - 52 * pen) {
      
      p <- vector("list", n_players)
      
      #bet and deal two cards to each player
      for(k in 1:n_players) {
        if(strat_bet[k] == "r7") {
          bet0 <- r7_bet_ramp[r7_count == r7]
        } else {
          bet0 <- 1
        }
        p[[k]] <- list(h = list(list(bet = bet0,
                                     val = shoe_val[c(dlt + 1, dlt + 2)],
                                     typ = NULL,
                                     tot = NULL,
                                     res = NA)),
                       i_bet = NA,
                       i_res = NA)
        r7 <- r7 + shoe_r7[dlt + 1] + shoe_r7[dlt + 2]
        dlt <- dlt + 2
      }
      
      #deal two cards to dealer
      d = list(val = shoe_val[c(dlt + 1, dlt + 2)],
               typ = NA,
               tot = NA)
      r7 <- r7 + shoe_r7[dlt + 1]
      dealer2_r7 <- shoe_r7[dlt + 2]
      dlt <- dlt + 2
      
      #offer insurance
      if(d$val[1] == 1) {
        for(k in 1:n_players) {
          dec_ins <- sp_vec[[k]][dec_length_sum + r7 - r7_min + 1]
          p[[k]]$i_bet <- ifelse(dec_ins == "I", 1, 0) * p[[k]]$h[[1]]$bet * 0.5
        }
      }
      
      #loop over players
      active_p <- 0
      for(k in 1:n_players) {
        
        #resolve insurance and blackjacks
        if(1 %in% d$val & 10 %in% d$val) {
          p[[k]]$i_res <- p[[k]]$i_bet * 2
          if(1 %in% p[[k]]$h[[1]]$val & 10 %in% p[[k]]$h[[1]]$val) {
            p[[k]]$h[[1]]$res <- 0
          } else {
            p[[k]]$h[[1]]$res <- -p[[k]]$h[[1]]$bet
          }
        } else {
          p[[k]]$i_res <- -p[[k]]$i_bet
          if(1 %in% p[[k]]$h[[1]]$val & 10 %in% p[[k]]$h[[1]]$val) {
            p[[k]]$h[[1]]$res <- 1.5 * p[[k]]$h[[1]]$bet
          } else {
            
            #loop over hands belonging to the same player
            l <- 1
            nHands <- 1
            while(l <= nHands) {
              
              #deal one card in the case of hand resulting from split
              if(l > 1) {
                p[[k]]$h[[l]]$val[2] <- shoe_val[dlt + 1]
                r7 <- r7 + shoe_r7[dlt + 1]
                dlt <- dlt + 1
              }
              
              #loop over playing decisions on the same hand
              dec <- "S"
              repeat {
                
                #calculate player total
                p[[k]]$h[[l]]$tot <- sum(p[[k]]$h[[l]]$val)

                #player busts
                if(p[[k]]$h[[l]]$tot > 21) {
                  p[[k]]$h[[l]]$res <- -p[[k]]$h[[l]]$bet
                  break
                }
                
                #determine player hand type
                if(length(p[[k]]$h[[l]]$val) == 2) {
                  if(p[[k]]$h[[l]]$val[1] == p[[k]]$h[[l]]$val[2]) {
                    if(p[[k]]$h[[l]]$val[1] == 1) {
                      if(l == 1) {
                        p[[k]]$h[[l]]$typ <- 12
                      } else {
                        p[[k]]$h[[l]]$typ <- 13
                      }
                    } else {
                      if(p[[k]]$h[[l]]$tot >= 12) {
                        if(p[[k]]$h[[l]]$tot == 20) {
                          p[[k]]$h[[l]]$typ <- 9
                        } else {
                          p[[k]]$h[[l]]$typ <- 10
                        }
                      } else {
                        p[[k]]$h[[l]]$typ <- 11
                      }
                    }
                  } else {
                    if(1 %in% p[[k]]$h[[l]]$val & p[[k]]$h[[l]]$tot <= 11) {
                      if(p[[k]]$h[[l]]$tot >= 7) {
                        p[[k]]$h[[l]]$typ <- 7
                      } else {
                        p[[k]]$h[[l]]$typ <- 8
                      }
                    } else {
                      if(p[[k]]$h[[l]]$tot >= 12) {
                        p[[k]]$h[[l]]$typ <- 5
                      } else {
                        p[[k]]$h[[l]]$typ <- 6
                      }
                    }
                  }
                } else {
                  if(1 %in% p[[k]]$h[[l]]$val & p[[k]]$h[[l]]$tot <= 11) {
                    if(p[[k]]$h[[l]]$tot >= 7) {
                      p[[k]]$h[[l]]$typ <- 3
                    } else {
                      p[[k]]$h[[l]]$typ <- 4
                    }
                  } else {
                    if(p[[k]]$h[[l]]$tot >= 12) {
                      p[[k]]$h[[l]]$typ <- 1
                    } else {
                      p[[k]]$h[[l]]$typ <- 2
                    }
                  }
                }
                
                #only allow one hit on a double, split aces only receive one card, and player has 21
                if(dec == "D" |
                   (l > 1 & p[[k]]$h[[l]]$val[1] == 1 & p[[k]]$h[[l]]$val[2] != 1) |
                   (p[[k]]$h[[l]]$tot == 21 | 
                    (p[[k]]$h[[l]]$tot == 11 & p[[k]]$h[[l]]$typ %in% c(3, 7)))) {
                  active_p <- 1
                  break
                }
                
                #determine decision
                dec <- sp_vec[[k]][sum(dec_length_cumsum[p[[k]]$h[[l]]$typ - 1]) + 
                                     r7_length * 10 * (p[[k]]$h[[l]]$tot - dec_minval[p[[k]]$h[[l]]$typ]) / dec_div[p[[k]]$h[[l]]$typ] +
                                     r7_length * (d$val[1] - 1) + r7 - r7_min + 1]
                
                #split
                if(dec == "P") {
                  for(n in 1:2) {
                    nHands <- nHands + 1
                    
                    #create new hands
                    p[[k]]$h[[nHands]] <- list(bet = p[[k]]$h[[l]]$bet,
                                               val = p[[k]]$h[[l]]$val[1],
                                               typ = NULL,
                                               tot = NULL,
                                               res = NA)
                  }
                  
                  #cancel bet on original split hand
                  p[[k]]$h[[l]]$bet <- 0
                  p[[k]]$h[[l]]$res <- 0
                  break
                } else {
                  
                  #hit or double
                  if(dec == "H" | dec == "D") {
                    p[[k]]$h[[l]]$val <- c(p[[k]]$h[[l]]$val, shoe_val[dlt + 1])
                    r7 <- r7 + shoe_r7[dlt + 1]
                    dlt <- dlt + 1
                    
                    #double bet
                    if(dec == "D") {
                      p[[k]]$h[[l]]$bet <- p[[k]]$h[[l]]$bet * 2
                    }
                  } else {
                    active_p <- 1
                    break
                  }
                }
              }
              l <- l + 1
            }
          }
        }
      }
      
      #if players are still active, play dealer hand
      if(active_p == 1) {
        repeat {
          
          #calculate dealer total and type
          d$tot <- sum(d$val)
          if(1 %in% d$val & d$tot <= 11) {
            d$typ <- 2
          } else {
            d$typ <- 1
          }
          
          #hit hand if conditions are met
          if((d$typ == 1 & d$tot >= 17) | 
             (d$typ == 2 & ((d$tot >= 8 & H17 == 1) |
                            (d$tot >= 7 & H17 == 0)))) {
            break
          } else {
            d$val <- c(d$val, shoe_val[dlt + 1])
            r7 <- r7 + shoe_r7[dlt + 1]
            dlt <- dlt + 1
          }
        }
        
        #settle all bets
        for(k in 1:n_players) {
          for(l in 1:length(p[[k]]$h)) {
            if(is.na(p[[k]]$h[[l]]$res)) {
              if(p[[k]]$h[[l]]$typ %in% c(3, 7, 13)) {
                tot_p <- p[[k]]$h[[l]]$tot + 10
              } else {
                tot_p <- p[[k]]$h[[l]]$tot
              }
              if(d$typ == 2) {
                tot_d <- d$tot + 10
              } else {
                tot_d <- d$tot
              }
              
              #player hand wins, loses, or pushes
              if(tot_d > 21 | tot_p > tot_d) {
                p[[k]]$h[[l]]$res <- p[[k]]$h[[l]]$bet
              } else {
                if(tot_p < tot_d) {
                  p[[k]]$h[[l]]$res <- -p[[k]]$h[[l]]$bet
                } else {
                  p[[k]]$h[[l]]$res <- 0
                }
              }
            }
          }
        }
      }
      
      #calculate total result for each player
      for(k in 1:n_players) {
        out_p[k] <- out_p[k] + sum(sapply(p[[k]]$h, "[[", "res"), p[[k]]$i_res, na.rm = TRUE)
      }
      
      #add second dealer card to count
      r7 <- r7 + dealer2_r7
    }
  }
  return(out_p)
}



