# Load Shiny
library(shiny)

#############################################
# Utility Functions
#############################################

calculate_hand_value <- function(hand) {
  non_aces <- sum(hand[hand != 11])
  aces <- sum(hand == 11)
  total <- non_aces + aces
  for (i in seq_len(aces)) {
    if (total + 10 <= 21) total <- total + 10
  }
  is_soft <- (total != (non_aces + aces)) && total <= 21
  list(value = total, is_soft = is_soft)
}

is_blackjack <- function(hand) {
  length(hand) == 2 && calculate_hand_value(hand)$value == 21
}

get_recommendation <- function(player_hand, dealer_upcard,
                               can_split, can_double, can_surrender,
                               is_split_aces = FALSE) {
  if (is_split_aces) return("S")
  phv <- calculate_hand_value(player_hand)
  player_val <- phv$value; is_soft <- phv$is_soft
  if (can_split && length(player_hand) == 2 && player_hand[1] == player_hand[2]) {
    card_val <- player_hand[1]
    if (card_val == 11) return("SP")
    if (card_val %in% c(8,9)) return("SP")
    if (card_val %in% c(2,3) && dealer_upcard %in% 4:7) return("SP")
    if (card_val == 6 && dealer_upcard %in% 2:6) return("SP")
    if (card_val == 7 && dealer_upcard %in% 2:7) return("SP")
  }
  if (can_surrender && length(player_hand) == 2) {
    if (player_val == 16 && dealer_upcard %in% c(9,10,11)) return("SU")
    if (player_val == 15 && dealer_upcard == 10) return("SU")
  }
  if (can_double && length(player_hand) == 2) {
    if (!is_soft && player_val %in% c(9,10,11)) return("D")
    if (is_soft && player_val %in% c(17,18) && dealer_upcard %in% 3:6) return("D")
  }
  if (is_soft) {
    if (player_val <= 17) return("H")
    if (player_val == 18) {
      if (dealer_upcard %in% c(9,10,11)) return("H") else return("S")
    }
    return("S")
  } else {
    if (player_val <= 8) return("H")
    if (player_val == 9) {
      if (dealer_upcard %in% 3:6 && can_double) return("D") else return("H")
    }
    if (player_val == 10) {
      if (dealer_upcard %in% 2:9 && can_double) return("D") else return("H")
    }
    if (player_val == 11 && can_double) return("D")
    if (player_val == 12) {
      if (dealer_upcard %in% 4:6) return("S") else return("H")
    }
    if (player_val >= 17) return("S")
    if (player_val >= 13 && player_val <= 16) {
      if (dealer_upcard <= 6) return("S") else return("H")
    }
    return("H")
  }
}

get_hint_message <- function(player_hand, dealer_upcard,
                             can_split, can_double, can_surrender,
                             is_split_aces = FALSE) {
  rec <- get_recommendation(player_hand, dealer_upcard,
                            can_split, can_double, can_surrender,
                            is_split_aces)
  hv <- calculate_hand_value(player_hand)$value
  up <- ifelse(dealer_upcard == 11, "A", dealer_upcard)
  action_text <- switch(rec, H="Hit", S="Stand", D="Double",
                        SP="Split", SU="Surrender", rec)
  paste0("With a total of ", hv, " vs dealer’s ", up,
         ", ", action_text, " is recommended.")
}

dealer_play <- function(dealer_hand, deck, dealer_hits_soft_17 = TRUE) {
  repeat {
    dhv <- calculate_hand_value(dealer_hand)
    if (dhv$value > 21) break
    if (dhv$value > 17) break
    if (dhv$value == 17 && dhv$is_soft && !dealer_hits_soft_17) break
    if (length(deck) < 1) break
    dealer_hand <- c(dealer_hand, deck[1])
    deck <- deck[-1]
  }
  list(dealer_hand = dealer_hand, deck = deck)
}

format_card <- function(card) {
  if (card == 11) "A" else if (card == 10) "10" else as.character(card)
}

ui <- fluidPage(
  tags$head(tags$style(HTML(
    ".card { display:inline-block; border:1px solid #333; border-radius:8px;
                padding:5px 10px; margin:2px; font-weight:bold; background:#fff; }
     .card.A { color:red; }"
  ))),
  titlePanel("Blackjack Simulator"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      numericInput("num_decks", "Number of Decks:", 1, 1, 8),
      checkboxInput("dealer_hit_soft17", "Dealer hits on soft 17?", TRUE),
      numericInput("max_splits", "Maximum Splits Allowed:", 3, 1, 6),
      hr(),
      actionButton("deal", "Deal New Hand"),
      actionButton("hit", "Hit"),
      actionButton("stand", "Stand"),
      actionButton("double_btn", "Double Down"),
      actionButton("split_btn", "Split"),
      actionButton("insurance_btn", "Take Insurance"),
      actionButton("surrender_btn", "Surrender"),
      actionButton("reset_btn", "Reset Bank & Log"),
      checkboxInput("show_hints", "Show Hints", FALSE)
    ),
    mainPanel(
      h4("Dealer’s Hand"), uiOutput("dealer_hand_ui"),
      h4("Player’s Hands"), uiOutput("player_hands_ui"),
      h4("Hint"), textOutput("hint"),
      hr(),
      h4("Score"), verbatimTextOutput("score"),
      hr(),
      h4("Game Log"), verbatimTextOutput("game_log")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    deck = NULL, dealer_hand = NULL,
    player_hands = list(), active_hand_index = 1,
    game_state = "idle", bank = 1000, base_bet = 10,
    insurance_bet = 0, insurance_offered = FALSE,
    insurance_taken = FALSE, wins = 0, losses = 0,
    pushes = 0, game_log = character(0)
  )
  
  update_log <- function(msg) {
    rv$game_log <- c(rv$game_log, paste(Sys.time(), "-", msg))
  }
  
  shuffle_deck <- function(n_decks) {
    single <- c(2:10,10,10,10,11)
    rep(sample(rep(single, 4)), n_decks)
  }
  
  reset_game <- function() {
    rv$deck <- shuffle_deck(input$num_decks)
    rv$dealer_hand <- NULL
    rv$player_hands <- list()
    rv$active_hand_index <- 1
    rv$game_state <- "idle"
    rv$insurance_offered <- FALSE
    rv$insurance_taken <- FALSE
    rv$insurance_bet <- 0
  }
  
  observeEvent(input$reset_btn, {
    rv$bank <- 1000; rv$wins <- 0; rv$losses <- 0; rv$pushes <- 0
    rv$game_log <- character(0)
    reset_game(); update_log("Bankroll and log reset.")
  })
  
  output$dealer_hand_ui <- renderUI({
    if (is.null(rv$dealer_hand)) return(tags$p("No hand yet"))
    cards <- rv$dealer_hand
    hidden <- rv$game_state %in% c("player_turn","playing")
    display <- if (hidden) c(cards[1], NA) else cards
    spans <- lapply(display, function(c) {
      if (is.na(c)) tags$span(class="card", "?") else {
        txt <- format_card(c)
        tags$span(class=ifelse(txt=="A","card A","card"), txt)
      }
    })
    total <- calculate_hand_value(rv$dealer_hand)$value
    total_text <- if (hidden) "?" else total
    tagList(spans, tags$span(paste0("(Total = ", total_text, ")")))
  })
  
  output$player_hands_ui <- renderUI({
    if (length(rv$player_hands) == 0) return(tags$p("No player hands yet."))
    hands_ui <- lapply(seq_along(rv$player_hands), function(i) {
      hinfo <- rv$player_hands[[i]]
      val <- calculate_hand_value(hinfo$cards)$value
      spans <- lapply(hinfo$cards, function(c) {
        txt <- format_card(c)
        tags$span(class=ifelse(txt=="A","card A","card"), txt)
      })
      tags$div(
        style = if (rv$active_hand_index==i && rv$game_state=="player_turn") "font-weight:bold;" else NULL,
        tags$strong(paste0("Hand #", i, ": ")), tagList(spans),
        tags$span(paste("(Total =", val, ")"))
      )
    })
    tagList(hands_ui)
  })
  
  output$hint <- renderText({
    if (!input$show_hints || rv$game_state != "player_turn") return("")
    hinfo <- rv$player_hands[[rv$active_hand_index]]
    get_hint_message(hinfo$cards, rv$dealer_hand[1],
                     (hinfo$split_count < input$max_splits) && length(hinfo$cards)==2,
                     length(hinfo$cards)==2,
                     length(hinfo$cards)==2,
                     hinfo$is_split_aces)
  })
  
  output$game_log <- renderText({ paste(rv$game_log, collapse="\n") })
  output$score <- renderText({
    paste0("Bank: $", rv$bank,
           "  Wins: ", rv$wins,
           "  Losses: ", rv$losses,
           "  Pushes: ", rv$pushes)
  })
  
  # Observe Deal/Hit/Stand/Double/Split/Insurance/Surrender (unchanged except next_hand_or_dealer)
  observeEvent(input$deal, {
    reset_game()
    if (length(rv$deck) < 4) {
      update_log("Not enough cards to deal. Reshuffling.")
      reset_game()
    }
    # Deduct base bet (for the first hand)
    if (rv$bank < rv$base_bet) {
      showModal(modalDialog(
        title = "Not enough funds",
        "You do not have enough bank to place a bet.",
        easyClose = TRUE
      ))
      return()
    }
    rv$bank <- rv$bank - rv$base_bet
    
    # Deal
    rv$player_hands <- list(
      list(
        cards = rv$deck[1:2],
        is_split_aces = FALSE,
        is_resolved = FALSE,
        bet = rv$base_bet,
        split_count = 0
      )
    )
    rv$dealer_hand <- rv$deck[3:4]
    rv$deck <- rv$deck[-(1:4)]
    rv$game_state <- "player_turn"
    rv$active_hand_index <- 1
    
    update_log("New hand dealt.")
    
    # Check immediate blackjack
    playerBJ <- is_blackjack(rv$player_hands[[1]]$cards)
    dealerBJ <- is_blackjack(rv$dealer_hand)
    
    # Offer insurance if dealer’s upcard is Ace (and no immediate dealer BJ)
    if (rv$dealer_hand[1] == 11 && !dealerBJ && !playerBJ) {
      rv$insurance_offered <- TRUE
      showNotification("Dealer shows Ace. Click 'Take Insurance' if you want it.", type = "message")
    }
    
    if (playerBJ || dealerBJ) {
      rv$game_state <- "over"
      if (playerBJ && dealerBJ) {
        # Push
        update_log("Both player and dealer have Blackjack -> Push.")
        showModal(modalDialog(
          title = "Push",
          "Both you and the dealer have Blackjack!",
          easyClose = TRUE
        ))
        # Return bet
        rv$bank <- rv$bank + rv$base_bet
        rv$pushes <- rv$pushes + 1
      } else if (playerBJ) {
        # Player BJ
        update_log("Player Blackjack! Pays 3:2.")
        showModal(modalDialog(
          title = "Blackjack!",
          "You have Blackjack! You win 3:2 payout.",
          easyClose = TRUE
        ))
        rv$wins <- rv$wins + 1
        rv$bank <- rv$bank + rv$base_bet * 2.5
      } else {
        # Dealer BJ
        update_log("Dealer Blackjack! Player loses.")
        showModal(modalDialog(
          title = "Dealer Blackjack",
          "Dealer has Blackjack. You lose your bet.",
          easyClose = TRUE
        ))
        rv$losses <- rv$losses + 1
        
        # Insurance?
        if (rv$insurance_taken) {
          # Insurance pays 2:1. Player paid insurance_bet, so net is +2 * insurance_bet
          rv$bank <- rv$bank + rv$insurance_bet * 3
          update_log("Insurance bet paid out 2:1.")
        }
      }
      # If user took insurance but dealer did NOT have BJ, that bet is lost
      if (rv$insurance_taken && !dealerBJ) {
        update_log("Insurance bet lost (dealer had no Blackjack).")
      }
    }
  })
  
  # Hit
  observeEvent(input$hit, {
    req(rv$game_state == "player_turn")
    if (rv$active_hand_index > length(rv$player_hands)) return()
    
    hand_info <- rv$player_hands[[rv$active_hand_index]]
    # If it’s split Aces and the house rule is only 1 card each, do not allow hitting
    if (hand_info$is_split_aces && length(hand_info$cards) == 2) {
      showNotification("Cannot hit split Aces (only one card allowed).", type = "error")
      return()
    }
    
    # Check recommended
    rec <- get_recommendation(
      hand_info$cards,
      rv$dealer_hand[1],
      can_split = FALSE,
      can_double = FALSE,
      can_surrender = FALSE,
      is_split_aces = hand_info$is_split_aces
    )
    if (rec != "H") {
      update_log(paste("Player chose Hit; recommended was", rec))
      showNotification("Not optimal!", type = "error")
    } else {
      update_log("Player chose Hit.")
    }
    
    # Draw card
    if (length(rv$deck) < 1) {
      update_log("Deck exhausted while hitting. Reshuffling.")
      reset_game()
      return()
    }
    new_card <- rv$deck[1]
    rv$deck <- rv$deck[-1]
    rv$player_hands[[rv$active_hand_index]]$cards <- c(hand_info$cards, new_card)
    
    # Check bust
    val <- calculate_hand_value(rv$player_hands[[rv$active_hand_index]]$cards)$value
    if (val > 21) {
      update_log(paste("Player busts on hand #", rv$active_hand_index))
      showModal(modalDialog(
        title = "Bust!",
        paste("You busted on hand #", rv$active_hand_index),
        easyClose = TRUE
      ))
      rv$losses <- rv$losses + 1
      next_hand_or_dealer()
    }
  })
  
  # Stand
  observeEvent(input$stand, {
    req(rv$game_state == "player_turn")
    if (rv$active_hand_index > length(rv$player_hands)) return()
    
    hand_info <- rv$player_hands[[rv$active_hand_index]]
    rec <- get_recommendation(
      hand_info$cards,
      rv$dealer_hand[1],
      can_split = FALSE,
      can_double = FALSE,
      can_surrender = FALSE,
      is_split_aces = hand_info$is_split_aces
    )
    if (rec != "S") {
      update_log(paste("Player chose Stand; recommended was", rec))
      showNotification("Not optimal!", type = "error")
    } else {
      update_log("Player chose Stand.")
    }
    next_hand_or_dealer()
  })
  
  # Double
  observeEvent(input$double_btn, {
    req(rv$game_state == "player_turn")
    if (rv$active_hand_index > length(rv$player_hands)) return()
    
    hand_info <- rv$player_hands[[rv$active_hand_index]]
    # Must be exactly 2 cards
    if (length(hand_info$cards) != 2) {
      showNotification("You can only double on your first two cards!", type = "error")
      return()
    }
    # Check bank
    if (rv$bank < hand_info$bet) {
      showNotification("Not enough bank to double!", type = "error")
      return()
    }
    # If split Aces and house rule is only 1 card, disallow double
    if (hand_info$is_split_aces) {
      showNotification("Cannot double split Aces (house rule).", type = "error")
      return()
    }
    
    rec <- get_recommendation(
      hand_info$cards,
      rv$dealer_hand[1],
      can_split = FALSE,
      can_double = TRUE,
      can_surrender = TRUE
    )
    if (rec != "D") {
      update_log(paste("Player chose Double; recommended was", rec))
      showNotification("Not optimal!", type = "error")
    } else {
      update_log("Player chose Double.")
    }
    
    # Deduct an additional bet
    rv$bank <- rv$bank - hand_info$bet
    # Double the bet for this hand
    rv$player_hands[[rv$active_hand_index]]$bet <- hand_info$bet * 2
    
    # Deal 1 card
    if (length(rv$deck) < 1) {
      update_log("Deck exhausted while doubling. Reshuffling.")
      reset_game()
      return()
    }
    new_card <- rv$deck[1]
    rv$deck <- rv$deck[-1]
    rv$player_hands[[rv$active_hand_index]]$cards <- c(hand_info$cards, new_card)
    
    # Check bust
    val <- calculate_hand_value(rv$player_hands[[rv$active_hand_index]]$cards)$value
    if (val > 21) {
      update_log(paste("Player busts after double on hand #", rv$active_hand_index))
      showModal(modalDialog(
        title = "Bust!",
        "You busted after doubling.",
        easyClose = TRUE
      ))
      rv$losses <- rv$losses + 1
      next_hand_or_dealer()
    } else {
      update_log("Player stands automatically after double.")
      next_hand_or_dealer()
    }
  })
  
  # Split
  observeEvent(input$split_btn, {
    req(rv$game_state == "player_turn")
    if (rv$active_hand_index > length(rv$player_hands)) return()
    
    hand_info <- rv$player_hands[[rv$active_hand_index]]
    cards <- hand_info$cards
    
    # Must be exactly 2 cards, same rank
    if (length(cards) != 2 || cards[1] != cards[2]) {
      showNotification("You can only split identical ranks!", type = "error")
      return()
    }
    # Check if we exceeded max splits
    if (hand_info$split_count >= input$max_splits) {
      showNotification("You have reached the max splits allowed.", type = "error")
      return()
    }
    # Check bankroll for additional bet
    if (rv$bank < hand_info$bet) {
      showNotification("Not enough bank to split!", type = "error")
      return()
    }
    
    rec <- get_recommendation(
      cards,
      rv$dealer_hand[1],
      can_split = TRUE,
      can_double = TRUE,
      can_surrender = TRUE
    )
    if (rec != "SP") {
      update_log(paste("Player chose Split; recommended was", rec))
      showNotification("Not optimal!", type = "error")
    } else {
      update_log("Player chose Split.")
    }
    
    # Deduct another bet
    rv$bank <- rv$bank - hand_info$bet
    
    # Perform the split
    card1 <- cards[1]
    card2 <- cards[2]
    
    # Deal new cards: 1 to each hand
    if (length(rv$deck) < 2) {
      update_log("Deck exhausted while splitting. Reshuffling.")
      reset_game()
      return()
    }
    new_card_1 <- rv$deck[1]
    new_card_2 <- rv$deck[2]
    rv$deck <- rv$deck[-(1:2)]
    
    # First hand
    rv$player_hands[[rv$active_hand_index]]$cards <- c(card1, new_card_1)
    # Increase split_count
    rv$player_hands[[rv$active_hand_index]]$split_count <- hand_info$split_count + 1
    # If we split Aces, mark it
    if (card1 == 11) {
      rv$player_hands[[rv$active_hand_index]]$is_split_aces <- TRUE
    }
    
    # Second (new) hand
    second_hand <- list(
      cards = c(card2, new_card_2),
      is_split_aces = (card2 == 11),
      is_resolved = FALSE,
      bet = hand_info$bet,
      split_count = hand_info$split_count + 1
    )
    # Insert the new hand right after the current one
    rv$player_hands <- append(rv$player_hands, list(second_hand), after = rv$active_hand_index)
  })
  
  # Insurance
  observeEvent(input$insurance_btn, {
    req(rv$game_state == "player_turn")
    if (!rv$insurance_offered) {
      showNotification("Insurance is not currently offered.", type = "error")
      return()
    }
    if (rv$insurance_taken) {
      showNotification("You already took insurance.", type = "error")
      return()
    }
    # Typically insurance is half your main bet. We'll assume half of the *first hand’s* bet:
    first_bet <- rv$player_hands[[1]]$bet
    insurance_cost <- first_bet / 2
    if (rv$bank < insurance_cost) {
      showNotification("Not enough bank to buy insurance!", type = "error")
      return()
    }
    rv$bank <- rv$bank - insurance_cost
    rv$insurance_bet <- insurance_cost
    rv$insurance_taken <- TRUE
    update_log("Player took insurance.")
    showNotification("Insurance bet placed.", type = "message")
  })
  
  # Surrender
  observeEvent(input$surrender_btn, {
    req(rv$game_state == "player_turn")
    if (rv$active_hand_index > length(rv$player_hands)) return()
    
    hand_info <- rv$player_hands[[rv$active_hand_index]]
    # Must be exactly 2 cards (early surrender)
    if (length(hand_info$cards) != 2) {
      showNotification("You can only surrender on your first two cards!", type = "error")
      return()
    }
    # If it’s split Aces, often no surrender is allowed, but that’s a house rule. We'll allow it or not.
    
    rec <- get_recommendation(
      hand_info$cards,
      rv$dealer_hand[1],
      can_split = FALSE,
      can_double = FALSE,
      can_surrender = TRUE,
      is_split_aces = hand_info$is_split_aces
    )
    if (rec != "SU") {
      update_log(paste("Player chose Surrender; recommended was", rec))
      showNotification("Not optimal!", type = "error")
    } else {
      update_log("Player chose Surrender.")
    }
    # Player forfeits half the bet
    refund <- hand_info$bet / 2
    rv$bank <- rv$bank + refund
    rv$losses <- rv$losses + 1
    
    showModal(modalDialog(
      title = "Surrender",
      paste("You surrendered hand #", rv$active_hand_index, "and got back half your bet."),
      easyClose = TRUE
    ))
    next_hand_or_dealer(busted = FALSE, surrendered = TRUE)
  })
  
  next_hand_or_dealer <- function(busted = FALSE, surrendered = FALSE) {
    rv$player_hands[[rv$active_hand_index]]$is_resolved <- TRUE
    # Look for next unresolved hand
    found_next <- FALSE
    if (rv$active_hand_index < length(rv$player_hands)) {
      for (i in (rv$active_hand_index+1):length(rv$player_hands)) {
        if (!rv$player_hands[[i]]$is_resolved) {
          rv$active_hand_index <- i
          found_next <- TRUE
          break
        }
      }
    }
    if (!found_next) {
      # Dealer turn
      rv$game_state <- "playing"
      dr <- dealer_play(rv$dealer_hand, rv$deck, input$dealer_hit_soft17)
      rv$dealer_hand <- dr$dealer_hand
      rv$deck <- dr$deck
      rv$game_state <- "over"
      dealer_val <- calculate_hand_value(rv$dealer_hand)$value
      # Resolve
      for (h in seq_along(rv$player_hands)) {
        phv <- calculate_hand_value(rv$player_hands[[h]]$cards)
        pv <- phv$value; bet <- rv$player_hands[[h]]$bet
        if (pv > 21) next
        if (dealer_val > 21) {
          rv$wins <- rv$wins + 1; rv$bank <- rv$bank + bet*2
          update_log(paste("Dealer bust; hand", h, "wins"))
        } else if (pv > dealer_val) {
          rv$wins <- rv$wins + 1; rv$bank <- rv$bank + bet*2
          update_log(paste("Hand", h, "wins", pv, "vs", dealer_val))
        } else if (pv < dealer_val) {
          rv$losses <- rv$losses + 1
          update_log(paste("Dealer beats hand", h, pv, "to", dealer_val))
        } else {
          rv$pushes <- rv$pushes + 1; rv$bank <- rv$bank + bet
          update_log(paste("Push on hand", h, "with", pv))
        }
      }
    } else {
      rv$game_state <- "player_turn"
    }
  }
}

shinyApp(ui, server)


