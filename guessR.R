

stage <- function(){
        hush <- function(code){
                sink("NUL") # use /dev/null in UNIX
                tmp = code
                sink()
                return(tmp)
        }
        num = 5
        loop = 1
        stage = 1
        count = 1
        gamble_bet <- function(seed, value, n = 5){
                if(!is.null(value)){
                        set.seed(seed)
                        master_pool <- sample.int(1000, n)
                        #cat(master_pool)
                        master_guess = sample(master_pool, 1)
                        return(master_guess)
                        
                } else {
                        set.seed(seed)
                        master_pool <- sample.int(1000, n)
                        cat(master_pool)
                        master_guess = sample(master_pool, 1)
                        user_guess <- readline("Your guess? ")
                        if (user_guess %in% master_guess){
                                message("Awesome! You got it right.")
                        } else {
                                stop("Try again!")
                        }
                }
                
        }
        
        #amount_staked = readline("How much do you wanna stake? ") %>% as.numeric()
        
        retry <- function(){
                retry <- readline("Try again? (Y/N): ")
                if (retry %in% c("Y", "y")){
                        message("\nLet's do this!")
                        # amount_staked = readline("How much do you wanna stake? ")
                        # amount_staked = as.numeric(amount_staked)
                        # odd_seed <- sample.int(1000,1)
                        # level_odd(loop, odd_seed)
                        return(level())
                } else if (retry %in% c("N", "n")) {
                        neg_response <- c("Sore loser", "Coward", "Pussy", "Olodo", "Cry baby")
                        message("\n",neg_response[sample.int(length(neg_response), 1)], "!")
                        return(FALSE)
                        break
                }
        }
        
        game_loop <- function(n){
                
                attempt = 2
                seed = sample.int(1000, 1)
                while (attempt >= 0){
                        #trial = attempt
                        output <- tryCatch(
                                expr={ gamble_bet(seed, NULL, n) },
                                error=function(e) { return(e) }
                        )
                        
                        if(is(output, "error")) {
                                if(attempt > 1){
                                        message(output$message)
                                        message("You have ", attempt, " more attempts")
                                } else if(attempt == 1) {
                                        message(output$message)
                                        message("You have ", attempt, " more attempt")
                                } else {
                                        message("Sorry, you have no more attempts")
                                        cat("\nRight answer: ", gamble_bet(seed, value = 1, n),"\n")
                                        return(retry())
                                }
                        } else {
                                if(attempt >= 2){
                                        message(output$message)
                                        message("You got it right in 1 guess")
                                        
                                } else {
                                        message(output$message)
                                        message("You got it right in ", 3 - attempt, " guesses")
                                        
                                }
                                return(TRUE)
                                break
                        }
                        attempt = attempt - 1
                }
                cat("\n")
        }
        
        level_odd <- function(loop, odd_seed){
                set.seed(odd_seed)
                if(loop == 1 && stage == 1){
                        odd = round(runif(1, 1.1, 2.2),2)
                } else if(loop == 2 && stage == 1){
                        odd = round(runif(1, 2.2, 2.4),2)
                } else if(loop == 3 && stage == 1){
                        odd = round(runif(1, 2.4, 2.6),2)
                } else if(loop == 4 && stage == 1){
                        odd = round(runif(1, 2.6, 2.8),2)
                } else if(loop == 5 && stage == 1){
                        odd = round(runif(1, 2.8, 3),2)
                }
                cat("\nLevel", loop, "| Odds:", odd,"\n\n")
                return(odd)
        }
        
        level <- function(){
                amount_staked = readline("How much do you wanna stake? ") ## Enter stake amount
                amount_staked = as.numeric(amount_staked) # Convert to numeric variable
                message("You're going home richer today. (Maybe)")
                
                odd_seed <- sample.int(1000,1)
                odds = hush(level_odd(loop, odd_seed))
                odds = c(odds, level_odd(loop, odd_seed))
                proceed = game_loop(num)
                if(proceed){
                        message("Cashout: ", odds[length(odds)] * amount_staked)
                } else {
                        message("Cashout: ", odds[length(odds)-1] * 0)
                        try(stop(), silent = T)
                }
                
                while(count <= 5){
                        #stake()
                        #level_odd(loop)
                        #proceed = game_loop(num)
                        #loop = loop + 1
                        if (proceed){
                                odd_seed <- sample.int(1000,1)
                                loop = loop + 1
                                # odds = level_odd(loop, odd_seed)
                                odds = c(odds, level_odd(loop, odd_seed))
                                num = num + 1
                                proceed2 = game_loop(num)
                                if (proceed2){
                                        message("Cashout: ", odds[length(odds)] * amount_staked)
                                } else {
                                        message("Cashout: ", odds[length(odds)-1] * amount_staked)
                                        try(stop(), silent = T)
                                        break
                                }
                                
                                #next
                        } else {
                                break
                        }
                        count = count + 1
                }
        }
        
        level()
}



#cashout <- message("Cashout: ", odd * hush(s))


