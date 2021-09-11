#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()will return one of the
#'   following combinations:
#'   [1] "car"  "goat" "goat"
#'   [1] "goat" "goat" "car"
#'   [1] "goat" "car" "goat"
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}

# create_game test
# try three times to see randomization
create_game()
create_game()
create_game()

#' @title Select the contestant's first pick of doors
#' @description the contestants first pick of a door, numbered 1-3.
#' randomly assigns a number 1-3 as the contestant's first pick
#' @details all three doors are still closed and available
#'      to the contestant
#' @param no arguments are used in select_door
#' @return the function returns a randomly generated number
#'      (1-3) indicating the door which the contestant first
#'      "picked"
#'
#'      the function will place the number into a.pick
#'
#' @examples select_door will return one of the following:
#'      [1] 1
#'      [1] 2
#'      [1] 3
#'
#' @export
#'
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}

# select_door test
# test the function
select_door()
select_door()
select_door()


#' @title
#'      Open a Goat Door
#' @description
#'      opens a door not selected by the contestant
#'      or a car to reveal a goat. This leaves 2 doors
#'      unopened
#' @details
#'      this function first checks if the contestant's
#'      pick is a car by using an IF statement.
#'      If the contestant has selected the car then the
#'      function will randomly select one of the remaining
#'      doors to reveal a goat.
#'
#'      If the contestant has selected one of the two doors
#'      with a goat, the function places a number (1,2,or3)
#'      that does not contain car or the contestant's pick
#'      into opened.door
#'
#' @param
#'      arguments in this function are:
#'      game: randomly assigns goat, goat, and car to doors 1,2,&3
#'      a.pick: randomly assigned integer of 1,2, or 3 as
#'      the contestant's first pick
#'
#' @return
#'      goat_door will return a number that is not assigned
#'      to the car or to the contestant. the number will reveal
#'      a goat.
#'
#'      the number will be placed in goat.door
#' @examples
#'      returns one of the following:
#'      [1] 1
#'      [1] 2
#'      [1] 3
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}


# open_goat_door test
this.game <- create_game()
this.game
my.initial.pick <- select_door()
my.initial.pick
open_goat_door( this.game, my.initial.pick )



#' @title Change Doors?
#' @description
#'      change_door allows the contestant the opportunity to
#'      switch the initially assigned door with the one
#'      remaining door opened
#' @details
#'      the function receives stay=T or stay=F
#'
#'      if stay=T the function stores the integer stored in
#'      a.pick in final.pick
#'
#'      if stay=F the function stores the door that is NOT
#'      the door determined by open_goat_door (goat.door)
#'      or the door that determined by select_door (a.pick)
#'      and stores the remaining integer (door) in final.pick
#'
#' @param
#'      stay=T or stay=F: a character signifying TRUE or FALSE
#'      to the contestant's choice of keeping their current
#'      door (stay=T), or switching doors with the host (stay=F)
#'
#'      open.door- an integer (1:3) returned from open_goat_door
#'      that signifies the goat door opened and removed from
#'      gameplay
#'
#'      a.pick- an integer (1:3) returned as the contestant's
#'      initial pick
#'
#' @return
#'      stores one of the following in final.pick:
#'           [1] 1
#'           [1] 2
#'           [1] 3
#' @examples
#'      change_door
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}

# change_door test
opened.door <- open_goat_door( this.game, my.initial.pick )

change_door( stay=T,
             opened.door=opened.door,
             a.pick=my.initial.pick )
change_door( stay=F,
             opened.door=opened.door,
             a.pick=my.initial.pick )

my.final.pick <- change_door( stay=F,
                              opened.door=opened.door,
                              a.pick=my.initial.pick )

this.game
my.initial.pick
my.final.pick

#' @title
#'      Determine game outcome
#' @descriptions
#'      determines if the contestant has won or lost by
#'      by revealing if a car (WIN) or goat (LOSE) is behind
#'      their door
#' @details
#'      the function compares the value to final.door from
#'      change_doors against the values "car" and "goat"
#'
#'      if there is a match with the door and "car" then the
#'      function returns "WIN"
#'
#'      if there is a match with the door and "goat" then
#'      the function returns "LOSE"
#' @param
#'      final.pick: the integer returned from change_doors
#'
#'      game: the initial set up of the game some
#'      combination of "goat","goat","car"
#' @return
#'      [1] wIN
#'      [1] LOSE
#'
#' @examples
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}


#determine_winner test
this.game
my.initial.pick

my.final.pick <- change_door( stay=T,
                              opened.door=opened.door,
                              a.pick=my.initial.pick )

determine_winner( final.pick=my.final.pick,
                  game=this.game )

my.final.pick <- change_door( stay=F,
                              opened.door=opened.door,
                              a.pick=my.initial.pick )

determine_winner( final.pick=my.final.pick,
                  game=this.game )

#' @title
#'      Play a game
#' @description
#'      goes through all of the steps (functions) that are
#'      in the monty hall game
#' @details
#'      one iteration of the entire game and will return the
#'      outcome of the game
#' @param
#'
#' @return
#'      [1] WIN
#'      [1] LOSE
#' @examples
#'      play_game
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}

# one iteration of play_game test
# game "recipe"
this.game <- create_game()
my.initial.pick <- select_door()
opened.goat.door <- open_goat_door( this.game, my.initial.pick )

# save results for both strategies for the game
my.final.pick.stay <- change_door( stay=T,
                                   opened.door=opened.goat.door,
                                   a.pick=my.initial.pick )
my.final.pick.switch <- change_door( stay=F,
                                     opened.door=opened.goat.door,
                                     a.pick=my.initial.pick )

# print game details and if you won

# if you stayed:
paste0( "GAME SETUP" )
this.game
paste0( "My initial selection: ", my.initial.pick )
paste0( "The opened goat door: ", opened.goat.door )
paste0( "My final selection: ", my.final.pick.stay )
paste0( "GAME OUTCOME:" )
determine_winner( final.pick=my.final.pick.stay,
                  game=this.game )

# if you switched:
paste0( "GAME SETUP" )
this.game
paste0( "My initial selection: ", my.initial.pick )
paste0( "The opened goat door: ", opened.goat.door )
paste0( "My final selection: ", my.final.pick.switch )
paste0( "GAME OUTCOME:" )
determine_winner( final.pick=my.final.pick.switch,
                  game=this.game )


#' @title
#'      Play a number of games
#' @description
#'      plays the full version of the games a number of times
#'      initially set at 100 games
#' @details
#'      plays the game and stores the results in a list named
#'      results.list. this is repeated via a loop
#'      n number of times.
#'
#'      the results of the games are stored in a dataframe
#'      called results.df
#'
#'
#' @param
#'      the function can run for any integer of gameplays.
#'      n=100 initially sets the game at 100 plays
#' @return
#'      the outcome of all games returned as a dataframe
#'      table
#' @examples
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}


#test play_n_games


create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}


#'
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}


open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats
  if( game[ a.pick ] == "car" )
  {
    goat.doors <- doors[ game != "car" ]
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  {
    opened.door <- doors[ game != "car" & doors != a.pick ]
  }
  return( opened.door ) # number between 1 and 3
}



#
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3)

  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ]
  }

  return( final.pick )  # number between 1 and 3
}


#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}


#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}

# one iteration of play_game test


# your game "recipe"
this.game <- create_game()
my.initial.pick <- select_door()
opened.goat.door <- open_goat_door( this.game, my.initial.pick )

# save results for both strategies for the game
my.final.pick.stay <- change_door( stay=T,
                                   opened.door=opened.goat.door,
                                   a.pick=my.initial.pick )
my.final.pick.switch <- change_door( stay=F,
                                     opened.door=opened.goat.door,
                                     a.pick=my.initial.pick )

# print game details and if you won

# if you stayed:
paste0( "GAME SETUP" )
this.game
paste0( "My initial selection: ", my.initial.pick )
paste0( "The opened goat door: ", opened.goat.door )
paste0( "My final selection: ", my.final.pick.stay )
paste0( "GAME OUTCOME:" )
determine_winner( final.pick=my.final.pick.stay,
                  game=this.game )

# if you switched:
paste0( "GAME SETUP" )
this.game
paste0( "My initial selection: ", my.initial.pick )
paste0( "The opened goat door: ", opened.goat.door )
paste0( "My final selection: ", my.final.pick.switch )
paste0( "GAME OUTCOME:" )
determine_winner( final.pick=my.final.pick.switch,
                  game=this.game )


#' @title
#'      Play a number of games
#' @description
#'      plays the full version of the games a number of times
#'      initially set at 100 games
#' @details
#'      plays the game and stores the results in a list named
#'      results.list. this is repeated via a loop
#'      n number of times.
#'
#'      the results of the games are stored in a dataframe
#'      called results.df
#'
#'
#' @param
#'      the function can run for any integer of gameplays.
#'      n=100 initially sets the game at 100 plays
#' @return
#'      the outcome of all games returned as a dataframe
#'      table. for example:
#'
#'              outcome
#'              strategy LOSE  WIN
#'              stay   0.76 0.24
#'              switch 0.24 0.76
#' @examples
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>%
    print()

  return( results.df )

}


#play_n_games test

this.game <- create_game()
my.initial.pick <- select_door()
opened.goat.door <- open_goat_door( this.game, my.initial.pick )

# save results for both strategies for the game
my.final.pick.stay <- change_door( stay=T,
                                   opened.door=opened.goat.door,
                                   a.pick=my.initial.pick )
my.final.pick.switch <- change_door( stay=F,
                                     opened.door=opened.goat.door,
                                     a.pick=my.initial.pick )

play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>%
    print()

  return( results.df )

}
play_n_games(n=100)
