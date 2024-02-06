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
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}



#' @title
#'  Contestant selects a door
#' @description
#'  `select_door()` This function was created to simulate a contestant randomly
#'  choosing one of the three doors.
#' @details
#'  First, a vector of three doors numbered 1,2,3 are created. Then one of those
#'  doors will be randomly selected using `sample()`. Finally, the function will
#'  return the randomized door selection.
#' @param
#'  "Doors" A numeric vector with values 1, 2,and 3, representing the three doors.
#' @param
#'  "A.pick" is chosen through the sample function which takes a random sample
#'  of 1 of the 3 doors.
#' @return
#'  The function returns a number between 1 and 3 indicating which door the
#'  contestant has selected.
#' @examples
#'  select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}


#' @title
#'  Host opens goat door
#' @description
#'  `open_goat_door()` this function was created to select a door that is not a
#'  car and is not the contestant's current door selection.
#' @details
#'  First, a vector of three doors numbered 1,2,3 are created. Then two control
#'  "If" statements are utilized in order to make sure that the host will choose
#'  a door with a goat behind it taking into account which door the contestant
#'  has already randomly selected.
#'  Case 1: If the contestant picks the door with the car, the host picks either
#'  of the 2 doors with goats. Case 2: If the contestant pick the door with a
#'  goat, the host picks the door that does not contain the car and is not the
#'  picked door. return which door the host opens.
#' @param
#'  "Doors" A numeric vector with values 1, 2,and 3, representing the three doors.
#' @param
#' "If statement" if contestant selected door with car, then the opened door is not the car door
#' and instead one of the goat doors is chosen randomly.
#' @param
#' "If statement" If contestant selected door with a goat, then the opened door is the door that
#' does not contain the car and is not the contestant's selected door.
#' @return
#'  The function returns a number between 1 and 3 indicating which door the host
#'  opens.
#' @examples
#' open_goat_door()
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

#' UNIT TEST A

#' can only be door 3
this.game <- c( "goat", "car", "goat" )
my.initial.pick <- 1
open_goat_door( this.game, my.initial.pick )

#' can only be door 1
this.game <- c( "goat", "car", "goat" )
my.initial.pick <- 3
open_goat_door( this.game, my.initial.pick )

#'
#'
#' @title
#'  Changing doors option
#' @description
#'  `open_goat_door()` This function was created to simulate the contestant's
#'  game-playing strategy if they decided to choose another door that is still
#'  closed.
#' @details
#'  First, a vector of three doors numbered 1,2,3 are created. Then two control
#'  "If" statements return the contestant's final door selected based on whether
#'  the contestant chooses to stay with their original selected door or choose
#'  the other door that the host did not select.
#' @param
#'  Input numeric vector
#' @param
#' If statement, if contestant chooses to stay. Final pick is equal to a.pick.
#' @param
#' If statement, if contestant chooses to not stay. Final pick is the door that
#' is not the open_goat_door and not a.pick in previous step.
#' @return
#' The function returns a number between 1 and 3 indicating which door the
#' contestant has decided to switch their selection to.
#' @examples
#' change_door()
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

#' UNIT TEST B

this.game <- c("goat","car","goat")
my.initial.pick <- 2

opened.door <- open_goat_door( this.game, my.initial.pick )

change_door( stay=F,
             opened.door=opened.door,
             a.pick=my.initial.pick )

my.final.pick <- change_door( stay=F,
                              opened.door=opened.door,
                              a.pick=my.initial.pick )

#' my.final.pick can only be the door that the host has not opened and
#' the door that the contestant did not initially pick.
this.game
my.initial.pick
opened.door
my.final.pick


#' @title
#'  Determine if contestant has won
#' @description
#'  `determine_winner()` This function was created in order to determine whether
#'  the contestant chose the correct door with the car behind it and win the
#'  game.
#' @details
#'  Using the control "If" statement, if the contestant's final selection is
#'  the door with the car behind it then they have won the game. If the
#'  contestant's final selection is the door with the goat behind it then they
#'  lose the game.
#' @param
#'  If statement, if the string of the final door selection equals car, then
#'  return the string, win.
#' @param
#'  If statement, if the string of the final door selection equals goat, then
#'  return the string, lose.
#' @return
#'  The function returns string, "WIN" if contestant's final door selection has
#'  a car. The function returns string, "LOSE" if contestant's final door
#'  selection has a goat behind it.
#' @examples
#'   determine_winner()
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

#' UNIT TEST C
#'
this.game <- c("goat","car","goat")
my.initial.pick <- 2
opened.goat.door <- open_goat_door( this.game, my.initial.pick )

my.final.pick.stay <- change_door( stay=T,
                                   opened.door=opened.door,
                                   a.pick=my.initial.pick )

game.outcome.stay <- determine_winner( final.pick=my.final.pick,
                                       game=this.game )

my.final.pick.switch <- change_door( stay=F,
                                     opened.door=opened.door,
                                     a.pick=my.initial.pick )

game.outcome.switch <- determine_winner( final.pick=my.final.pick,
                                         game=this.game )

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
#'  Simulation Set-Up
#' @description
#' `play_game()` this function was created so that all of the functions that were
#' created earlier would be all executed together to simulate a game from start
#' to finish.
#' @details
#' First, a new game is created and the contestant's first door selection is
#' recorded in "first.pick". Then the host opens another door with a goat behind
#' it. Then both choices are recorded if the contestant chose to stay or switch.
#' The door is revealed and if it is a car they win, if it is a goat then they
#' lose. The results of this game are recorded and returned in a table.
#' @param
#'  When the function is called, a new game is created and a door representing
#'  the contestant's pick is randomly selected with the select_door() function.
#'  Then, the function open_goat_door selects the host's door that is neither
#'  a door with the car or the contestant's door.
#' @param
#'  final.pick.stay and final.pick.switch is based on whether the contestant
#'  chooses to stay with their original picked door "True" or switch to another
#'  door "False".
#' @param
#'  outcome.stay and outcome.switch record whether the contestant "won" the car
#'  or "lost" if the door had a goat behind it.
#' @param
#'  The game strategy and win/lost results from this game are then input into
#'  a dataframe.
#' @return
#'  Returns a dataframe containing 2 columns labeled strategy and outcome. The first
#'  row returns the game results "Win" or "lose" if the contestant stayed with
#'  their selection. The second row returns the game results "win" or lose" if
#'  the contestant switched their door selection.
#' @examples
#'  play_game()
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


#' @title
#' Play the game 100 times
#' @description
#'  `play_game()` this function was created in order to automatically run
#'  100 games and receive the results of those 100 games. The results are
#'  recorded and returned in a table to understand which game strategy produces
#'  more "Wins".
#' @details
#'  With inferential simulations, best practice is to simulate the game
#'  numerous times and record each outcome. This ensures that the simulated
#'  statistical outcomes converge closely to the theoretical values.
#'  The average proportion of wins achieved by each strategy in this scenario
#'  are returned.
#' @param
#' When the function is called a results.list is created which will store all 100
#' game results. Then the game will run through 100 iterations and the results
#' will be stored in a table which contains the proportions of wins and losses
#' by game strategy.
#' @return
#' returns a table which contains the proportion of each game-strategy's outcome.
#' @examples
#'  play_n_games()
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
