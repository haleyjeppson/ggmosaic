#' Flying Etiquette Survey Data
#'
#' Data from the results of a SurveyMonkey
#' survey commissioned by FiveThirtyEight for the story
#' 41 Percent of Fliers Say It’s Rude To Recline Your Airplane Seat.
#'
#'
#' @format A data frame with 1040 rows and 27 variables:
#' \describe{
#'   \item{id}{Respondent ID}
#'   \item{flight_freq}{How often do you travel by plane?}
#'   \item{do_you_recline}{Do you ever recline your seat when you fly?}
#'   \item{height}{How tall are you?}
#'   \item{has_child_under_18}{Do you have any children under 18?}
#'   \item{three_seats_two_arms}{n a row of three seats, who should get to use the two arm rests?}
#'   \item{two_seats_one_arm}{In a row of two seats, who should get to use the middle arm rest?}
#'   \item{window_shade}{Who should have control over the window shade?}
#'   \item{rude_to_move_to_unsold_seat}{Is it rude to move to an unsold seat on a plane?}
#'   \item{rude_to_talk_to_neighbor}{Generally speaking, is it rude to say more than a few words to the stranger sitting next to you on a plane?}
#'   \item{six_hr_flight_leave_seat}{On a six hour flight from NYC to LA, how many times is it acceptable to get up if you're not in an aisle seat?}
#'   \item{reclining_obligation_to_behind}{Under normal circumstances, does a person who reclines their seat during a flight have any obligation to the person sitting behind them?}
#'   \item{rude_to_recline}{Is it rude to recline your seat on a plane?}
#'   \item{eliminate_reclining}{Given the opportunity, would you eliminate the possibility of reclining seats on planes entirely?}
#'   \item{rude_to_switch_seats_friends}{Is it rude to ask someone to switch seats with you in order to be closer to friends?}
#'   \item{rude_to_switch_seats_family}{Is it rude to ask someone to switch seats with you in order to be closer to family?}
#'   \item{rude_to_wake_neighbor_bathroom}{Is it rude to wake a passenger up if you are trying to go to the bathroom?}
#'   \item{rude_to_wake_neighbor_walk}{Is it rude to wake a passenger up if you are trying to walk around?}
#'   \item{rude_to_bring_baby}{In general, is it rude to bring a baby on a plane?}
#'   \item{rude_to_bring_unruly_child}{In general, is it rude to knowingly bring unruly children on a plane?}
#'   \item{use_electronics_takeoff}{Have you ever used personal electronics during take off or landing in violation of a flight attendant's direction?	}
#'   \item{smoked_inflight}{Have you ever smoked a cigarette in an airplane bathroom when it was against the rules?}
#'   \item{gender}{Gender}
#'   \item{age}{Age}
#'   \item{household_income}{Household Income}
#'   \item{education}{Education}
#'   \item{region}{Region}
#' }
#'
#' @source \url{https://github.com/fivethirtyeight/data/tree/master/flying-etiquette-survey}
#' @docType data
#' @name fly
#' @usage fly
"fly"

#' Data related to happiness from the general social survey.
#'
#' The data is a small sample of variables related to happiness from the
#' general social survey (GSS). The GSS is a yearly cross-sectional survey of
#' Americans, run since 1972. We combine data for more than 25 years to yield over 60 thousand
#' observations, and of the over 5,000 variables, we select some variables that are related to
#' happiness:
#' @format A data frame with 62466 rows and 11 variables
#' \itemize{
#'  \item year. year of the response, 1972 to 2018.
#'  \item age. age in years: 18--89 (89 stands for all 89 year olds and older).
#'  \item degree. highest education: lt high school, high school, junior
#'     college, bachelor, graduate.
#'  \item finrela. how is your financial status compared to others: far below, below average, average, above average, far above.
#'  \item happy. happiness: very happy, pretty happy, not too happy.
#'  \item health. health: excellent, good, fair, poor.
#'  \item marital. marital status:  married, never married, divorced,
#'    widowed, separated.
#'  \item sex. sex: female, male.
#'  \item polviews. from extremely conservative to extremely liberal.
#'  \item partyid. party identification: strong republican, not str republican, ind near rep, independent, ind near dem, not str democrat, strong democrat, other party.
#'  \item wtssall. probability weight. 0.39--8.74
#' }
#'
#' @keywords datasets
#' @usage data(happy)
"happy"


#' Passengers and crew on board the Titanic
#'
#' A dataset containing some demographics and survival of people on board the Titanic
#' @format A data frame with 2201 rows and 4 variables:
#' \describe{
#'   \item{Class}{factor variable containing the class of a passenger (1st, 2nd, 3rd) or crew.}
#'   \item{Sex}{Male/Female.}
#'   \item{Age}{Child/Adult. This information is not very reliable, because it was inferred from boarding documents that did not state actual age in years.}
#'   \item{Survived}{Yes/No.}
#' }
#' @keywords datasets
#' @name titanic
"titanic"

