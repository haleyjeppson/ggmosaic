#' Flying Etiquette Survey Data
#'
#' Data from the results of a SurveyMonkey
#' survey commissioned by FiveThirtyEight for the story
#' 41 Percent of Fliers Say It’s Rude To Recline Your Airplane Seat.
#'
#'
#' @format A data frame with 1040 rows and 27 variables:
#' \describe{
#'   \item{ID}{Respondent ID}
#'   \item{FlightFreq}{How often do you travel by plane?}
#'   \item{DoYouRecline}{Do you ever recline your seat when you fly?}
#'   \item{Height}{How tall are you?}
#'   \item{Child18}{Do you have any children under 18?}
#'   \item{Seats3_2Arms}{n a row of three seats, who should get to use the two arm rests?}
#'   \item{Seats2_1Arm}{In a row of two seats, who should get to use the middle arm rest?}
#'   \item{WhoControlsWindowShade}{Who should have control over the window shade?}
#'   \item{RudeToMoveToUnsoldSeat}{Is it rude to move to an unsold seat on a plane?}
#'   \item{RudeToTalkToNeighbor}{Generally speaking, is it rude to say more than a few words to the stranger sitting next to you on a plane?}
#'   \item{SixHrFlightRudeToLeaveSeat}{On a six hour flight from NYC to LA, how many times is it acceptable to get up if you're not in an aisle seat?}
#'   \item{RecliningObligationToBehind}{Under normal circumstances, does a person who reclines their seat during a flight have any obligation to the person sitting behind them?}
#'   \item{RudeToRecline}{Is it rude to recline your seat on a plane?}
#'   \item{EliminateReclining}{Given the opportunity, would you eliminate the possibility of reclining seats on planes entirely?}
#'   \item{RudeToSwitchSeatsForFriends}{Is it rude to ask someone to switch seats with you in order to be closer to friends?}
#'   \item{RudeToSwitchSeatsForFamily}{Is it rude to ask someone to switch seats with you in order to be closer to family?}
#'   \item{RudeToWakeNeighborForBathroom}{Is it rude to wake a passenger up if you are trying to go to the bathroom?}
#'   \item{RudeToWakeNeighborForWalk}{Is it rude to wake a passenger up if you are trying to walk around?}
#'   \item{RudeToBringBaby}{In general, is it rude to bring a baby on a plane?}
#'   \item{RudeToBringUnrulyChild}{In general, is it rude to knowingly bring unruly children on a plane?}
#'   \item{UseElectronicsDuringTakeoff}{Have you ever used personal electronics during take off or landing in violation of a flight attendant's direction?	}
#'   \item{HaveYouSmoked}{Have you ever smoked a cigarette in an airplane bathroom when it was against the rules?}
#'   \item{Gender}{Gender}
#'   \item{Age}{Age}
#'   \item{HouseholdIncome}{Household Income}
#'   \item{Education}{Education}
#'   \item{Region}{Region}
#' }
#'
#' @source \url{https://github.com/fivethirtyeight/data/tree/master/flying-etiquette-survey}
#' @docType data
#' @name fly
#' @usage fly
"fly"


#' Data related to happiness from the general social survey.
#'
#' The General Social Survey (GSS) is a yearly cross-sectional survey of
#' Americans, run since 1972. This data set is a small subset of the over 5000 variables collected in the GSS.
#' We combine data since 1972 to yield more than 50 thousand
#' observations, for some variables that are related to
#' happiness:
#'
#' \itemize{
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
#'  \item wtsall. probability weight. 0.39--8.74
#' }
#'
#' @source \url{http://gss.norc.org/Get-The-Data}
#' @name happy
#' @usage data(happy)
#' @format A data frame with 62466 rows and 11 variables
"happy"
