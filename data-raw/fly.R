library(tidyverse)
library(forcats)
library(curl)

# read in data
fly_ett <- read_csv( curl("https://raw.githubusercontent.com/fivethirtyeight/data/master/flying-etiquette-survey/flying-etiquette.csv"))
fly <- fly_ett # for my poor internet connection

# start to process data -----------------------------------

names(fly) <- c("id", "flight_freq", "do_you_recline", "height", "has_child_under_18",
                "three_seats_two_arms", "two_seats_one_arm", "window_shade",
                "rude_to_move_to_unsold_seat", "rude_to_talk_to_neighbor",
                "six_hr_flight_leave_seat", "reclining_obligation_to_behind",
                "rude_to_recline", "eliminate_reclining", "rude_to_switch_seats_friends",
                "rude_to_switch_seats_family", "rude_to_wake_neighbor_bathroom",
                "rude_to_wake_neighbor_walk", "rude_to_bring_baby",
                "rude_to_bring_unruly_child", "use_electronics_takeoff",
                "smoked_inflight", "gender", "age", "household_income", "education", "region")

fly <- fly %>%
  mutate_if(is.character, as.factor)

# flight_freq -------------------------

fly %>% group_by(flight_freq) %>% tally()

fly <- fly %>%
  mutate(flight_freq = fct_collapse(flight_freq, `more than once a month` = c("Every day", "A few times per week", "A few times per month"),
                                    `once a month or less` = c("Once a month or less"),
                                    `once a year or less` = c("Once a year or less"),
                                 `never` = c("Never"))) %>%
  mutate(flight_freq = fct_relevel(flight_freq, rev(c("more than once a month", "once a month or less", "once a year or less", "never"))))

# do_you_recline -------------------------

fly %>% group_by(do_you_recline) %>% tally()

fly <- fly %>%
  mutate(do_you_recline = tolower(do_you_recline)) %>%
  #mutate(do_you_recline = fct_collapse(do_you_recline, usually = c("Always", "Usually"), sometimes = c("About half the time", "Once in a while"), never = c("Never"))) %>%
  mutate(do_you_recline = fct_relevel(do_you_recline, rev(c("always", "usually", "about half the time", "once in a while", "never"))))

# fly$do_you_recline3 <- fct_collapse(fly$do_you_recline,
#                                     Yes = c("Always", "Usually", "About half the time", "Once in a while"),
#                                     No = c("Never")
# )


# height -------------------------

fly %>% group_by(height) %>% tally() %>% arrange(n)

fly <- fly %>%
  mutate(height = tolower(height)) %>%
  mutate(height = fct_relevel(height, c("under 5 ft.", "5'0\"", "5'1\"", "5'2\"", "5'3\"", "5'4\"", "5'5\"",
                                            "5'6\"", "5'7\"", "5'8\"", "5'9\"", "5'10\"", "5'11\"",
                                            "6'0\"", "6'1\"", "6'2\"", "6'3\"", "6'4\"", "6'5\"", "6'6\" and above")))

# fly$height <- fct_collapse(fly$height,
#                             `Under 5'` = "Under 5 ft.",
#                             `5' - 5'5\"` = c("5'0\"", "5'1\"", "5'2\"","5'3\"","5'4\"", "5'5\""),
#                             `5'6\" - 5'11\"` = c("5'6\"", "5'7\"", "5'8\"","5'9\"","5'10\"", "5'11\""),
#                             `More than 6'0\"` = c("6'0\"", "6'1\"", "6'2\"","6'3\"","6'4\"", "6'5\"", "6'6\" and above")
# )

# fly$height2 <- forcats::fct_relevel(fly$height2, "Less than 5'5\"", "5'6\" - 5'11\"")

# has_child_under_18 -------------------------

fly %>% group_by(has_child_under_18) %>% tally()

fly <- fly %>%
  mutate(has_child_under_18 = tolower(has_child_under_18)) %>%
  mutate(has_child_under_18 = fct_relevel(has_child_under_18, c("no")))

levels(fly$has_child_under_18)

# three_seats_two_arms -------------------------

fly %>% group_by(three_seats_two_arms) %>% tally()

fly <- fly %>%
  mutate(three_seats_two_arms =fct_recode(three_seats_two_arms, shared = "The arm rests should be shared", middle_seat = "The person in the middle seat gets both arm rests",
                                       window_and_aisle = "The people in the aisle and window seats get both arm rests", first_to_use_it = "Whoever puts their arm on the arm rest first",
                                       other = "Other (please specify)" )) %>%
  mutate(three_seats_two_arms =fct_relevel(three_seats_two_arms, c("shared", "middle_seat", "window_and_aisle", "first_to_use_it", "other")))

levels(fly$three_seats_two_arms)

# two_seats_one_arm -------------------------

fly %>% group_by(two_seats_one_arm) %>% tally()

fly <- fly %>%
  mutate(two_seats_one_arm =fct_recode(two_seats_one_arm, shared = "The arm rests should be shared", window_seat = "The person by the window",
                                      aisle_seat = "The person in aisle", first_to_use_it = "Whoever puts their arm on the arm rest first",
                                      other = "Other (please specify)" )) %>%
  mutate(two_seats_one_arm =fct_relevel(two_seats_one_arm, c("shared", "window_seat", "aisle_seat", "first_to_use_it", "other")))

levels(fly$two_seats_one_arm)

# window_shade -------------------------

fly %>% group_by(window_shade) %>% tally()

fly <- fly %>%
  mutate(window_shade =fct_recode(window_shade, everyone = "Everyone in the row should have some say", window = "The person in the window seat should have exclusive control")) %>%
  mutate(window_shade =fct_relevel(window_shade, c("everyone")))

levels(fly$window_shade)

# rude_to_move_to_unsold_seat -------------------------

fly %>% group_by(rude_to_move_to_unsold_seat) %>% tally()

fly <- fly %>%
  mutate(rude_to_move_to_unsold_seat =fct_recode(rude_to_move_to_unsold_seat, no = "No, not rude at all", somewhat = "Yes, somewhat rude", yes = "Yes, very rude")) %>%
  mutate(rude_to_move_to_unsold_seat =fct_relevel(rude_to_move_to_unsold_seat, c("no", "somewhat")))

levels(fly$rude_to_move_to_unsold_seat)

# rude_to_talk_to_neighbor -------------------------

fly %>% group_by(rude_to_talk_to_neighbor) %>% tally()

fly <- fly %>%
  mutate(rude_to_talk_to_neighbor =fct_recode(rude_to_talk_to_neighbor, no = "No, not at all rude", somewhat = "Yes, somewhat rude", yes = "Yes, very rude")) %>%
  mutate(rude_to_talk_to_neighbor =fct_relevel(rude_to_talk_to_neighbor, c("no", "somewhat")))

levels(fly$rude_to_talk_to_neighbor)

# six_hr_flight_leave_seat -------------------------

fly %>% group_by(six_hr_flight_leave_seat) %>% tally()

fly <- fly %>%
    mutate(six_hr_flight_leave_seat =fct_recode(six_hr_flight_leave_seat, not_okay = "It is not okay to get up during flight", four_or_more = "More than five times times",
                                                once = "Once", twice = "Twice", three_times = "Three times", four_or_more = "Four times")) %>%
    mutate(six_hr_flight_leave_seat =fct_relevel(six_hr_flight_leave_seat, c("not_okay", "once", "twice", "three_times", "four_or_more")))

levels(fly$six_hr_flight_leave_seat)

# reclining_obligation_to_behind -------------------------
fly %>% group_by(reclining_obligation_to_behind) %>% tally()

fly <- fly %>%
  mutate(reclining_obligation_to_behind = fct_recode(reclining_obligation_to_behind, no = "No, the person on the flight has no obligation to the person behind them",
                                                     yes = "Yes, they should not recline their chair if the person behind them asks them not to")) %>%
  mutate(reclining_obligation_to_behind = fct_relevel(reclining_obligation_to_behind, c("no")))

levels(fly$reclining_obligation_to_behind)

# rude_to_recline -------------------------
fly %>% group_by(rude_to_recline) %>% tally()

fly <- fly %>%
  mutate(rude_to_recline = fct_recode(rude_to_recline, no = "No, not rude at all", somewhat = "Yes, somewhat rude", yes = "Yes, very rude")) %>%
  mutate(rude_to_recline = fct_relevel(rude_to_recline, c("no", "somewhat")))

levels(fly$rude_to_recline)

# eliminate_reclining -------------------------
fly %>% group_by(eliminate_reclining) %>% tally()

fly <- fly %>%
  mutate(eliminate_reclining = fct_recode(eliminate_reclining, no = "No", yes = "Yes")) %>%
  mutate(eliminate_reclining = fct_relevel(eliminate_reclining, "no"))

levels(fly$eliminate_reclining)

# rude_to_switch_seats_friends -------------------------
fly %>% group_by(rude_to_switch_seats_friends) %>% tally()

fly <- fly %>%
  mutate(rude_to_switch_seats_friends = fct_recode(rude_to_switch_seats_friends, no = "No, not at all rude", somewhat = "Yes, somewhat rude", yes = "Yes, very rude")) %>%
  mutate(rude_to_switch_seats_friends = fct_relevel(rude_to_switch_seats_friends, c("no", "somewhat")))

levels(fly$rude_to_switch_seats_friends)

# rude_to_switch_seats_family -------------------------
fly %>% group_by(rude_to_switch_seats_family) %>% tally()

fly <- fly %>%
  mutate(rude_to_switch_seats_family = fct_recode(rude_to_switch_seats_family, no = "No, not at all rude", somewhat = "Yes, somewhat rude", yes = "Yes, very rude")) %>%
  mutate(rude_to_switch_seats_family = fct_relevel(rude_to_switch_seats_family, c("no", "somewhat")))

levels(fly$rude_to_switch_seats_family)

# rude_to_wake_neighbor_bathroom -------------------------
fly %>% group_by(rude_to_wake_neighbor_bathroom) %>% tally()

fly <- fly %>%
  mutate(rude_to_wake_neighbor_bathroom = fct_recode(rude_to_wake_neighbor_bathroom, no = "No, not at all rude", somewhat = "Yes, somewhat rude", yes = "Yes, very rude")) %>%
  mutate(rude_to_wake_neighbor_bathroom = fct_relevel(rude_to_wake_neighbor_bathroom, c("no", "somewhat")))

# rude_to_wake_neighbor_walk -------------------------
fly %>% group_by(rude_to_wake_neighbor_walk) %>% tally()

fly <- fly %>%
  mutate(rude_to_wake_neighbor_walk = fct_recode(rude_to_wake_neighbor_walk, no = "No, not at all rude", somewhat = "Yes, somewhat rude", yes = "Yes, very rude")) %>%
  mutate(rude_to_wake_neighbor_walk = fct_relevel(rude_to_wake_neighbor_walk, c("no", "somewhat")))

# rude_to_bring_baby -------------------------
fly %>% group_by(rude_to_bring_baby) %>% tally()

fly <- fly %>%
  mutate(rude_to_bring_baby = fct_recode(rude_to_bring_baby, no = "No, not at all rude", somewhat = "Yes, somewhat rude", yes = "Yes, very rude")) %>%
  mutate(rude_to_bring_baby = fct_relevel(rude_to_bring_baby, c("no", "somewhat")))

# rude_to_bring_unruly_child -------------------------
fly %>% group_by(rude_to_bring_unruly_child) %>% tally()

fly <- fly %>%
  mutate(rude_to_bring_unruly_child = fct_recode(rude_to_bring_unruly_child, no = "No, not at all rude", somewhat = "Yes, somewhat rude", yes = "Yes, very rude")) %>%
  mutate(rude_to_bring_unruly_child = fct_relevel(rude_to_bring_unruly_child, c("no", "somewhat")))

# use_electronics_takeoff -------------------------
fly %>% group_by(use_electronics_takeoff) %>% tally()

fly <- fly %>%
  mutate(use_electronics_takeoff = fct_recode(use_electronics_takeoff, no = "No", yes = "Yes")) %>%
  mutate(use_electronics_takeoff = fct_relevel(use_electronics_takeoff, c("no")))

# smoked_inflight -------------------------
fly %>% group_by(smoked_inflight) %>% tally()

fly <- fly %>%
  mutate(smoked_inflight = fct_recode(smoked_inflight, no = "No", yes = "Yes")) %>%
  mutate(smoked_inflight = fct_relevel(smoked_inflight, c("no")))

# gender -------------------------
fly %>% group_by(gender) %>% tally()

fly <- fly %>%
  mutate(gender = fct_recode(gender, female = "Female", male = "Male"))

# age -------------------------
fly %>% group_by(age) %>% tally()

fly <- fly %>%
  mutate(age = fct_recode(age, `60+` = "> 60")) %>%
  mutate(age = fct_relevel(age, c("18-29", "30-44", "45-60", "60+")))

# household_income -------------------------
fly %>% group_by(household_income) %>% tally()

fly <- fly %>%
  mutate(household_income = fct_recode(household_income, `$150,000+` = "150000")) %>%
  mutate(household_income = fct_relevel(household_income, c("$0 - $24,999", "$25,000 - $49,999", "$50,000 - $99,999", "$100,000 - $149,999")))

levels(fly$household_income)

# education -------------------------
fly %>% group_by(education) %>% tally()

fly <- fly %>%
  mutate(education = tolower(education)) %>%
  mutate(education = fct_relevel(education, c("less than high school degree", "high school degree", "some college or associate degree")))

levels(fly$education)

# region -------------------------
fly %>% group_by(region) %>% tally()

fly <- fly %>%
  mutate(region = tolower(region)) %>%
  mutate(region = fct_collapse(region, midwest = c("east north central", "west north central"), northeast = c("new england", "middle atlantic"),
                             south = c("west south central", "east south central", "south atlantic"))) %>%
  mutate(region = fct_relevel(region, c("pacific", "mountain", "midwest", "northeast", "south")))

levels(fly$region)

# -------------------------------------------------------------------------
# save data
#save(fly, file = "data/fly.rda")



















