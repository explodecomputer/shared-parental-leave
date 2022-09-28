#' Calculating time and money based on parental leave and nursery
#'
#' @param parent1_wp Work plan for parent 1, output from \code{work_plan}
#' @param parent2_wp Work plan for parent 2, output from \code{work_plan}
#' @param baby_waking_time Expected time that baby wakes up in the morning e.g. "07:00"
#' @param baby_sleeping_time Expected time that baby sleeps at night e.g. "19:00"
#' @param parental_leave_option Choose option 1 or 2 as per http://www.bristol.ac.uk/hr/policies/shared-parental-leave.html
#' @param statutory_weekly_pay Expected weekly pay (as of April 2022 it was 156.66)
#' @param hourly_nursery_rate Hourly nursery cost
#' @param parent1_annual_salary Gross salary of parent 1 from day 1
#' @param parent2_annual_salary Gross salary of parent 2 from day 1
#' @param annual_salary_increment Expected annual increment, default=0.02
#'
#' @return List of results
#' \itemize{
#'    \item parent1_wp = Parent 1 working times
#'    \item parent2_wp = Parent 2 working times
#'    \item baby_schedule = Baby's schedule
#'    \item overlapping_times = Overlapping times between parents and baby
#'    \item parent1_salary = Parent 1 salary over time
#'    \item parent2_salary = Parent 2 salary over time
#'    \item statutory_parental_pay = Statutory parental pay over time
#'    \item salary_loss = Salary reductions due to leave
#'    \item nursery_cost = Nursery costs
#'    \item weekly_monies = Summary of weekly incomes and outgoings
#'    \item babytime = Summary of time spent with baby
#'    \item earnings_plot = Plot of weekly net earnings
#'    \item babytime_plot = Plot of weekly baby time
#' }
#' @export
babytime <- function(parent1_wp, parent2_wp, baby_waking_time, baby_sleeping_time, parental_leave_option, statutory_weekly_pay = 156.66, hourly_nursery_rate, parent1_annual_salary, parent2_annual_salary, annual_salary_increment=0.02)
{
  parent1_salary <- salary_calc(base_salary = parent1_annual_salary, annual_increment = annual_salary_increment, wp = parent1_wp)
  parent2_salary <- salary_calc(base_salary = parent2_annual_salary, annual_increment = annual_salary_increment, wp = parent2_wp)
  sw <- summarise_weeks(parent1_wp, parent2_wp)

  # Get the baby's schedule
  wp_baby <- baby_time(wp = parent1_wp, waking_time = baby_waking_time, sleeping_time = baby_sleeping_time)

  # Figure out overlapping times between parent1, parent2 and baby
  wp <- wp_overlap(wp_parent1 = parent1_wp, wp_parent2 = parent2_wp, wp_baby = wp_baby)
  
  # Pay
  # choose option 1 or 2 as per http://www.bristol.ac.uk/hr/policies/shared-parental-leave.html
  # Get the expected statutory income
  sp <- statutory_parental_pay(nt = sw, option = parental_leave_option, rate = statutory_weekly_pay)
  
  # How much salary is lost by being on leave
  sl <- salary_loss(salary1 = parent1_salary$income, salary2 = parent2_salary$income, nt = sw, option = parental_leave_option)
  
  # What is the nursery cost
  # Put in an hourly rate 
  nc <- nursery_cost(wp = wp, hourly_rate = hourly_nursery_rate)
  
  mdat <- data_frame(
  	week=1:nrow(parent1_salary),
  	income1 = parent1_salary$income,
  	salary_loss1 = sl$income1,
  	income1_total = income1 - salary_loss1,
  	income2 = parent2_salary$income,
  	salary_loss2 = sl$income2,
  	income2_total = income2 - salary_loss2,
  	statutory_pay = sp,
  	nursery = nc$nc_adj,
  	net = income1_total + income2_total + statutory_pay - nursery
  ) %>% gather(what, money, -week)
  
  bt <- group_by(wp, week) %>%
  	summarise(parent1 = sum(with_baby1), parent2 = sum(with_baby2), total=parent1 + parent2) %>%
  	gather(who, time, -week)
  
  # Weekly earnings over time
  p1 <- ggplot(subset(mdat, what %in% c("income1_total", "income2_total", "net")), aes(x=week, y=money)) +
  geom_point(aes(colour=what)) +
  geom_line(aes(colour=what)) +
  scale_colour_brewer(type="qual")
  
  # Amount of available time to spend with baby, weekly
  p2 <- ggplot(bt, aes(x=week, y=time)) +
  geom_point(aes(colour=who)) +
  geom_line(aes(colour=who)) +
  scale_colour_brewer(type="qual") +
  expand_limits(y = 0)
  
  ret <- list(
    parent1_wp = parent1_wp,
    parent2_wp = parent2_wp,
    baby_schedule = wp_baby,
    overlapping_times = wp,
    parent1_salary = parent1_salary,
    parent2_salary = parent2_salary,
    statutory_parental_pay = sp,
    salary_loss = sl,
    nursery_cost = nc,
    weekly_monies = mdat,
    babytime = bt,
    earnings_plot = p1,
    babytime_plot = p2
  )
  return(ret)
}
