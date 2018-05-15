library(tidyr)
library(ggplot2)

# calculating for 3 years from baby due date

# Draw up working schedules
parent1_wp <- work_plan("2018-08-25",      # Due date
	on_leave(26),                         # 6 months on leave
	working(26, "Mon-Fri 09:00-17:00"),   # 6 months working full time
	working(104, "Mon-Thu 09:00-17:00")   # 6 months working mon-thu
)

parent2_wp <- work_plan("2018-08-25",      # Due date
	working(26, "Mon-Fri 09:00-17:00"),   # 6 months working full time
	on_leave(26),                         # 6 months on leave
	working(104, "Tue-Fri 09:00-17:00")   # 6 months working tue-fri
)

# Calculate the income
# - annual salary after tax
# - annual salary increase
# - working schedule
parent2_salary <- salary_calc(35000, 0.02, parent2_wp)
parent1_salary <- salary_calc(28182, 0.02, parent1_wp)
sw <- summarise_weeks(parent1_wp, parent2_wp)

# Get the baby's schedule
wp_baby <- baby_time(parent1_wp, waking_time="07:00", sleeping_time="19:00")

# Figure out overlapping times between parent1, parent2 and baby
wp <- wp_overlap(parent1_wp, parent2_wp, wp_baby)

# Pay
# choose option 1 or 2 as per http://www.bristol.ac.uk/hr/policies/shared-parental-leave.html
# Get the expected statutory income
sp <- statutory_parental_pay(sw, 1)

# How much salary is lost by. being on leave
sl <- salary_loss(parent1_salary$income, parent2_salary$income, sw, 1)

# What is the nursery cost
# Put in an hourly rate 
nc <- nursery_cost(wp, 45/8)

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
ggplot(subset(mdat, what %in% c("income1_total", "income2_total", "net")), aes(x=week, y=money)) +
geom_point(aes(colour=what)) +
geom_line(aes(colour=what)) +
scale_colour_brewer(type="qual")

# Amount of available time to spend with baby, weekly
ggplot(bt, aes(x=week, y=time)) +
geom_point(aes(colour=who)) +
geom_line(aes(colour=who)) +
scale_colour_brewer(type="qual")

