jo_wp <- work_plan("2018-08-25",
	on_leave(26),
	working(26, "Mon-Fri 09:00-17:00"),
	working(104, "Mon-Thu 09:00-17:00")
)

gib_wp <- work_plan("2018-08-25",
	working(26, "Mon-Fri 09:00-17:00"),
	on_leave(26),
	working(104, "Tue-Fri 09:00-17:00")
)

wp_baby <- baby_time(jo_wp)
wp <- wp_overlap(jo_wp, gib_wp, wp_baby)
sw <- summarise_weeks(jo_wp, gib_wp)

jo_salary <- salary_calc(34704, 0.02, gib_wp)
gib_salary <- salary_calc(28182, 0.02, jo_wp)

sp <- statutory_parental_pay(sw, 1)
sl <- salary_loss(jo_salary$income, gib_salary$income, sw, 1)
nc <- nursery_cost(wp, 45/8)


library(tidyr)
library(ggplot2)

mdat <- data_frame(
	week=1:nrow(jo_salary),
	income1 = jo_salary$income,
	salary_loss1 = sl$income1,
	income1_total = income1 - salary_loss1,
	income2 = gib_salary$income,
	salary_loss2 = sl$income2,
	income2_total = income2 - salary_loss2,
	statutory_pay = sp,
	nursery = nc$nc_adj,
	net = income1_total + income2_total + statutory_pay - nursery
) %>% gather(what, money, -week)


bt <- group_by(wp, week) %>%
	summarise(parent1 = sum(with_baby1), parent2 = sum(with_baby2), total=parent1 + parent2) %>%
	gather(who, time, -week)

ggplot(subset(mdat, what %in% c("income1_total", "income2_total", "net")), aes(x=week, y=money)) +
geom_point(aes(colour=what)) +
geom_line(aes(colour=what)) +
scale_colour_brewer(type="qual")

ggplot(bt, aes(x=week, y=time)) +
geom_point(aes(colour=who)) +
geom_line(aes(colour=who)) +
scale_colour_brewer(type="qual")

