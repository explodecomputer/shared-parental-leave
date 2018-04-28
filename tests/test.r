# account for taxes and other contributions? Or ask for net salary?
# my net salary: 28128
# gib's net salary: 34704 (excluding Wellcome top up)

# have to sort out pension contributions - can't pay from salary exchange scheme after end of SMP period


jo_salary <- salary_calc(28182, 0.02, 52*3)
jo_spp1 <- statutory_parental_pay(140.98, 0, 26, 1)
jo_reduction1 <- salary_loss(jo_salary, 0, 26, 52, 1)
jo_spp2 <- statutory_parental_pay(140.98, 0, 26, 2)
jo_reduction2 <- salary_loss(jo_salary, 0, 26, 52, 2)

total1 <- jo_salary + jo_reduction1 + jo_spp1
total2 <- jo_salary + jo_reduction2 + jo_spp2
sum(total1)
sum(total2)

# option 1 is better

gib_salary <- salary_calc(34704, 0.02, 52*3)
gib_spp <- statutory_parental_pay(140.98, 26, 26, 1)
gib_reduction <- salary_loss(gib_salary, 26, 26, 52, 1)

totalg <- gib_salary + gib_spp + gib_reduction
sum(totalg)





### baby time


bwt <- baby_awake_time("07:00", "19:00", "2018-08-25", 52)
wt <- work_time("08:45", "17:15", "2018-08-25", c("Tue", "Wed", "Thu", "Fri"), 52)
time_with_baby(bwt, wt)



work_schedule("2018-08-25",
	working("09:00", "17:00", c("Mon", "Tue", "Wed", "Thu", "Fri"), 26),
	on_leave(26),
	working("09:00", "17:00", c("Tue", "Wed", "Thu", "Fri"), 104)
)




wt <-  c(
	work_time("09:00", "17:00", "2018-08-25", c("Mon", "Tue", "Wed", "Thu", "Fri"), 26),
	rep(NA, 26),

)




### Nursery time

jo <- c(
	rep(0, 26),
	rep(5, 26),
	rep(4, 104)
)
gib <- c(
	rep(5, 26),
	rep(0, 26),
	rep(4, 104)
)

nursery_time(jo, gib)


wrapper <- function(p1_time, p2_time, salary1_base, salary2_base, option)

