# Timing differences in Discounting






# First situation:
# Yearly discounting with annual 1 QALY per year and an annual discount rate of 3%
# Where we discount all years including the 1st year
values = c(1, 1, 1, 1)            # 1 QALY per year
discount_rate = 0.03              # 3% annual discount rate
t_year <- seq_along(values)       # Time: 1, 2, 3, 4
discounted_values <- values * (1 + discount_rate)^(-t_year)
sum(discounted_values)            # ~3.7171


# Monthly discounting with ractional time, but with an annual discount rate of 3%

monthly_qaly <- 1 / 12            # 1 QALY/year = 1/12 per month
months <- 4 * 12                  # 48 months
t_month <- seq(from = 1, to = months) / 12      # 48 fractional years
values <- rep(monthly_qaly, months)
discounted_values_month <- values * (1 + discount_rate)^(-t_month)
sum(discounted_values_month)    # ~3.76


# Here we notice that we have
paste("disounted annually", round(sum(discounted_values),4), sep =" " )
paste("disounted montly", round(sum(discounted_values_month),4), sep =" " )


# Key reasons for this differences:
# The yearly discounting assues QALYs are recvied at the end fo each year and the lumpsum is discounted
# In the monthly discounting, QALYS rea received gradually every month. and due to this there is less discounting as they are spread earlier
# This leads to a slightly higher present value in the monthly discounting, because the QALYs are received sooner in the year.

# So even with a similar total of 4 undiscounted QALYs, the discounting differs due to timing of the rewards.

paste("difference ", (sum(discounted_values)   - sum(discounted_values_month)  ), sep =" " )


# Apprach
# sum

