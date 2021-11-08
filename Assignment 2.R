summary(knick_knack_test)
table(knick_knack_test$m_s)

sum(knick_knack_test$m_s)
50286
sum(knick_knack_test$x_s)
316



3343/161.50
20.69969/10000
#q1.    0.0021

knick_knack_test$responserate <- knick_knack_test$x_s/knick_knack_test$m_s
sum(knick_knack_test$responserate>0.0021)
#q2.    51


knick_knack_roll$resp_r <- knick_knack_roll$Resp_s/knick_knack_roll$Roll_s
RollY <- filter(knick_knack_roll, Roll. == "Y")
mean(RollY$resp_r)
#0.0039

#q3.    ???

library(VGAM)
fit <- vglm(cbind(knick_knack_roll$Roll_s,knick_knack_roll$Resp_s) ~ 1, betabinomialff, trace=TRUE)
Coef(fit)
