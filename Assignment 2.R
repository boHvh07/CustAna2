summary(knick_knack_test)
table(knick_knack_test$m_s)

sum(knick_knack_test$m_s)
50286
sum(knick_knack_test$x_s)
316


m <- 161.50
c <- 3343
c/m
brk <- 20.69969/10000
#q1.    0.0021

knick_knack_test$responserate <- knick_knack_test$x_s/knick_knack_test$m_s
sum(knick_knack_test$responserate>=0.0021)
#q2.    51
#####OR
respRFM <-knick_knack_test %>% group_by(knick_knack_test$Segment) %>% summarize(n_resp=sum(x_s, na.rm = TRUE), n_mail=sum(m_s,na.rm = TRUE)) %>% mutate(resp_rate = n_resp/n_mail) %>% print(n=90)
sum(respRFM$resp_rate >= brk)


respRFM_Roll<-knick_knack_roll %>% group_by(knick_knack_roll$Segment) %>% summarize(n_resp=sum(Resp_s, na.rm = TRUE), n_mail=sum(Roll_s,na.rm = TRUE)) %>% mutate(resp_rate = n_resp/n_mail) %>% print(n=90)
mean_resp_roll <- mean(respRFM_Roll$resp_rate, na.rm = TRUE)

absolute_error <- mean(abs(respRFM$resp_rate - respRFM_Roll$resp_rate),na.rm = TRUE)
round(absolute_error, digits = 3) 
#q3.    0.005

library(VGAM)
library(dplyr)
respRFM<-
    knick_knack_test %>% group_by(knick_knack_test$Segment) %>% summarize(n_resp=sum(x_s, na.rm = TRUE), n_mail=sum(m_s,na.rm = TRUE)) %>% mutate(resp_rate = n_resp/n_mail)
respRFM<-respRFM %>% mutate(n_nonresp = n_mail-n_resp) %>% relocate(n_nonresp, .after=n_resp)
fit <- vglm(cbind(respRFM$n_resp,respRFM$n_nonresp) ~ 1, betabinomialff, trace=TRUE)
Coef(fit)
#q4.    0.4389

a<-Coef(fit)[[1]]
b<-Coef(fit)[[2]]

post_mean_resp<-(a+respRFM$n_resp)/(a+b+respRFM$n_mail)

respRFM<-cbind(respRFM, post_mean_resp)

sum(respRFM$post_mean_resp>=brk)
#q5.    66

absolute_mean_2 <- mean(abs(respRFM$post_mean_resp - respRFM_Roll$resp_rate),na.rm = TRUE)
round(absolute_mean_2, digits = 4)
#q6.    0.0037

ROI <- subset(respRFM, post_mean_resp >= brk)
C1 <- c/10000
ROI$C1 <- ROI$n_mail*C1

ROI$segpro <- (m*ROI$post_mean_resp) - C1
sum(ROI$segpro)
#q7.    58