# Simulate data and perform CEA

 

# clear workspace

rm(list = ls())

 

 

### Import packages --------

library(data.table)

library(dplyr)

library(readr)

library(tidyverse)

library(lubridate)

library(stringr)

library (lme4)

library (boot)

library(ggplot2)

library(mice)

 

 

### Create data ----------

 

# patient ID

id<-as.data.frame(1:100)

                 

# create the cost variable

cost<- as.data.frame(rnorm(100, 1500, 500))

 

# create the cost variable

QALYs<- as.data.frame(rnorm(100, 0.5, 0.3))

 

# treatment code

trtcode1 <- matrix(1, nrow=50, ncol =1)

trtcode0 <- matrix(0, nrow=50, ncol =1)

trtcode <- rbind(trtcode0, trtcode1)

 

# merge all data sets

data <- as.data.frame(cbind(id, cost, QALYs, trtcode))

 

colnames(data)<-c("id", "cost", "QALYs", "trtcode")

 

rm(trtcode0, trtcode1, cost, QALYs, trtcode, id)

 

 

### Regression analysis ---------

 

# effects

model_effect <- lm (QALYs ~ trtcode, data = data)

summary(model_effect)

confint(model_effect) # get 95% CI

 

 

# costs

model_costs <- lm (cost ~ trtcode , data = data)

summary(model_costs)

confint(model_costs)  # get 95% CI

 

# store results from analysis

# to calculate ICER

coef_effect <- as.data.frame(summary(model_effect )$coefficients[2,1])

coef_cost <- as.data.frame(summary(model_costs)$coefficients[2,1])

 

 

# merge

coef_icer <- cbind(coef_effect, coef_cost)

# add column names

colnames(coef_icer)<-c("coef_effect", "coef_cost")

 

# remove data sets not needed

rm(coef_cost, coef_effect)

 

### Bootstrapping ---------

 

# non paramteric bootstrap 

 

B<-1000 # Number of resampling iterations

results<-as.data.frame(matrix(NA, nrow=B, ncol = 3)) #  matrix with 2 columns and 'B' rows

colnames(results)<-c("cost_diff", "effect_dif", "boot_id")

 

# Bootstrapping

for(i in 1:B){

  df_boot <- data[data$id %in% sample(data$id, replace = TRUE),]

  model_costs <- lm (cost ~ trtcode, data = df_boot)

  model_effect <- lm (QALYs ~ trtcode, data = df_boot)

  results[i,1]<-model_costs$coefficients[2]

  results[i,2]<-model_effect$coefficients[2]

  results[i,3] <- i

}

 

 

### cost-effectiveness plane ----------------

ggplot () +

  geom_point(data=results, aes(x=results$effect_dif, y=results$cost_dif, color = I("blue3")))+

  geom_point(data=coef_icer, aes(x=coef_effect, y=coef_cost, color = I("red3"))) +

  labs (x= "Difference in Effects", y= "Difference in Costs") +

  geom_hline(yintercept=0, linetype = "dashed") +

  geom_vline(xintercept=0, linetype = "dashed") +

  ylim(min(results$cost_dif * 1.2),

       max(results$cost_dif * 1.2)) + # scale range is proportional to range of values

  xlim(min(results$effect_dif * 1.2),

       max(results$effect_dif * 1.2)) + # scale range is proportional to range of values

  theme_minimal()

 

 

# Get proportion in each of the

# four quadrants of teh CE plane

results <- results %>%

  mutate(quadr= ifelse(results$cost_dif > 0 & results$effect_dif > 0, "NE_quad",

                       ifelse(results$cost_dif < 0 & results$effect_dif > 0, "SE_quad",

                              ifelse(results$cost_dif < 0 & results$effect_dif <0, "SW_quad",

                                     "NW_quad"))))

 

(table(results$quadr)/(B)*100)  # B number of bootstrapped data sets

 

# add assertion to

# ensure no mistakes in code

z <- sum((table(results$quadr)/(B)*100) )

if (z!=100) # unique patid's must be the same as initially imported dataset

  stop("You made an error in your code. Cost and effect pairs are wrong")

rm(z)

 

 

### Cost-effectiveness acceptability curves --------------

 

wtp_seq <- seq(0, 50000, by= 5000)    # define wtp range

lw=length(wtp_seq)                    # get length of wtp values

 

# create a wtp vector

# for different wtp thresholds

for (i in wtp_seq){

  results[[paste('wtp_',i ,sep="")]] <- ifelse((i*results$effect_dif)-

                                                 results$cost_dif >0,1,0) # NMB:yes/no

}

 

# View(results)

# uncomment to see results

 

# create a data frame to store

# results in order to make ceac

ceac_res<-as.data.frame(matrix(NA, nrow=lw, ncol = 2)) # 2 columns: wtp length and p.ce

colnames(ceac_res)<-c("wtp_thr", "p.ce")

ceac_res$wtp_thr <- wtp_seq

 

for (i in 5:ncol(results)){   # from 5th column till end; amend as needed

  a <- table(results[,i])       # extract n of NMB

  a <- cbind(a,prop.table(a))   # calculate proportion of NMB:yes

  a <- a[nrow(a),ncol(a)]       # store proportion for each wtp, last row & column

  ceac_res$p.ce[i-4]<-a         # bind with ceac results

}

# if the probability of ce is 0, the results may be wrong!

# then force p.ce <- 0 !!!

 

 

# uncomemnt to test

# b <- table(results$wtp_45000)

# b <- cbind(b,prop.table(b))

# b <- b[2,2]   

 

# plot ceac

ggplot () +

  geom_line(data=ceac_res, aes(x=ceac_res$wtp_thr, y=ceac_res$p.ce, color = I("blue3")))+

  labs (x= "WTP threshold", y= "Probability of cost-effectiveness") +

  ylim(c(0, 1)) +

  xlim(min(ceac_res$wtp_thr), max(ceac_res$wtp_thr)) +  # uses range of wtp

  theme_minimal()

 

# remove data sets we do not need

rm(a, i)
