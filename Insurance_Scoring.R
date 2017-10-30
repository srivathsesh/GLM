library("dplyr")
predictionfile <- read.csv(file = 'logit_insurance_test.csv')
# remove TARGET_FLAG and TARGET_AMT
predictionfile <- predictionfile %>% select(c(-2,-3))

# impute data
df.imp <- VIM::kNN(predictionfile)

# indicator variables created
factorcols <- sapply(df.imp,is.factor)
dummyvars <- dummy::dummy(df.imp[,factorcols],int = T)

df.imp<- cbind(df.imp,dummyvars) %>% 
  dplyr::mutate(JOB.category = ifelse(JOB %in% c("Student", "Home Maker", "Clerical", "z_Blue_Collar"),0,1))

# Score model

#P_TARGET_FLAG <- predict(train.logistic.rev,newdata = df.imp, type = 'response') - Use this in the source file to check
# p.eqn <- getequation("P_TARGET_FLAG",train.logistic.rev,8) - Use this to get the eta equation correctly...available in the source
#

test_p <- df.imp %>% 
  mutate(L_TARGET_FLAG = -1.25962419 - 3.12e-05 * BLUEBOOK  + 2.16761818 *  URBANICITY_Highly.Urban..Urban  + 0.70216247 *  CAR_USE_Commercial  + 0.34795086 *  KIDSDRIV  + 0.01513301 *  TRAVTIME  + 0.12305204 *  MVR_PTS  - 0.03429193 *  CAR_AGE  - 0.81784286 *  REVOKED_No  - 0.58964657 *  PARENT1_No  - 1.9e-06 *  HOME_VAL  - 0.05017022 *  TIF  + 0.18203741 *  CLM_FREQ  - 0.82496441 *  CAR_TYPE_Minivan  - 0.23550083 *  MSTATUS_Yes  - 0.40715006 *  JOB_.  - 0.97424013 *  JOB_Manager) %>% 
  mutate(P_TARGET_FLAG = (1+exp(-1*L_TARGET_FLAG))^-1) %>% 
  mutate(TARGET_AMT = ifelse(P_TARGET_FLAG < 0.5, 0, 4434.04 - 52.11 * CAR_AGE  + 0.12 *  BLUEBOOK)) %>% 
  mutate(CRASH = ifelse(P_TARGET_FLAG < 0.5, 0, 1)) %>% 
  select(INDEX,P_TARGET_FLAG,TARGET_AMT)

write.csv(test_p,'SESHADRI.logit_insurance_test.csv')
