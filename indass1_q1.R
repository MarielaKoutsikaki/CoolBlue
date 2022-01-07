######################## Load the data ##########################

load("C:/Users/New/Desktop/marketing/Ind Ass1/Assignment_1.RData")


#################### Description of the data ####################

head(CLV_data)
head(Conjoint_data)

#to find the levels of all attributes
lapply(Conjoint_data[,4:length(Conjoint_data)-1], levels)


######################### QUESTION 1.a ###########################

#set the baselines
relevel(Conjoint_data$form,ref='Powder')
relevel(Conjoint_data$noapply,ref='200 times')
relevel(Conjoint_data$disinfect,ref='No')
relevel(Conjoint_data$bio,ref = 'No')
relevel(Conjoint_data$price,ref = '35 cents')

mdl <- glm(choice ~ form + noapply + disinfect + bio + price, 
           family = "binomial", Conjoint_data)

results <- summary(mdl)
results




######################### QUESTION 1.b ###########################



#create the partworth list
form<-c(0,0,0)
names(form)<-c('Powder','Concentrate','Premix')

noapply<-c(0,0,0)
names(noapply)<-c('200 times','100 times','50 times')

disinfect<-c(0,0)
names(disinfect)<-c('No','Yes')

bio<-c(0,0)
names(bio)<-c('No','Yes')


price<-c(0,0,0)
names(price)<-c('35 cents','49 cents','79 cents')




partworth <- list('form'=form,'noapply'=noapply,'disinfect'=disinfect,'bio'=bio,'price'=price)
partworth

#create a coeffs list
coeffs <- results$coefficients[,1]*(results$coefficients[,4]<.05)
coeffs


#fill the partworth list
partworth$form[2:3] <- coeffs[2:3]
partworth$noapply[2:3] <- coeffs[4:5]
partworth$disinfect[2] <- coeffs[6]
partworth$bio[2] <- coeffs[7]
partworth$price[2:3] <- coeffs[8:9]
partworth




######################### QUESTION 1.c ###########################













