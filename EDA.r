#EDA Case Study

#reading data
loan.data <- read.csv("loan.csv", stringsAsFactors = FALSE, header = TRUE)
str(loan.data)

library(dplyr)
library(stringr)
library(ggplot)
library(tidyr)
library(tidyverse)

#this will show that the total number of rows are equal to NA values for some columns, which means they are emepty.


#------------------------------------ Understanding Data -------------------------------------------#
# The given data set has information related to customers applying for loan in a lending company.

#------------------------------------ Objective -------------------------------------------#
# The objective is to derive those factors which might be responsible for an individual defaulting a loan.
#The aim is to identify patterns which indicate if a person is likely to default, which may be used for taking actions 
#such as denying the loan, reducing the amount of loan, lending (to risky applicants) at a higher interest rate, etc.


#---------------------------------- checking for NA values column wise -----------------------------------------------#
sapply(loan.data, function(x) sum(is.na(x)))
map(loan.data, ~sum(is.na(.)))

#---------------------------------- checking NA row wise -----------------------------------------------#
apply(loan.data, MARGIN = 1, function(x) sum(is.na(x)))


# -------------Going through the dataset we see that there are a lot of columns having no data
#(total number of rows are equal to NA Values for some columns) & 0 or NA values-------------#
#-------------------so these will not be required.------------------#
#total_il_high_credit_limit
#total_bc_limit
#total_bal_ex_mort
#tot_hi_cred_lim
#percent_bc_gt_75
#pct_tl_nvr_dlq
#num_tl_op_past_12m
#num_tl_90g_dpd_24m
#num_tl_30dpd
#num_tl_120dpd_2m
#num_sats
#num_rev_tl_bal_gt_0
#num_rev_accts
#num_op_rev_tl
#num_il_tl
#num_bc_tl
#num_bc_sats
#num_actv_rev_tl
#num_actv_bc_tl
#num_accts_ever_120_pd
#mths_since_recent_revol_delinq
#mths_since_recent_inq
#mths_since_recent_bc_dlq
#mths_since_recent_bc
#mort_acc
#mo_sin_rcnt_tl
#mo_sin_rcnt_rev_tl_op
#mo_sin_old_rev_tl_op
#mo_sin_old_il_acct
#bc_util
#bc_open_to_buy
#avg_cur_bal
#acc_open_past_24mths
#inq_last_12m
#total_cu_tl
#inq_fi
#total_rev_hi_lim
#all_util
#max_bal_bc
#open_rv_24m
#open_rv_12m
#il_util
#total_bal_il
#mths_since_rcnt_il
#open_il_24m
#open_il_12m
#open_il_6m
#open_acc_6m
#tot_cur_bal
#tot_coll_amt
#verification_status_joint
#dti_joint
#annual_inc_joint
#mths_since_last_major_derog
#collections_12_mths_ex_med
#chargeoff_within_12_mths
#tax_liens


loan.data <- loan.data[,!(colnames("total_il_high_credit_limit","total_bc_limit","total_bal_ex_mort","tot_hi_cred_lim","percent_bc_gt_75","pct_tl_nvr_dlq","num_tl_op_past_12m","num_tl_90g_dpd_24m","num_tl_30dpd","num_tl_120dpd_2m","num_sats","num_rev_tl_bal_gt_0","num_rev_accts","num_op_rev_tl","num_il_tl","num_bc_tl","num_bc_sats","num_actv_rev_tl","num_actv_bc_tl","num_accts_ever_120_pd","mths_since_recent_revol_delinq","mths_since_recent_inq","mths_since_recent_bc_dlq","mths_since_recent_bc","mort_acc","mo_sin_rcnt_tl","mo_sin_rcnt_rev_tl_op","mo_sin_old_rev_tl_op","mo_sin_old_il_acct","bc_util","bc_open_to_buy","avg_cur_bal","acc_open_past_24mths","inq_last_12m","total_cu_tl","inq_fi","total_rev_hi_lim","all_util","max_bal_bc","open_rv_24m","open_rv_12m","il_util","total_bal_il","mths_since_rcnt_il","open_il_24m","open_il_12m","open_il_6m","open_acc_6m","tot_cur_bal","tot_coll_amt","verification_status_joint","dti_joint","annual_inc_joint","mths_since_last_major_derog","collections_12_mths_ex_med","chargeoff_within_12_mths","tax_liens"))]

loan.data[!sapply(loan.data, function(x) all(is.na(x)))]
loan.data[!sapply(loan.data, function(x) all(x==0))]
loan.data[!sapply(loan.data, function(x) all(x==0 | NA))]

# ---------------- For some columns (below given), they contain single value for all observations hence they can be ignored -----------------------------------#
#delinq_amnt -- contains 0
#acc_now_delinq -- contains 0
#application_type -- contains "individual"
#policy_code -- contains 1
#initial_list_status -- contains "f"
#pymnt_plan -- contains "n"

loan.data <- loan.data[,!(colnames("delinq_amnt","acc_now_delinq","application_type","policy_code","initial_list_status","pymnt_plan"))]

# Also there are many columns which will not be required for analysis.
#url -- not of any use without user name and password - Here loan_id value is same as id
#desc -- values inside this looks like comments which might not be of any use during analysis
#zip_code -- complete zip code is not given so there will be no use 
#addr_state - location will be of no use to determine the defaulters
#title -- values for title seem to be random and redundant for values in purpose so removing

loan.data <- loan.data[,!(colnames("url","desc","zip_code","addr_state","title"))]



#check for duplicate values


#replacing values represting NA with NA
loan.data$emp_length <- str_replace_all(loan.data$emp_length, "n/a", "NA")



#removing % sign from int_rate and revol_util columns and converting into numeric 
as.numeric(gsub("\\%", "", loan.data$int_rate))
as.numeric(gsub("\\%", "", loan.data$revol_util))

#removing >, <, + sign from emp_length column
gsub("\\+", "", loan.data$emp_length)
gsub("\\<", "", loan.data$emp_length)
gsub("\\>", "", loan.data$emp_length)

sum(is.na(loan.data$emp_length))
sum(loan.data$emp_length=="")

#considering mean value for the missing data in emp_length
loan.data$emp_length[loan.data$emp_length==""] <- median(loan.data$emp_length,na.rm = T)

#removing "months" from term column
as.numeric(gsub("months", "", loan.data$term))


#converting date variable and amount variable
as.numeric(loan.data$funded_amnt_inv)
as.numeric(loan.data$annual_inc)
as.POSIXct(loan.data$issue_d, format = "%d-%m-%Y")
as.POSIXct(loan.data$earliest_cr_line, format = "%d-%m-%Y")
as.POSIXct(loan.data$last_pymnt_d, format = "%d-%m-%Y")
as.POSIXct(loan.data$last_credit_pull_d, format = "%d-%m-%Y")
loan$issue_d <- paste("01-",loan$issue_d,sep="")
loan$issue_d <- as.Date(loan$issue_d,"%d-%B-%y")

#rounding off multiple columns

loan.data %>% mutate_each(funs(round(.,2)),loan_amnt, funded_amnt, funded_amnt_inv, annual_inc, revol_bal,out_prncp, out_prncp_inv,total_pymnt,total_pymnt_inv, total_rec_prncp, total_rec_int,total_rec_late_fee, recoveries, collection_recovery_fee,last_pymnt_amnt)

#Finding out frequencies for factors in loan_status.

loan.data %>%
group_by(loan_status) %>%
#summarize(pct.loan_status)
#mutate(Percent = 100*loan_status/sum(loan_status))


#There are three given loan status out of which we need to consider only Fully Paid and Charged Off 
#as for the Current status we are not sure if they will get converted to fully paid or charged off

loan.data<-loan.data[-which(toupper(loan.data$loan_status) == "CURRENT"), ]









#plottting
#---------------------------------------------------------------- Univariate ---------------------------------------------------------#

#loan_status -> This shows the current status of the loan.
#Fully Paid = repayment of loan has been done
#Charged Off = individual has defaulted the loan.


#--------------------------------------------------------------- Loan Status ---------------------------------------------------------#

plot_loan_Status<- ggplot(loan.data, aes(reorder(loan_status, -table(loan_status)[loan_status]))) +
					geom_bar(stat="count") + scale_y_continuous(trans='log2') + 
					geom_text(stat='count',aes(label=..count..),vjust=-0.5)+ 
					xlab("Loan Status")+ylab("Frequency of Applicants") +
					ggtitle("Loan Status -Frequency of Applicants")

#--------------------------------------------------------------- Loan Status ---------------------------------------------------------#					
function(dataset,var,var_name)
(loan,loan$loan_status,"Default Distribution")

					
ggplot(aes(x = as.factor(loan_status))) +
geom_bar(aes(y = (..count..)/sum(..count..))) +
geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
scale_y_continuous(labels = percent) +
labs(title = var_name, y = "Percent", x = "Default Distribution")+
theme(axis.text.y=element_blank(), axis.ticks=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 60, hjust = 1)
					
			
#this will give us the percentage of people who have defaulted the loan

#--------------------------------------------------------------- Purpose ---------------------------------------------------------#

(loan,loan$purpose,"Types of Products offers by Lending club")

ggplot(aes(x = as.factor(purpose))) +
geom_bar(aes(y = (..count..)/sum(..count..))) +
geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
scale_y_continuous(labels = percent) +
labs(title = var_name, y = "Percent", x = "Types of Products offers by Lending club")+
theme(axis.text.y=element_blank(), axis.ticks=element_blank(),axis.title.y=element_blank(),axis.text.x = element_text(angle = 60, hjust = 1)

#--------------------------------------------------------------- Home Ownership ---------------------------------------------------------#
ggplot(loan.data, aes(reorder(home_ownership, -table(home_ownership)[home_ownership]))) + 
geom_bar(stat="count") + 
scale_y_continuous(trans='log2') + 
geom_text(stat='count',aes(label=..count..),vjust=-0.5)+ 
xlab("Home Ownership")+ylab("Applicants Count") + 
ggtitle("Home Ownership of the Applicants") 



#------------------------------------------------- bi-variate--------------------------------------------------#

#annual income vs loan status
ggplot(loan, aes(loan$loan_status, loan$annual_inc))+ 
	geom_boxplot() + ylim(0,150000) +
	labs(title = "Annual income vs Loan Status", x = "Loan Status",y = "Annual income")

################# Histogram - Grade vs Status #################
ggplot(loan, aes(x=factor(loan$grade), fill = loan$loan_status)) + 
  geom_bar(position = "dodge") +
  labs(title = "Grade vs Status",x = "Grade",y = "Count")


  
  
  

#considering the defaulter data

#filter by grades & subgrade
#filter by amount range
#--------------------------------------------------------#
# 1. Loan Amount <- The listed amount of the loan applied for by the borrower. If at some point in time, the credit department reduces the loan amount, then it will be reflected in this value.
# 2. Funded Amount <- The total amount committed to that loan at that point in time.
# 3. Funded Amount Inv <- The total amount committed by investors for that loan at that point in time.




