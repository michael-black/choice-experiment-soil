rm(list = ls())
if (!require('pacman')) install.packages('pacman')
pacman::p_load(support.CEs, AlgDesign, tidyverse)

## Iterate over random designs until practical design considerations are met
b = 99
d = 99
i = 0
while ((b > 0) || (d > 1)){
  i = i + 1
## Design pre-test survey with no prior information using rotation method
des=rotation.design(
  attribute.names=list(
    WI = c("0.1", "0.2", "0.3"), # water infiltration rates
    OM = c("0.5", "1", "2.5"), # organic matter rates
    Comp = c("20","10","0"), # arbitrary compaction values
    Price = c("19", "25", "30")), # price per acre
  nalternatives = 2, # number of alternatives not including the opt out option
  nblocks=1, 
  row.renames = FALSE, 
  randomize = TRUE,
  seed=i) # set seed for replicability; otherwise a new design occurs each iteration
design <- make.design.matrix(
  choice.experiment.design = des,
  optout = TRUE, # thrid alternative of opt-out
  continuous.attributes = c("OM","Comp", "WI", "Price"),
  unlabeled = TRUE)
## Check for undesirable combinations or dominated options
check <- design %>%
  filter(ALT != 3) %>% # for checking unrealistic combos, we don't need to consider opt out
  dplyr::select(QES, ALT, WI, OM, Comp, Price) %>%
  pivot_wider(names_from = c(ALT), values_from = c(WI, OM, Comp, Price)) %>%
  mutate(bad = ifelse(WI_1 > WI_2 & OM_1 <= OM_2 & Comp_1 >= Comp_2, 1, 0),
		bad2 = ifelse(WI_1 < WI_2 & OM_1 >= OM_2 & Comp_1 <= Comp_2, 1, 0),
		dom = ifelse(WI_1>=WI_2 & OM_1>=OM_2 & Comp_1<=Comp_2 & Price_1<=Price_2, 1, 0),
		dom2 = ifelse(WI_1<=WI_2 & OM_1<=OM_2 & Comp_1>=Comp_2 & Price_1>=Price_2, 1, 0))
## note: 'bad' is relative to our scenario, 'dom' is when one alt totally dominates another
d <- sum(check$dom)
d2 <- sum(check$dom2)
d = d+d2
b <- sum(check$bad)
b2 <- sum(check$bad2)
b = b + b2
}

#### Once you have found a good design, make note of the most recent seed (i) and re-run the following:
		# des=rotation.design(
		#   attribute.names=list(
		#     WI = c("0.1", "0.2", "0.3"), # water infiltration rates
		#     OM = c("0.5", "1", "2.5"), # organic matter rates
		#     Comp = c("20","10","0"), # arbitrary compaction values
		#     Price = c("19", "25", "30")), # price per acre
		#   nalternatives = 2, # number of alternatives not including the opt out option
		#   nblocks=1, 
		#   row.renames = FALSE, 
		#   randomize = TRUE,
		#   seed=i) # set seed for replicability; otherwise a new design occurs each iteration
		# design <- make.design.matrix(
		#   choice.experiment.design = des,
		#   optout = TRUE, # thrid alternative of opt-out
		#   continuous.attributes = c("OM","Comp", "WI", "Price"),
		#   unlabeled = TRUE)
		# ## Check for undesirable combinations or dominated options
		# check <- design %>%
		#   filter(ALT != 3) %>% # for checking unrealistic combos, we don't need to consider opt out
		#   dplyr::select(QES, ALT, WI, OM, Comp, Price) %>%
		#   pivot_wider(names_from = c(ALT), values_from = c(WI, OM, Comp, Price)) %>%
		#   mutate(bad = ifelse(WI_1 > WI_2 & OM_1 <= OM_2 & Comp_1 >= Comp_2, 1, 0),
		# 		bad2 = ifelse(WI_1 < WI_2 & OM_1 >= OM_2 & Comp_1 <= Comp_2, 1, 0),
		# 		dom = ifelse(WI_1>=WI_2 & OM_1>=OM_2 & Comp_1<=Comp_2 & Price_1<=Price_2, 1, 0),
		# 		dom2 = ifelse(WI_1<=WI_2 & OM_1<=OM_2 & Comp_1>=Comp_2 & Price_1>=Price_2, 1, 0))


### Save final version
# write.csv(check, file = "./Build/Output/final_pretest_design.csv")
