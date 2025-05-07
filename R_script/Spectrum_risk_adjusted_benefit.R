library(readr)
library(decisionSupport)

input_estimates <- estimate_read_csv("estimates.csv")

model_function <- function(){
	# subgraph Risk_adjusted_benefit
	Risk_adjusted_benefit <- function(If_change, If_high_attention, If_low_confounders, If_understandable){
		# Subgraph Output
		Comparison <- (If_high_attention < If_understandable) * 1
		Math <- Comparison * If_high_attention
		Math_2 <- 1 - Comparison
		Math_3 <- Math_2 * If_understandable
		Math_4 <- Math + Math_3
		Comparison_2 <- (If_change < If_low_confounders) * 1
		Math_5 <- Comparison_2 * If_change
		Math_6 <- 1 - Comparison_2
		Math_7 <- Math_6 * If_low_confounders
		Math_8 <- Math_5 + Math_7
		Comparison_3 <- (Math_4 < Math_8) * 1
		Math_9 <- Math_4 * Comparison_3
		Math_10 <- 1 - Comparison_3
		Math_11 <- Math_10 * Math_8
		Math_12 <- Math_9 + Math_11
		Risk_adjusted_benefit_2 <- Math_12
		
		return(list(Risk_adjusted_benefit_2=Risk_adjusted_benefit_2))
	}

	# Risk_adjustment
	Risk_adjusted_benefit_list <- Risk_adjusted_benefit(If_change, If_high_attention, If_low_confounders, If_understandable)
	Risk_adjustment <- Risk_adjusted_benefit_list$Risk_adjusted_benefit_2
	
	# generate list of output variables
	return(list(Risk_adjustment=Risk_adjustment))
}


mc <- mcSimulation(estimate=input_estimates,
		model_function=model_function,
		numberOfModelRuns=50000,
		functionSyntax='plainNames')


write_csv(data.frame(mc["y"]), "/tmp/decision_ui_result_45wv0bzb.csv")