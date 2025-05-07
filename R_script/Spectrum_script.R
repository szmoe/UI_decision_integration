library(readr)
library(decisionSupport)

input_estimates <- estimate_read_csv("estimates.csv")

model_function <- function(){
	# subgraph NPV_5_year
	NPV_5_year <- function(Discount, Sample){
		# NPV
		To_Series <- rep(Sample, 5)
		Net_Present_Value <- discount(To_Series, Discount, calculate_NPV=TRUE)
		NPV <- Net_Present_Value
		
		return(list(NPV=NPV))
	}

	# subgraph Subgraph
	Subgraph <- function(){
		return(list())
	}

	# subgraph Spectrum_model
	Spectrum_model <- function(Contest_cost, Discount, Healthy_food_choice, Improved_knowledge, Influencer_cost, Information_value, Motivation_to_change, Nutritionist_cost, Positive_health_effect, Risk_adjustment, Youtube_cost){
		# Subgraph Output
		Total_benefit <- Information_value + Improved_knowledge + Motivation_to_change + Healthy_food_choice + Positive_health_effect
		Math <- Risk_adjustment * Total_benefit
		Total_cost <- Youtube_cost + Contest_cost + Influencer_cost + Nutritionist_cost
		NPV <- Math - Total_cost
		NPV_5_year_list <- NPV_5_year(Discount, NPV)
		NPV_2 <- NPV_5_year_list$NPV
		
		return(list(NPV_2=NPV_2))
	}

	# Result
	Spectrum_model_list <- Spectrum_model(Contest_cost, Discount, Healthy_food_choice, Improved_knowledge, Influencer_cost, Information_value, Motivation_to_change, Nutritionist_cost, Positive_health_effect, Risk_adjustment, Youtube_cost)
	Result <- Spectrum_model_list$NPV_2
	
	# generate list of output variables
	return(list(Result=Result))
}


mc <- mcSimulation(estimate=input_estimates,
		model_function=model_function,
		numberOfModelRuns=50000,
		functionSyntax='plainNames')


write_csv(data.frame(mc["y"]), "/tmp/decision_ui_result_gier2_n2.csv")