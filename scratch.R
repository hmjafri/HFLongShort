get_unit_weight <- function(instrument_idx, date) 
{
	instrument = portolio[instrument_idx]
	price = instrument$history[date]$Close	
	
	unit_weight = price/portfolio$current_value
	return(unit_weight)
}	

set_weight <- function(instrument_idx, new_weight, date)
{
	instrument = portolio[instrument_idx]
	price = instrument$history[date]$Close
	total_price = new_weight * portfolio$current_value	
	n_shares = total_price/price
	
	# Adding to the portfolio	
	if (new_weight > 0) {	
		buy(instrument, n_shares, price)
	}
	else {
		sell(instrument, n_shares, abs(price))			
	}
}

adjust_weight <- function(instrument_idx, current_weight, desired_weight, available_weight, current_date)
{
	r_value = instrument$history$R_val
	r_val_threshold = 0.50	
	
	if(r_val > r_val_threshold) {
		weight_diff = desired_weight - current_weight
		smallest_weight_unit = get_unit_weight(instrument_idx, current_date)
		n_units_to_adjust = trunc(weight_diff/smallest_weight_unit)
		final_weight = smallest_weight_unit * n_units_to_adjust
		weight_error = desired_weight - final_weight 
		
		if (current_weight == 0) {
			set_weight(instrument_idx, final_weight, current_date) 	
		}
		else {
			reset_weight(instrument_idx, final_weight, current_date)
		}
	}
	else {
		reset_weight(instrument_idx, 0)	
	}
}