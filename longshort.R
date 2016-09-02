RAMP_FACTOR = 1.4
DAMP_FACTOR = 0.1

################################################################################
# Decrease short position only
longer_short_only <- function(wl, ws, f)
{
	new_short_diff = abs(ws) - wl/f
	new_short_diff
}

# Increase long position only
longer_long_only <- function(wl, ws, f)
{
	new_long_diff = f * abs(ws) - wl
	new_long_diff
}

# Change both short and long For abs(f) > 1
longer_long_short <- function(wl, ws, f) 
{ 
	wl = abs(wl)
	ws = abs(ws)
	f = abs(f)
	
	val = list()
	fraction = (f*ws - wl)/(f - 1)
	val$wl = abs(wl - fraction)
	val$ws = abs(ws - fraction)
	val
}

################################################################################
vol_to_weight <- function(vols, wt) 
{
	vols = 1/vols
	vol_sum = wt_sum(vols)
	
	factor = wt/vol_sum
	wts = vols * factor
	if(wt_sum(wts) == 0) { print(vols); dfdfd}
	wts
}

equal_weights <- function(orig_wts, diff_wt, n_wts, vols, method = 2)
{	
	if (method == 1) {
		new_wts = orig_wts + diff_wt/n_wts
	}
	else {
		# Indices of interest. All selected either long or short
		idxs = which(is.na(orig_wts))
		
		# Volatilites of interst
		vols[idxs] = NA
	
		# New weights based on volatilites
		new_wts = vol_to_weight(vols, wt_sum(orig_wts) + diff_wt)
	}
	
	new_wts
}

################################################################################

# Push higher towards long side
ls_allocate_longer <- function(long_wts, short_wts, new_ls_ratio, cash_wt, vols, choice = 1)
{
	# This is what we will calculate
	new_weights = list()
	
	# count of longs and shorts
	n_longs = ls_count(long_wts)
	n_shorts = ls_count(short_wts)
	
	# Total weight of longs and shorts
	total_long_wt = wt_sum(long_wts)
	total_short_wt = wt_sum(short_wts)
	saved_short = short_wts
	if (choice == 1) {# Only buy more, if possible
		
		# Get weight to add to longs 
		diff = longer_long_only(total_long_wt, total_short_wt, new_ls_ratio)
		
		# We are scaling weights, using cash_wt is parameter
		long_wt_candidate = exp(cash_wt * DAMP_FACTOR) * (total_long_wt + diff)
		new_long_wt = long_wt_candidate  - total_long_wt
		short_wt_candidate = exp(cash_wt * DAMP_FACTOR) * (total_short_wt)
		new_short_wt = short_wt_candidate - total_short_wt
		
		if (cash_wt >= new_long_wt) { # If enough cash is available
			# Uniformly inrease weights of longs
			long_wts = equal_weights(long_wts, new_long_wt, n_longs, vols) 
			short_wts = equal_weights(short_wts, new_short_wt, n_shorts, vols)
			
			cat("LONGS", wt_sum(long_wts), "SHORT", wt_sum(short_wts), "\n")
		}
		else { # Not enough cash. Use up all we have
			long_wts = equal_weights(long_wts, cash_wt, n_longs, vols) 
			
			# Call again. TODO: We are not updateing cash_wt
			retval = ls_allocate_longer(long_wts, short_wts, new_ls_ratio, cash_wt, vols, choice = 2)
			long_wts = retval$longs
			short_wts = retval$shorts
		}
	}
	
	else if (choice == 2) { # Only cover short, as much as possible
		diff = longer_short_only(wt_sum(long_wts), wt_sum(short_wts), new_ls_ratio)
		cat("CRASH POINT cash_wt", cash_wt, "DIFF", diff, "ls", new_ls_ratio, "\n")
		if (cash_wt >= diff) {# Enough cash to cover shorts
			# Uniformly decreases weights on shorts
			short_wts = equal_weights(short_wts, diff, n_shorts, vols) 
		}
		else { # If not enough cash, use all we have. Cant do anything else
			short_wts = equal_weights(short_wts, cash_wt, n_shorts, vols) 
		}
	}
	
	else { # Tweak both long and short
		
		print("NOT YET IMPLEMENTED")
		total_longs = wt_sum(long_wts)
		total_shorts = wt_sum(short_wts)
		ret = longer_long_short(total_longs, total_shorts, new_ls_ratio)
				
		scale_fraction = ret$wl/total_longs
		
		scale_fraction = RAMP_FACTOR * scale_fraction
		
		long_wts = ret$wl/n_longs
		short_wts = ret$ws/n_shorts			
	}
	
	new_weights$shorts = short_wts
	new_weights$longs = long_wts
	new_weights
}

###############################################################################
shorter_short_only <- function(wl, ws, f)
{
	new_short_diff = wl/f - abs(ws)
	new_short_diff
}
	
shorter_long_only <- function(wl, ws, f)
{
	new_long_diff = wl - f * abs(ws)
	new_long_diff
}

shorter_long_short <- function(wl, ws, f) 
{ 
	val = (f*ws - wl)/(f - 1)
	val
}

###############################################################################

# Push higher towards short side
ls_allocate_shorter <- function(long_wts, short_wts, new_ls_ratio, cash_wt, vols, choice = 1)
{
	# This is what we will calculate
	new_weights = list()
	
	# count of longs and shorts
	n_longs = ls_count(long_wts)
	n_shorts = ls_count(short_wts)
	
	# Total weight of longs and shorts
	total_long_wt = wt_sum(long_wts)
	total_short_wt = wt_sum(short_wts)
		
	if (choice == 1) {# Only sell more, if possible
		
		# Get weight to substract from longs
		diff = shorter_long_only(total_long_wt, total_short_wt, new_ls_ratio)
		
		if (total_long_wt >= diff) { # If longs are enough to be reduced
			# Uniformly reduce weights of longs
			long_wts = equal_weights(long_wts, -diff, n_longs, vols)
		}
		else {# If longs are not enough, just short more
			diff = shorter_short_only(total_long_wt, total_short_wt, new_ls_ratio)
			
			# new total short weight
			new_short_wt = total_short_wt - diff
			
			# Changing short weights
			short_wts = equal_weights(short_wts, -diff, n_shorts, vols)			
		}
	}
	
	else if (choice == 2) { # tweak both shorts and longs
		diff = shorter_short_only(total_long_wt, total_short_wt, new_ls_ratio)
		
		# Limit of weight of shorts
		shorts_ceiling = get_shorts_ceiling()
		
		# We are scaling weights, using cash_wt is parameter
		long_wt_candidate = exp(cash_wt * DAMP_FACTOR) * (wt_sum(long_wts))
		new_long_wt = long_wt_candidate  - total_long_wt
		short_wt_candidate = exp(cash_wt * DAMP_FACTOR) * (total_short_wt - diff)
		new_short_wt = short_wt_candidate - total_short_wt
		
		if (shorts_ceiling <= short_wt_candidate ) {# We can short more
			# Uniformly increase shorts

			short_wts = equal_weights(short_wts, new_short_wt, n_shorts, vols)
			long_wts = equal_weights(long_wts, new_long_wt, n_longs, vols)
		}
		else { # We have already reached or approaching short limit
			short_wts = equal_weights(short_wts, (shorts_ceiling - total_short_wt), n_shorts, vols)	
			
			# Call again.  TODO: We are not updateing cash_wt
			retval = ls_allocate_shorter(long_wts, short_wts, new_ls_ratio, cash_wt, vols, choice = 1)
			long_wts = retval$longs
			short_wts = retval$shorts
		}
	}
	
	else { # LAST CHOICE: We have reached short limit and have no longs to sell
		print("We have reached short limit and have no longs to sell")
	
	}
	
#	print(short_wts)
#	print(long_wts)
	new_weights$shorts = short_wts
	new_weights$longs = long_wts
	
	new_weights
}
