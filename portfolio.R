RESULTS_FILE = "~/R/data/results/output"

# Create a markovitz mean-variance portfolio
create_mv <- function(returns)
{	
	spec = portfolioSpec()
	print(spec)
	setTargetReturn(spec) = 0.05
	mv_port = minvariancePortfolio(returns, spec)
	print(mv_port)
	return(mv_port)
}

# Provide new weights for given strategy
rebalance <- function(custom_pfolio, date) 
{
	if (custom_pfolio@type == 1) {
		# minvariance portfolio
		val = update_mv_weights(custom_pfolio@mv_pfolio, date)
		custom_pfolio@mv_pfolio = val$mv_pfolio
		custom_pfolio@desired_weights = val$weights
	}
	else if (custom_pfolio@type == 2){
		# long short high freq
		custom_pfolio = update_ls_weights(custom_pfolio, date)
	}
	
	custom_pfolio
}

quarter <- function(date)
{
	if ((date >= "2007-09-01") && (date <= "2007-10-31")) {
		return(1)	
	}
	else if ((date >= "2007-11-01") && (date <= "2008-01-31")){
		return(2)
	}
	else if ((date >= "2008-02-01") && (date <= "2008-03-31")) {
		return(3)
	}
	else if ((date >= "2008-04-01") && (date <= "2008-05-31")) {
		return(4)
	}
	else
		return(5)
}

# Transaction cost for given ticket on given date
get_tc <- function(ticker_idx, n_shares, date, pfolio)
{
	date_idx = quarter(date)

	m_cap = as.numeric(pfolio@mcaps_series[date_idx, ticker_idx])
	
	vol = as.numeric(pfolio@vol_series[date, ticker_idx])
	volume = as.numeric(pfolio@volume_series[date, ticker_idx])
	
	fixed = 0.02 * n_shares
	slippage = (1e3/m_cap) * (1 + n_shares/volume + vol)
	
	return(fixed + slippage)
}

# Adjust weight to factor for transaction costs, etc
adjusted_weights_buy <- function(weight, symbol_idx, date, pfolio)
{
	# Total allocation for this purchase
	allocation = weight * pfolio@current_value

	# Price without transaction cost
	price_vanilla = as.numeric(pfolio@closing_prices[date, symbol_idx]) 

	# Total shares
	n_shares = trunc(allocation/price_vanilla)
	tc = get_tc(symbol_idx, n_shares, date, pfolio)
	
	n_shares = trunc((allocation - tc)/(price_vanilla))
	
	equity_cost = price_vanilla * n_shares
	adj_weight = equity_cost/pfolio@current_value
	
	# Transaction costs are always positive
	tcost = tc
	tc_weight = abs(tcost/pfolio@current_value)
	
	# Return desired parameters
	ret_val = list()
	ret_val$adj_weight = adj_weight
	ret_val$tc_weight = tc_weight
	ret_val$equity_cost = equity_cost
	ret_val$tcost = tcost
	ret_val$net_cost = tcost + equity_cost
	ret_val$n_shares = n_shares
	
	ret_val
}

adjusted_weights_sell <- function(weight, symbol_idx, date, pfolio)
{
	allocation = weight * pfolio@current_value
	
#	cat ("$$ ALLOCATION is ", allocation, "weigt", weight, "\n\n\n")

	price_vanilla = as.numeric(pfolio@closing_prices[date, symbol_idx]) 
	
	n_shares = round(allocation/price_vanilla)
	tcost = get_tc(symbol_idx, abs(n_shares), date, pfolio)
	tc_weight = tcost/pfolio@current_value
	
	ret_val = list()
	ret_val$adj_weight = (price_vanilla * n_shares)/pfolio@current_value
	ret_val$tc_weight = tc_weight
	ret_val$equity_cost = price_vanilla * n_shares
	ret_val$tcost = tcost
	ret_val$net_cash = allocation - tcost
	ret_val$n_shares = n_shares
	
	ret_val
}

# TODO: This will work for long-only portfolio
update_to_market <- function(custom_pfolio, date)
{
	current_nshares = custom_pfolio@current_nshares
	closing_prices = custom_pfolio@closing_prices
	
	# We will recalculate total value
	current_equity = 0
	current_lever = 0
	
	for (i in 1:length(current_nshares)) {
		symbol = custom_pfolio@active_instruments[[i]]@symbol
		n_shares = current_nshares[i]
		
		# Find closing price on given date
		closing_price = as.numeric(closing_prices[date, symbol])
		
		custom_pfolio@current_values[i] =  n_shares * closing_price
		
		# Update total value
		if (n_shares >= 0) # Long
			current_equity = current_equity + custom_pfolio@current_values[i]
		else # Short
			current_lever = current_lever + custom_pfolio@current_values[i]
	}
	
	custom_pfolio@current_equity = current_equity
	custom_pfolio@current_lever = current_lever
	custom_pfolio@current_value = current_equity + custom_pfolio@current_cash + current_lever
	
	for (i in 1:length(current_nshares)) {
		custom_pfolio@current_weights[i] = custom_pfolio@current_values[i]/custom_pfolio@current_value
	}
	
	custom_pfolio
}

# Portfolio backtesting
backtest <- function(custom_pfolio, output_file)
{
	dates = rownames(custom_pfolio@returns_series)
	
	cat ("Backtesting from", , "to", end_at, "\n")
	
#	dates = dates[start_at:end_at]
	
	# Iterate through each date
	for (k in 1:length(dates)) {
		# Current date
		date = k

		# Update prices etc for the portfolio
		custom_pfolio = update_to_market(custom_pfolio, date)
		
		# Now, do any rebalancing
		custom_pfolio = rebalance(custom_pfolio, date)
		weights = custom_pfolio@desired_weights
	
		cat("LS ratio,", wt_sum(weights[weights > 0])/wt_sum(weights[weights < 0]), "\n")

		# Move on if no new weights computed
		if (is.na(weights[1])) {
			next
		}
		cat("SUM", sum(weights), "\n", "Raw weights", weights, "\n")
		
		cat ("*******************", dates[k], "***********************8\n")
		 
		print_pfolio(custom_pfolio, "", output_file)
		
		cached_current_value = custom_pfolio@current_value
		
		# 
		for (i in 1:length(weights)) {
	
			# Extract all ticket symbols
			symbol = custom_pfolio@active_instruments[[i]]@symbol
			
			# Extract old weights
			old_weight = custom_pfolio@current_weights[i]
			new_weight = weights[i] 
			
			# Rebalance fraction
			wt_diff = new_weight - old_weight
			
			if (wt_diff > 0) { # Buying/Buying back
				wt_params = adjusted_weights_buy(wt_diff, i, date, custom_pfolio)
		 
				custom_pfolio@current_cash = custom_pfolio@current_cash - wt_params$net_cost
			}
			else if (wt_diff < 0) { # Selling/Selling short
				wt_params = adjusted_weights_sell(wt_diff, i, date, custom_pfolio)

				custom_pfolio@current_cash = custom_pfolio@current_cash - wt_params$net_cash
			}
			
			else {
				wt_params  = list()
				wt_params$tcost = 0
				wt_params$adj_weight = 0
				wt_params$n_shares  = 0
				wt_params$equity_cost = 0
			}
					
			custom_pfolio@current_nshares[i] = custom_pfolio@current_nshares[i] + wt_params$n_shares
			custom_pfolio@current_values[i] = custom_pfolio@current_values[i] + wt_params$equity_cost
			
			# We lose value equal to transaction cost
			cached_current_value = cached_current_value - wt_params$tcost			
			
			# Finally, set the new weigts in portfolio
			custom_pfolio@current_weights[i] = custom_pfolio@current_weights[i] + wt_params$adj_weight
			
			# Keep track of transaction costs 
			custom_pfolio@current_net_tc = custom_pfolio@current_net_tc + wt_params$tcost
		}
		
		# Update long and short weights arrays
		long_idxs = which(!is.na(custom_pfolio@current_long_wts))
		short_idxs = which(!is.na(custom_pfolio@current_short_wts))
		custom_pfolio@current_long_wts[long_idxs] = custom_pfolio@current_weights[long_idxs]
		custom_pfolio@current_short_wts[short_idxs] = custom_pfolio@current_weights[short_idxs]
		
		# Update total value
		custom_pfolio@current_value = cached_current_value
		
#		print_pfolio(custom_pfolio, "")
		cat("\n----------------------", k, "----------------------------\n")
	}
	
	print("!!!!!!!!!!!!!!!!EXITING")
	custom_pfolio
}

print_pfolio <- function(pfolio, random_string, output_file)
{ 
	cat("!!", random_string, "!!\n")
#	cat("Current shares", pfolio@current_nshares, "\n")
#	cat("Current weights", pfolio@current_weights, "\n")
#	cat("Current values", pfolio@current_values, "\n")
	
	net_long_wt = wt_sum(pfolio@current_weights[pfolio@current_weights > 0])
	net_short_wt = wt_sum(pfolio@current_short_wts[pfolio@current_weights < 0])
	ls_ratio = net_long_wt/net_short_wt
	cash_wt = get_cash_weight(pfolio)
	
	cat("Current LS ratio,", ls_ratio, "\n")
	cat("D CASH", cash_wt, "\n")
	cat("TOTAL LONG", net_long_wt, "\n")
	cat("TOTAL SHORT", net_short_wt, "\n") 
	cat("TOTAL EQUITY", pfolio@current_equity, "\n")
	cat("TOTAL LEVER", pfolio@current_lever, "\n")
	cat("TOTAL CASH", pfolio@current_cash, "\n")
	cat("TOTAL TC", pfolio@current_net_tc, "\n")
	cat("TOTAL VALUE", pfolio@current_value, "\n\n")
	
	out_string = paste(ls_ratio, net_long_wt, net_short_wt, cash_wt, pfolio@current_equity, 
					   pfolio@current_lever, pfolio@current_cash, pfolio@current_net_tc, 
					   pfolio@current_value, sep = ",")
	
	write(out_string, output_file, append = TRUE, ncolumns = 9)
}

####################################################
# High-frequency long short portfolio manipulation #
####################################################
# Compute bias towards long or short. It just ratio of long to short
ls_get_bias <- function(long_wts, short_wts)
{
	long_wts = as.numeric(na.omit(as.vector(long_wts)))
	short_wts = as.numeric(na.omit(as.vector(short_wts)))
	
	abs(sum(na.omit(long_wts))/sum(na.omit(short_wts)))
}

# Sum of weights in weight array
wt_sum <- function(wts)
{
	wts = as.vector(wts)
	wts = na.omit(wts)
	wts = as.numeric(wts)
	sum(wts)
}

# Count of items in weights array
ls_count <- function(wts)
{
	wts = as.vector(wts)
	wts = na.omit(wts)
	wts = as.numeric(wts)
	length(wts)
}

get_allocation_method <- function(pfolio)
{
	long_wt = wt_sum(pfolio@current_long_wts)
	if (long_wt < .7)
		return(3)
}

# Update weights short long/short porfolio strategy
update_ls_weights <- function(custom_pfolio, date)
{
	market_mean = as.numeric(custom_pfolio@forecast_series[date, ])
	damping_factor = 50
	
	confidence = market_mean # market_mean/market_dev

	current_bias = ls_get_bias(custom_pfolio@current_long_wts, custom_pfolio@current_short_wts)

	# Tweak weights and try again if we dont have a bias
	if (is.nan(current_bias) || is.na(current_bias)) {
		idx = which(!is.na(custom_pfolio@current_long_wts))
		custom_pfolio@current_long_wts[idx] = 0.005
		
		idx = which(!is.na(custom_pfolio@current_short_wts))
		custom_pfolio@current_short_wts[idx] = -0.005
		
		current_bias = ls_get_bias(custom_pfolio@current_long_wts, custom_pfolio@current_short_wts)
	}
	
	new_bias = current_bias + (current_bias * confidence) * damping_factor
	bias_diff = new_bias - current_bias
	
	# Cache weights
	long_wts = custom_pfolio@current_long_wts
	short_wts = custom_pfolio@current_short_wts
	cash_wt = get_cash_weight(custom_pfolio)

	# Time series of volatilites
	vols = as.vector(custom_pfolio@vol_series[date, ])
	
	cat("BIAS OLD", current_bias, "BIAS NEW", new_bias, "DIFF", bias_diff, 
	    "CONF:", confidence, "longwt", wt_sum(long_wts), "shortwt", wt_sum(short_wts), "\n")

	#Sys.sleep(1)
	
	if (bias_diff > 0) { # Go longer	
		print("GOING LONGER")
		if (wt_sum(long_wts) > 0.8) choice = 2 else choice = 1
		new_weights = ls_allocate_longer(long_wts, short_wts, new_bias, cash_wt, vols, choice)
	}
	else if (bias_diff < 0) { # Go shorter
		print("GOING SHORTER")
		if (wt_sum(short_wts) < -0.8) choice = 1 else choice = 2
		new_weights = ls_allocate_shorter(long_wts, short_wts, new_bias, cash_wt, vols, choice)
	}
	else {
		# No changes to be made
		return(custom_pfolio@current_weights)
	}
	
	retval = list()
	longs = new_weights$longs
	shorts = new_weights$shorts
	
	all_weights = rbind(longs, shorts)
	new_weights = na.omit(as.vector(all_weights))	
	custom_pfolio@desired_weights = as.array(new_weights)
	
	custom_pfolio
}

########################################
# Mean-variance portfolio manipulation #
########################################
# Update weights using minvariance portfolio
update_mv_weights <- function(mv_pfolio, date)
{	
	ret_val = list()

	ret_val$mv_pfolio = mv_pfolio
	ret_val$weights = array()
	
	# Is this the first time I am being asked for weights
	if (mv_pfolio@last_update == "") {
		ret_val$mv_pfolio@last_update = date
		return(ret_val)
	}
	
	# Do not update unless its time to update
	if ((as.Date(mv_pfolio@last_update) + mv_pfolio@update_frequency) > as.Date(date)) {	
		return (ret_val)
	}
	
	# Need return up to the specified date
	series = cut(mv_pfolio@returns, mv_pfolio@last_update, date)
	
	# minvariance portfolio
	mv_pfolio@pfolio_object = minvariancePortfolio(data = series, spec = portfolioSpec())
	
#	print(mv_pfolio@pfolio_object)
	
#	print("ABOUT TO GET WEIGHTS")
	# Extract newly assigned weights
	ret_val$weights = getWeights(mv_pfolio@pfolio_object)
	
	# We are updating weights
	ret_val$mv_pfolio@last_update = date
	
	ret_val
}

init_mvportfolio <- function(historical_returns)
{
	mv_pfolio = new("mv_portfolio", update_frequency = 10,
								    returns = historical_returns
									
					)
					
	mv_pfolio@last_update = ""
	return(mv_pfolio)				
}

# Rolling portfolio
rolling_mv <- function (data, spec, constraints, from, to, action = NULL, title = NULL, description = NULL, ...) 
{
    roll = list()
    for (i in 1:length(from)) {
        series = cut(data, from = from[i], to = to[i])
		
		# TODO: With constraints, I am getting an error
        portfolio = minvariancePortfolio(data = series, spec)
        roll[i] = portfolio
        if (!is.null(action)) {
            fun = match.fun(action)
            fun(roll, from, to, ...)
        }
    }
    invisible(roll)
}

############################
# Basic Portfolio Routines #
############################
# Select what we want to short and what we want to long
get_shorts_and_longs <- function(instruments)
{
	n_ins = length(instruments)
	retval = list()
	set.seed = 2
	# List of tickers
	tickers = c()
	
	# Construct list of ticker
	for (i in 1:n_ins) {
		tickers = append(tickers, instruments[[i]]@symbol)	
	}
	
	rarr = (runif(n_ins, 1, n_ins))
	s_idxs = which(rarr > n_ins/2)
	l_idxs = which(rarr <= n_ins/2)
	
	retval$shorts = as.array(tickers[s_idxs])
	retval$longs = as.array(tickers[l_idxs])
	
	#retval$shorts = as.array(tickers[1:trunc(n_ins/2)])
	#retval$longs = as.array(tickers[(trunc(n_ins/2) + 1):n_ins])
	retval
}

# Fraction of cash at hand
get_cash_weight <- function(pfolio)
{
	pfolio@current_cash/pfolio@current_value
}

get_shorts_ceiling <- function(pfolio)
{
	return (-1.0)	
}

setup_mcap <- function(instruments, DATA_DIRECTORY)
{
	for (i in 1:length(instruments)) {
		symbol = instruments[[i]]@symbol
		print(symbol)
		cap_file = paste(DATA_DIRECTORY, "market/", symbol, ".csv", sep = "")
		mcaps = read.csv(cap_file, header = FALSE, row.names = 1)
		dates = format(rownames(mcaps), format = "%m/%d/%Y %I:%M:%S %p")
		mcaps = timeSeries(mcaps, charvec = dates, format = "%m/%d/%Y %I:%M:%S %p", units = instruments[[i]]@symbol)
			
		rows = rownames(mcaps)
		
		start_at = which((as.Date(rows) > as.Date("2007-07-01")))[1]   
		end_at = which((as.Date(rows) > as.Date("2008-07-01")))[1]
		
		period = rows[start_at:end_at]
		
		mcaps = mcaps[period, ]
		
		rownames(mcaps) = seq(1:length(period))
		if (!exists("mcaps_all"))
			mcaps_all = mcaps
		else
			mcaps_all = cbind(mcaps_all, mcaps)
	}	
	mcap_all
}

# Create a custom portfolio
create_portfolio <- function(instruments, cash, rows_to_read = 20554)
{
	PORTFOLIO = new("portfolio", active_instruments = instruments, 
					             current_value = cash,
								 current_cash = cash,
								 current_equity = 0)
	
								 
    # Weight per equity							
	weight = PORTFOLIO@max_equity_value/length(PORTFOLIO@active_instruments)

	# The desired weight vector for portfolio
	PORTFOLIO@desired_weights = as.array(rep(weight, length(PORTFOLIO@active_instruments)))
	
	# Current weights shoul be set to zero
	PORTFOLIO@current_weights = as.array(rep(0, length(PORTFOLIO@active_instruments)))
	PORTFOLIO@current_nshares = as.array(rep(0, length(PORTFOLIO@active_instruments)))
	PORTFOLIO@current_values = as.array(rep(0, length(PORTFOLIO@active_instruments)))

	# Market cap values
	mcaps_file = paste(DATA_DIRECTORY, "mcaps_series.csv", sep = "")
	if (!file.exists(mcaps_file)) {
		PORTFOLIO@mcaps_series = setup_mcap(instrument, DATA_DIRECTORY)		
		write.csv(mcaps_all, mcaps_file)
	}
	else {
		PORTFOLIO@mcaps_series = as.timeSeries(read.csv(mcaps_file, row.names = 1))
	}
	
	print("Market Cap Done")
		
	# Historical volatility
	vol_file = paste(DATA_DIRECTORY, "vol_series.csv", sep = "/")
	if (!file.exists(vol_file)) {
		PORTFOLIO@vol_series = get_volatility(instruments)
		write.csv(PORTFOLIO@vol_series, vol_file)
	}
	else {
		PORTFOLIO@vol_series = as.timeSeries(read.csv(vol_file, row.names = 1, nrows = rows_to_read))
	}
	
	# Need to remove NA
	dates_original = rownames(PORTFOLIO@vol_series)
	PORTFOLIO@vol_series = na.omit(PORTFOLIO@vol_series)
	dates_to_use = rownames(PORTFOLIO@vol_series)
	
	print("Volatility done")
	
	# Volume
	volume_file = paste(DATA_DIRECTORY, "volume_series.csv", sep = "/")
	if (!file.exists(volume_file)) {
		PORTFOLIO@volume_series = get_volume(instruments)
		write.csv(PORTFOLIO@volume_series, volume_file)
	}
	else {
		PORTFOLIO@volume_series = as.timeSeries(read.csv(volume_file, row.names = 1, nrows = rows_to_read)) 
	}
	
	PORTFOLIO@volume_series = PORTFOLIO@volume_series[dates_to_use, ]
	print("Volume Done")
	
	# Index forecast
	index_file = paste(DATA_DIRECTORY, "index.csv", sep = "/")
	PORTFOLIO@forecast_series = as.timeSeries(read.csv(index_file,
													   row.names = dates_original, 
													   nrows = rows_to_read)) 
	PORTFOLIO@forecast_series = PORTFOLIO@forecast_series[dates_to_use, ]
	
	# Historical returns
	returns_file = paste(DATA_DIRECTORY, "returns_series.csv", sep = "/")
	if (!file.exists(returns_file)) {
		PORTFOLIO@returns_series = get_returns(instruments)
		write.csv(PORTFOLIO@returns_series, returns_file)
	}
	else {
		PORTFOLIO@returns_series = as.timeSeries(read.csv(returns_file, row.names = 1, nrows = rows_to_read)) 
	}
	PORTFOLIO@returns_series = PORTFOLIO@returns_series[dates_to_use, ]
	print("Returns Series done")
	
	# Historical closing prices
	cprices_file = paste(DATA_DIRECTORY, "cprices_series.csv", sep = "/")
	if (!file.exists(cprices_file)) {
		PORTFOLIO@closing_prices = get_closing_prices(instruments)
		write.csv(PORTFOLIO@closing_prices, cprices_file)
	}
	else {
		PORTFOLIO@closing_prices = as.timeSeries(read.csv(cprices_file, row.names = 1, nrows = rows_to_read))
	}
	PORTFOLIO@closing_prices = PORTFOLIO@closing_prices[dates_to_use, ]
	print("Historical Prices done")
	
	# Setup longs and shorts
	ret = get_shorts_and_longs(instruments)
	PORTFOLIO@current_shorts = ret$shorts
	PORTFOLIO@current_longs = ret$longs
	PORTFOLIO@current_long_wts = as.array(rep(NA, length(PORTFOLIO@active_instruments)))
	PORTFOLIO@current_short_wts = as.array(rep(NA, length(PORTFOLIO@active_instruments)))
	for (i in 1:length(instruments)) {
		if (instruments[[i]]@symbol %in% PORTFOLIO@current_longs) {
			PORTFOLIO@current_long_wts[i] = 0
		}
		else {
			PORTFOLIO@current_short_wts[i] = 0
		}
	}
	
	# Time series for transaction costs
	all_dates = rownames(PORTFOLIO@closing_prices)
	tc_init = rep(0, length(all_dates))
	
	PORTFOLIO@current_cash = cash
	PORTFOLIO@current_equity = 0
	PORTFOLIO@current_lever = 0
	PORTFOLIO@current_net_tc = 0
	
	# Set as minvariance portfolio
	PORTFOLIO@type = 2
		
	return(PORTFOLIO)
}

pfolio_subset <- function(pfolio, start)
{
	total_rows = nrow(pfolio@returns_series)
	
	pfolio@returns_series = pfolio@returns_series[start:total_rows, ]
	pfolio@closing_prices = pfolio@closing_prices[start:total_rows, ]
	pfolio@vol_series = pfolio@vol_series[start:total_rows, ]  
	pfolio@forecast_series = pfolio@forecast_series[start:total_rows, ]
	pfolio@volume_series = pfolio@volume_series[start:total_rows, ]
	pfolio
}

# Create a custom portfolio
create_portfolio_fast <- function(pfolio_slow, pfolio_last, start, end)
{		
        pfolio_fast = new("portfolio", active_instruments = pfolio_slow@active_instruments, 
                                     current_value = pfolio_last@current_value,
                                     current_cash = pfolio_last@current_cash,
                                     current_equity = pfolio_last@current_equity)

        # The desired weight vector for portfolio
        pfolio_fast@desired_weights = pfolio_last@desired_weights

        # Current weights shoul be set to zero
        pfolio_fast@current_weights = pfolio_last@current_weights
        pfolio_fast@current_nshares = pfolio_last@current_nshares
		pfolio_fast@current_values = pfolio_last@current_values
		
        # Market cap values
        pfolio_fast@mcaps_series = pfolio_last@mcaps_series
        pfolio_fast@returns_series = pfolio_slow@returns_series[start:end, ]
        pfolio_fast@closing_prices = pfolio_slow@closing_prices[start:end, ]
        pfolio_fast@vol_series = pfolio_slow@vol_series[start:end, ]
        pfolio_fast@forecast_series = pfolio_slow@forecast_series[start:end, ]
        pfolio_fast@volume_series = pfolio_slow@volume_series[start:end, ]
 
        pfolio_fast@current_shorts = pfolio_last@current_shorts
        pfolio_fast@current_longs = pfolio_last@current_longs
        pfolio_fast@current_long_wts = pfolio_last@current_long_wts
        pfolio_fast@current_short_wts = pfolio_last@current_short_wts

        pfolio_fast@current_cash = pfolio_last@current_cash
        pfolio_fast@current_equity = pfolio_last@current_equity
        pfolio_fast@current_lever = pfolio_last@current_lever
        pfolio_fast@current_net_tc = pfolio_last@current_net_tc

        # Set as minvariance portfolio
        pfolio_fast@type = pfolio_last@type

        return(pfolio_fast)
}

