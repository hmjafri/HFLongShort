# Compute volatility for the past n periods
compute_volatility <- function(instrument_history, period_length)
{
	dates = rownames(instrument_history)
	
	# Time series of returns
	returns = instrument_history[, "Returns"]
	
	# We must have more returns than period's length
	if (length(dates) <= period_length) {
		return(NULL)
	}

	
	for (i in (period_length): length(dates)) {
		period_returns = returns[(i - period_length + 1):i]	
		vol = sd(period_returns) * sqrt(255)
		
		if (exists("period_volatility")) 
			period_volatility = append(period_volatility, vol)
		else
			period_volatility = vol		
	}
	
	# Make a timeseries out of volatility
	period_volatility = timeSeries(period_volatility, 
	                               charvec = tail(dates, length(dates) - period_length + 1))
	
	# Attach volatility to instrument's historical data
	instrument_history =  cbind(instrument_history, period_volatility)
	
	# Rename column
	colnames(instrument_history)[ncol(instrument_history)] = "Volatility"
	
	return(instrument_history[(period_length:nrow(instrument_history)),])
}

compute_period_returns <- function(instrument_history, period)
{
	dates = rownames(instrument_history)	
	period_return = c(NA)

	# Calculate returns for each period
	for(i in (period + 1):length(dates)) {
		row = dates[i - period]
		last_close = as.numeric(instrument_history[row, "Close"])
		close = as.numeric(instrument_history[dates[i], "Close"])
		
		period_return = append(period_return, (close - last_close)/close)
	}

	period_return = timeSeries(period_return, charvec = dates, units = c("Returns"))
	
	# Merge returns with the instrument history
	instrument_history = cbind(instrument_history, period_return)

	# First row removed because we dont have return for it
	instrument_history
}

# Create timeSeries of closing prices of all instruments
get_closing_prices <- function(instruments) 
{	
	for (i in 1:length(instruments)) {
		history = instruments[[i]]@history
		
		if (!exists("closing_prices")) {
			closing_prices = history[, "Close"]
		}
		else {
			closing_prices = cbind(closing_prices, 	history[, "Close"])
		}
		
		colnames(closing_prices)[ncol(closing_prices)] = instruments[[i]]@symbol
	}
	
	return(closing_prices)
}

get_volume <- function(instruments)
{
	for (i in 1:length(instruments)) {
		history = instruments[[i]]@history
		cat("Processing", instruments[[i]]@symbol, "\n")
		if (!exists("volume")) {
			volume = history[, "Volume"]
		}
		else {
			volume = cbind(volume, history[, "Volume"])
		}
		
		colnames(volume)[ncol(volume)] = instruments[[i]]@symbol
	}
	
	return(volume)	
}

get_volatility <- function(instruments)
{
	for (i in 1:length(instruments)) {
		history = instruments[[i]]@history
		
		if (!exists("vol")) {
			vol = history[, "Volatility"]
		}
		else {
			vol = cbind(vol, history[, "Volatility"])
		}
		
		colnames(vol)[ncol(vol)] = instruments[[i]]@symbol
	}
	
	return(vol)
}

# Provides a timeSeries of returns for given instruments
get_returns <- function(instruments)
{
	for (i in 1:length(instruments)) {
		history = instruments[[i]]@history
		
		if (exists("returns_series")) {
			returns_series = cbind(returns_series, history[ ,"Returns"])
		}
		else {
			returns_series = history[ ,"Returns"]
		}
		
		colnames(returns_series)[i] = instruments[[i]]@symbol
	}
	
	return(returns_series)
}

# Create buy/sell signals for the historical instrument data
analyze_instruments <- function(instruments,  result_dir)
{
	# create daily returns
	for (i in 1:length(instruments)) {
		instrument = instruments[[i]]
		instrument@history = compute_period_returns(instrument@history, 1)
	
		cat("Finished computing returns for", instrument@symbol, "\n")
		
		instrument@history = compute_volatility(instrument@history, 10)
		
		cat("Finished computing volatility for", instrument@symbol,  "\n")
		
		instruments[[i]] = instrument
		
		# Write output to file
		out_file = paste(result_dir, instrument@symbol, ".csv", sep ="")
		write.csv(instrument@history, out_file)
	}
	
	# Associate a randomly generated time series with the instruments' historical
	# time series
#	for (i in 1:length(instruments)) {
			
#		instrument = instruments[[i]]
		
#		history = instrument@history
		
#		my_signal = sample(1:100, size = nrow(history), replace=TRUE)
#		my_signal = timeSeries(my_signal, charvec = rownames(history))
#		instruments[[i]]@history = cbind(history, my_signal)
#	}
	
	return(instruments)
}

# Create a single instrument
create_instrument <- function(sym, hist) 
{
#	instrument = new("instrument", symbol = sym, history = hist) 	
	instrument = new("instrument", symbol = sym)
	
	return(instrument)
}

# Make sure that series is according to our specifications (so far,
# only dates are being using for specification
verify_series <- function(symbol, series, from_date, to_date)
{
	series_start = as.Date(rownames(series)[1])
	series_end = as.Date(rownames(series)[nrow(series)])

	if (series_start != from_date) {
		if (series_start > from_date) {
			missing_data = as.timeSeries(yahooSeries(symbol, from = from_date, to = series_start - 1))
			series = rbind(series, missing_data)
		}
		else if (series_start < from_date) {
			start_idx = which(rownames(series) == as.character(from_date))
			series = series[start_idx:nrow(series)]
		}
	}
	
	if (series_end != to_date) {
		if (series_end < to_date) {
			missing_data = as.timeSeries(yahooSeries(symbol, from = series_end + 1, to = to_date))	
			series = rbind(series, missing_data)
		}
		else if (series_end > to_date) {
			end_idx = which(rownames(series) == as.character(to_date))
			series = series[1:end_idx]
		}
	}
	
	return (series)
}

# Creates a list of instruments. If data not locally available, then downloads
# from yahooFinance
setup_instruments <- function(data_dir, symbol_file, from_date, to_date)
{
	# Read symbols from symbol file
	symbol_file = paste(data_dir, symbol_file, sep = "")
	
	# Vector of all symbols
	symbols = scan(symbol_file, what = "character")

	# all instruments to be returned with this
	instruments = list(NULL)
	
	for(i in 1:length(symbols)) {
		symbol = symbols[i]
        data_file_name = paste(data_dir, symbol, ".csv", sep = "")
		cat("Processing", data_file_name, "\n")
		
        if (!file.exists(data_file_name)) {
			cat("Missing", data_file_name, "\n")
			next
			series_table = yahooSeries(symbol, from = from_date, to = to_date)
            series_ts = as.timeSeries(series_table)
            colnames(series_ts)[1] = "Open"
            colnames(series_ts)[2] = "High"
            colnames(series_ts)[3] = "Low"
            colnames(series_ts)[4] = "Close"
            colnames(series_ts)[5] = "Volume"
            write.csv(series_ts, file = data_file_name)
        }
        else {
#			series_table = read.csv(data_file_name, header = TRUE, sep = ",", row.names = 1)
#			ft = format(rownames(series_table), format = "%y-%m-%d %H:%M:%S")
#			rownames(series_table) = ft
#			series_ts = timeSeries(series_table, charvec = rownames(series_table))
        }

#		series_ts = verify_series(symbol, series_ts, from_date, to_date)
		
		instruments[i] = create_instrument(symbol, series_ts)		
	}
	
	return (instruments)
}