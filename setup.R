# Globals
SOURCE_HOME = "~/R/g4073/"
DATA_DIRECTORY = "~/R/data/results/"
RESULT_DIRECTORY = "~/R/data/results/"
SYMBOL_FILE = "sp100_sym"
HISTORY_START = as.Date("2007-09-04")
HISTORY_END = as.Date("2008-09-12")
TRANSACTION_COST <<- 0.02
INITIAL_VALUE = 1e7
INITIAL_CASH = 1e7
INITIAL_LEVERAGE = 1
PORTFOLIO = NULL

set_environment <- function(stage)
{
	library(timeSeries)
	library(timeDate)
	
	if (stage == 4) {
		if ("package:fAssets" %in% search()) detach("package:fAssets", unload = TRUE)
		if ("package:fBasics" %in% search()) detach("package:fBasics", unload = TRUE)
		if ("package:fPortfolio" %in% search()) detach("package:fPortfolio", unload = TRUE)
		if ("package:timeSeries" %in% search()) detach("package:timeSeries", unload = TRUE)
		if ("package:timeDate" %in% search()) detach("package:timeDate", unload = TRUE)
			
		invisible(library(fSeries))
		invisible(library(fCalendar))
		invisible(library(fImport))
	}
	
	if (stage == 2) {
		if ("package:fSeries" %in% search()) detach("package:fSeries")
		if ("package:fCalendar" %in% search()) detach("package:fCalendar")
		if ("package:fImport" %in% search()) detach("package:fImport")
		
		invisible(library(fPortfolio))
	}
}

create_classes <- function()
{
	setClass("instrument", representation(symbol ="character", history ="timeSeries"))
	
#	setClass("mv_portfolio", representation(pfolio_object = "fPORTFOLIO",
#											update_frequency = "numeric",
#										 	last_update = "character",
#											last_weights = "array",
#											returns = "timeSeries",
#											returns_period = "numeric"
#											)								
#			) 
											
	setClass("portfolio", representation(type = "numeric",
										 
										 active_instruments = "list",
										 closing_prices = "timeSeries",
										 returns_series = "timeSeries",
										 vol_series = "timeSeries",
										 mcaps_series = "timeSeries",
										 forecast_series = "timeSeries",
										 volume_series = "timeSeries",

										 current_nshares = "array",
										 current_weights = "array",
										 current_shorts = "array",
										 current_short_wts = "array",
										 current_longs = "array",
										 current_long_wts = "array",
										 current_values = "array",
										 desired_weights = "array",
										 tc_weights = "array",
										 
										 start_date = "character",
										 current_date = "character",
										 current_value = "numeric",
										 current_cash = "numeric",
										 current_equity = "numeric",
										 current_lever = "numeric",
										 current_net_tc = "numeric",
										 max_equity_value = "numeric"
										 )
			)
										 
}

presetup <- function()
{
	# Source all relavent files
	source(paste(SOURCE_HOME, "instrument.R", sep = ""))
	source(paste(SOURCE_HOME, "portfolio.R", sep = ""))
	source(paste(SOURCE_HOME, "longshort.R", sep = ""))

	# Create our objects
	create_classes()
}

get_instruments <- function()
{
	set_environment(stage = 1)
	presetup()
	
	# Source all relavent files
	source(paste(SOURCE_HOME, "instrument.R", sep = ""))
	source(paste(SOURCE_HOME, "portfolio.R", sep = ""))
	
	# Create our objects
	create_classes()
	
	# Setup all available instruments
	instruments = setup_instruments(DATA_DIRECTORY, SYMBOL_FILE, HISTORY_START, HISTORY_END)
	
	# Perform all analysis (prediction etc) on the instruments
	#instruments = analyze_instruments(instruments, RESULT_DIRECTORY)
	
	return(instruments)
}

# Creates a portfolio
get_portfolio <- function(instruments)
{
	presetup()

	# Need to change environemt before rMetrics portfolio can be used
	set_environment(stage = 2)
	
	# Create the portfolio
    portfolio = create_portfolio(instruments, INITIAL_CASH)
	
	return(portfolio)
}

# Stocks with unsual volatility of return. Useful in find splits
get_splitters <- function(instruments)
{
	i = 1
	
	for (stock in instruments){
		
		n_rows = nrow(stock@history)
		
		max_vol = max(stock@history[2:n_rows, "Volatility"])
		
		if(max_vol > 2) {
			max_idx = which(stock@history[2:n_rows, "Volatility"] == max(stock@history[2:n_rows, "Volatility"]))
			cat("[", i, "]", stock@symbol, max_idx, "\n")
		}
		
		i = i + 1
	}
}

get_incompletes <- function(instruments)
{
	i = 1
	for (stock in instruments) {
		first_date = rownames(stock@history[1, ])
		if (as.Date(first_date) != "2007-09-04") {
			cat("[", i, "]", stock@symbol, "\n")
		}
		i = 1+1
	}
}

get_backtest <- function(portfolio, start, end)
{
	presetup()
	backtest(portfolio, start, end, 1)	
}

# Fast the simple backtest. Split up data
get_backtest_fast <- function(pfolio_slow, start, end, step = 50)
{
	presetup()
  
	# All the dates
	dates = rownames(pfolio_slow@returns_series)
  
	# Output preparation
	header = paste("LS_RATIO", "LONG_WT", "SHORT_WT", "CASH_WT", "EQUITY", "LEVER", "CASH", "TC", "VAL", sep = ",")
	output_file = paste(RESULTS_FILE, as.numeric(Sys.time()), sep = "_")
	write(header, output_file, ncolumns = 9)

	window_start = start
	window_end = window_start + step - 1

	pfolio_fast = create_portfolio_fast(pfolio_slow, pfolio_slow, window_start, window_end)

	# total steps
	steps = trunc((end - start)/step)
  
	for (i in 1:steps) {
		pfolio_fast_last = backtest(pfolio_fast, output_file) 
		
		window_start = window_end + 1
		window_end = window_start + step - 1
		pfolio_fast = create_portfolio_fast(pfolio_slow, pfolio_fast_last, window_start, window_end)  
	}
}

graphs <- function(table)
{
       # Create ROC curve
        pl_file = paste("/mnt/store/common/tmp2/pl_", as.numeric(Sys.time()), ".jpeg", sep = "")
        jpeg(pl_file)
        plot(table[, "VAL"] , main = "P&L curve 2 month (~3000 Trading cycles)", 
		     xlab = "time ->", ylab = "P&L")
        dev.off()
		
		# Create ROC curve
        pl_file = paste("/mnt/store/common/tmp2/tc_", as.numeric(Sys.time()), ".jpeg", sep = "")
        jpeg(pl_file)
        plot(table[, "TC"] , 
			main = "Transaction Costs: 2 month (~3000 Trading cycles)", 
			xlab = "time ->", ylab = "Transaction Cost")
        dev.off()	
		
		# Create ROC curve
        pl_file = paste("/mnt/store/common/tmp2/ls_", as.numeric(Sys.time()), ".jpeg", sep = "")
        jpeg(pl_file)
        plot(abs(table[, "LS_RATIO"]) , 
			main = "LS Ratio: 2 month (~3000 Trading cycles)", 
			xlab = "time ->", ylab = "Transaction Cost")
        dev.off()	
}

main <- function()
{
	instruments = get_instruments()
	
	portfolio = get_portfolio(instruments)
	
	#
	# 11/13 Above three steps finish without problems
	#
	
	backtest(portfolio, "2007-10-01", "2008-08-31", 1)
	return (portfolio)
}
