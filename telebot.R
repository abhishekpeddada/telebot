library(telegram.bot)
library(quantmod)
library(TTR)
library(dotenv)

# Define the stock symbols
stock_symbols <- c("HDFCAMC.NS", "APOLLOHOSP.NS", "OFSS.NS", "LUPIN.NS", "INDHOTEL.NS", "TORNTPOWER.NS", 
                   "HINDPETRO.NS", "MAXHEALTH.NS", "DIXON.NS", "TIINDIA.NS", "MARICO.NS", "GODREJPROP.NS", 
                   "AUROPHARMA.NS", "PERSISTENT.NS", "MUTHOOTFIN.NS", "PRESTIGE.NS", "POLICYBZR.NS", 
                   "KALYANKJIL.NS", "BHARTIHEXA.NS", "ALKEM.NS", "LINDEINDIA.NS", "OBEROIRLTY.NS", 
                   "PIIND.NS", "POWERINDIA.NS", "SCHAEFFLER.NS", "JSL.NS", "VOLTAS.NS", "ABBOTINDIA.NS", 
                   "MPHASIS.NS", "BSE.NS", "THERMAX.NS", "MOTILALOFS.NS", "SUNDARMFIN.NS", "PETRONET.NS", 
                   "LLOYDSME.NS", "FLUOROCHEM.NS", "AUBANK.NS", "PAGEIND.NS", "GLENMARK.NS", "PREMIERENE.NS", 
                   "KPITTECH.NS", "GET&D.NS", "COFORGE.NS", "FEDERALBNK.NS", "FORTIS.NS", "NAM-INDIA.NS", 
                   "PAYTM.NS", "APLAPOLLO.NS", "TATATECH.NS", "EXIDEIND.NS", "IPCALAB.NS", "APARINDS.NS", 
                   "BLUESTARCO.NS", "NATIONALUM.NS", "360ONE.NS", "MFSL.NS", "AJANTPHARM.NS", "DEEPAKNTR.NS", 
                   "OLAELEC.NS", "CHOLAHLDNG.NS", "GODFRYPHLP.NS", "FIRSTCRY.NS", "KAYNES.NS", "BASF.NS", 
                   "ABFRL.NS", "SYNGENE.NS", "CRISIL.NS", "TATAINVEST.NS", "GODIGIT.NS")

dotenv::load_dot_env()
# Define your bot token
bot_token <- Sys.getenv("BOT_TOKEN")
bot <- Bot(token = bot_token)

# Function to check for EMA crossover from previous day to current day
check_ema_crossover <- function(symbol) {
  tryCatch({
    # Fetch stock data
    stock_data <- getSymbols(symbol, src = "yahoo", from = Sys.Date() - 365, to = Sys.Date(), auto.assign = FALSE)
    
    # Ensure data is valid
    if (nrow(stock_data) < 50) stop(paste(symbol, ": n = 50 is outside valid range"))
    
    # Calculate EMAs
    ema_9 <- EMA(Cl(stock_data), n = 9)
    ema_50 <- EMA(Cl(stock_data), n = 50)
    ema_100 <- EMA(Cl(stock_data), n = 100)

    # Check that we have at least 2 days of EMA data
    if (length(ema_9) < 2 || length(ema_50) < 2 || length(ema_100) < 2) {
      stop(paste(symbol, ": Insufficient EMA data"))
    }

    # Get the values for the last two days (previous and current)
    prev_ema_9 <- ema_9[length(ema_9) - 1]
    prev_ema_50 <- ema_50[length(ema_50) - 1]
    prev_ema_100 <- ema_100[length(ema_100) - 1]
    
    current_ema_9 <- last(ema_9)
    current_ema_50 <- last(ema_50)
    current_ema_100 <- last(ema_100)

    # Condition to check if 9 EMA crosses above 50 and 100 EMA today, but was below them yesterday
    # Check if the 9 EMA crossed above 50 and 100 EMA
    crossed_above <- prev_ema_9 <= prev_ema_50 && prev_ema_9 <= prev_ema_100 &&
                     current_ema_9 > current_ema_50 && current_ema_9 > current_ema_100
    
    # Check if the 9 EMA crossed below 50 and 100 EMA
    crossed_below <- prev_ema_9 >= prev_ema_50 && prev_ema_9 >= prev_ema_100 &&
                     current_ema_9 < current_ema_50 && current_ema_9 < current_ema_100
   
    n_crossed_above_50 <- prev_ema_9 <= prev_ema_50 &&  current_ema_9 > current_ema_50
    n_crossed_below_50 <- prev_ema_9 >= prev_ema_50 && current_ema_9 < current_ema_50

    n_crossed_above_100 <- prev_ema_9 <= prev_ema_100 && current_ema_9 > current_ema_100
    n_crossed_below_100 <- prev_ema_9 >= prev_ema_100 && current_ema_9 < current_ema_100
current_ema_9 < current_ema_100    
    # Return appropriate message
    if (crossed_above) {
      return(paste(symbol, ": EMA 9 just crossed ABOVE EMA 50 and EMA 100\n"))
    } else if (crossed_below) {
      return(paste(symbol, ": EMA 9 just crossed BELOW EMA 50 and EMA 100\n"))
    } else if (n_crossed_above_50) {
      return(paste(symbol, ": EMA 9 just crossed ABOVE EMA 50\n"))
    } else if (n_crossed_below_50) {
      return(paste(symbol, ": EMA 9 just crossed BELOW EMA 50\n"))
    } else if (n_crossed_above_100) {
      return(paste(symbol, ": EMA 9 just crossed ABOVE EMA 100\n"))
    } else if (n_crossed_below_100) {
      return(paste(symbol, ": EMA 9 just crossed BELOW EMA 100\n"))
    }
  }, error = function(e) {
    return(paste("Error fetching data for", symbol, ":", e$message))
  })
}

# Telegram handler to send EMA status of all stocks
stock_handler <- function(bot, update) {
  result_messages <- c()
  
  for (symbol in stock_symbols) {
    result <- check_ema_crossover(symbol)
    result_messages <- c(result_messages, result)
  }
  
  # Check message length to avoid Telegram's message length restriction
  final_message <- paste(result_messages, collapse = "\n")
  if (nchar(final_message) > 4096) {
    final_message <- substr(final_message, 1, 4096)  # Trim to fit Telegram's max message length
  }
  
  bot$sendMessage(chat_id = update$message$chat_id, text = final_message)
}

# Set up bot command handler
start_handler <- CommandHandler("start", stock_handler)
updater <- Updater(token = bot_token)

# Add handler to dispatcher
updater$dispatcher$add_handler(start_handler)

# Start polling for messages
updater$start_polling()
updater$idle()
~
