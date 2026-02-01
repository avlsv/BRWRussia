# server.R

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(plumber)

# 'app.R' is the location of the file containing your endpoint functions
r <- plumb("App.R")
# get port number from environment variable
port <- as.integer(Sys.getenv("PORT", unset = 8000))
r$run(port=port, host='0.0.0.0')
