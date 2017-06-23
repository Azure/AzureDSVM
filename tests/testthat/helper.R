# helper functions - these are the same as used in AzureSMR package.

skip_if_missing_config <- function(f){
  if(!file.exists(f)) {
    msg <- paste("To run tests, add a file ~/.azuresmr/settings.json containing AzureML keys.",
                 "See ?workspace for help",
                 sep = "\n")
    message(msg)
    testthat::skip("settings.json file is missing")
  }
}

skip_if_offline <- function(){
  u <- tryCatch(url("https://mran.microsoft.com"),
                error = function(e)e)
  if(inherits(u, "error")){
    u <- url("http://mran.microsoft.com")
  }
  on.exit(close(u))
  z <- tryCatch(suppressWarnings(readLines(u, n = 1, warn = FALSE)),
                error = function(e)e)
  if(inherits(z, "error")){
    testthat::skip("Offline. Skipping test.")
  }
}

pubkey_gen <- function() {
  # pubkey key extraction.
  
  sys_info <- Sys.info()
  
  priv_key <- paste0(ifelse(sys_info["sysname"] == "Windows", 
                            "C:/Users/zhle/.ssh/",
                            "~/.ssh/"),
                     "id_rsa")
  
  file_exist <- file.exists(priv_key)
  
  if (file_exist) {
    dsvm_pubkey <- system(paste0("ssh-keygen -y -f ",
                                 priv_key),
                          intern=TRUE)
  } else {
    stop("Test aborted because no private key located at ~/.ssh. Please 
         generate key pair to do test again.")
  }
  
  return(dsvm_pubkey)
}