#' @title Distribute public keys across nodes.
#' 
#' @description The function distributes public keys across nodes so as to allow secure communication between each of the nodes. This is to enable doParallel backend to create socket-based cluster based on the nodes for parallel computation.
#' 
#' @param location Location of the DSVMs.
#' 
#' @param hostnames Host names of the DSVMs.
#' 
#' @param usernames User name used for the DSVMs. In the case of DSVM cluster, the user name is unique.
#' 
#' @param pubkeys Public keys used for the DSVMs.
#' 
#' @param dns.labels DNS labels for the DSVMs.
#' 
#' @note Note this function merely applies for Linux DSVM. In addition, it relies on SSH/SCP for file transfer so OpenSSH or other SSH tools are required if they do not exist by default in the system.
#' 
#' @return No returned value. 
keyDistribution <- function(location,
                            hostnames,
                            usernames,
                            pubkeys,
                            count,
                            dns.labels=hostnames)
{
  
  # Set working directory to where temp files are located.
  
  cwd <- getwd()
  setwd(tempdir())
  
  # Do key gen in each node.  Propagate pub keys of each node back
  # to local.  Put the pub keys in authorized_keys and distribute
  # onto nodes.
  
  fqdns <- paste(dns.labels, location, "cloudapp.azure.com", sep=".")
  
  auth_keys <- character(0)
  tmpkeys   <- tempfile(paste0("AzureDSVM_pubkey_"))
  
  file.create(tmpkeys)
  
  copy <- "scp"
  options <- "-q -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"
  
  for (i in 1:count)
  {
    
    # Add an option to switch off host key checking - for the purposes
    # of avoiding pop up. Also do not clog up the user's known_hosts
    # file with all the servers created.
    
    tmpkey <- tempfile(paste0("AzureDSVM_pubkey_", hostnames[i], "_"))
    
    file.create(tmpkey)
    
    # Generate key pairs in the VM
    
    cmd <- sprintf("ssh %s -l %s %s %s",
                   options, usernames[i], fqdns[i],
                   "'ssh-keygen -t rsa -N \"\" -f ~/.ssh/id_rsa'")
    system(cmd, intern=TRUE, ignore.stdout=FALSE, ignore.stderr=FALSE, wait=FALSE)
    
    # Copy the public key and append it to the local machine.
    
    cmd <- sprintf("%s %s %s@%s:.ssh/id_rsa.pub %s",
                   # copy, options, usernames[i], fqdns[i], tmpkey)
                   copy, options, usernames[i], fqdns[i], file.path(".", basename(tmpkey)))
    
    system(cmd)
    
    # Append the public keys into authorized_key.
    
    auth_keys <- paste0(auth_keys, readLines(tmpkey), "\n")
    
    writeLines(auth_keys, tmpkeys)
    
    # Clean up the temp pub key file
    
    file.remove(tmpkey)
  }
  
  # Create a config file. To avoid any prompt when nodes are
  # communicating with each other.
  
  tmpscript <- paste0("./AzureDSVM_script_", hostnames[i], "_")
  
  file.create(tmpscript)
  
  sh <- writeChar(paste0("cat .ssh/pub_keys >> .ssh/authorized_keys\n",
                         "echo Host *.", location,
                         ".cloudapp.azure.com >> ~/.ssh/config\n",
                         "echo StrictHostKeyChecking no >> .ssh/config\n",
                         "echo UserKnownHostsFile /dev/null >> .ssh/config\n",
                         "chmod 600 .ssh/config\n"),
                  con=tmpscript)
  
  # Distribute the public keys and config files to nodes.
  
  for (i in 1:count)
  {
    # Copy the pub_keys onto node.
    
    system(sprintf("%s %s %s %s@%s:.ssh/pub_keys",
                   # copy, options, tmpkeys, usernames[i], fqdns[i]))
                   copy, options, file.path(".", basename(tmpkeys)), usernames[i], fqdns[i]))
    
    # Copy the config onto node and run it.
    
    system(sprintf("%s %s %s %s@%s:.ssh/shell_script",
                   # copy, options, tmpscript, usernames[i], fqdns[i]))
                   copy, options, file.path(".", basename(tmpscript)), usernames[i], fqdns[i]))
    
    system(sprintf("ssh %s -l %s %s 'chmod +x .ssh/shell_script'",
                   options, usernames[i], fqdns[i]))
    
    system(sprintf("ssh %s -l %s %s '.ssh/shell_script'",
                   options, usernames[i], fqdns[i]))
  }
  
  # Clean up.
  
  file.remove(tmpkeys, tmpscript)
  
  # revert back to original working directory.

  setwd(cwd)
  
  data.frame(hostname = hostnames,
             username = usernames,
             fqdn     = fqdns,
             stringsAsFactors=FALSE)
}