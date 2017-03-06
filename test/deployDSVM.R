cat("vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n")

source("common.R")

# Connect to the Azure subscription and use this as the context for
# our activities.

context <- createAzureContext(tenantID=TID, clientID=CID, authKey=KEY)

# Check if the resource group already exists. Take note this script
# will not remove the resource group.

if (existsRG(context, RG, LOC))
{
  cat("Resource group pre-exists\n\n")
} else
{
  cat("Resource group does not exist.\n\n")
  
  # Create a new resource group into which we create the VMs and
  # related resources. Resource group name is RG. 
  
  # Note that to create a new resource group one needs to add access
  # control of Active Directory application at subscription level.

  azureCreateResourceGroup(context, RG, LOC)
}

# Check that it now exists.

{
  if (existsRG(context, RG, LOC))
    cat("\nResource group now exists.\n\n")
  else
    cat("\nResource group STILL does not exist.\n\n")
}

# Create the required Linux DSVM - generally 4 minutes.

ldsvm <- deployDSVM(context, 
                    resource.group=RG,
                    location=LOC,
                    hostname=HOST,
                    username=USER,
                    pubkey=PUBKEY)

cat("\nReported fully qualified domain name and IP.\n\n")
ldsvm

cat("\nCheck operational status.\n\n")
operateDSVM(context, RG, HOST, operation="Check")

cat("\nList of VMs under this resource group.\n\n")
azureListVM(context, RG) %>% select(-ID) %>% print()

# Send a simple system() command across to the new server to test its
# existence. Expect a single line with an indication of how long the
# server has been up and running.

cat("\nAttempt a remote secure shell connection.\n\n")
cmd <- paste("ssh -q",
             "-o StrictHostKeyChecking=no",
             "-o UserKnownHostsFile=/dev/null",
             ldsvm, "uptime")
cmd
system(cmd, intern=TRUE)

cat("\nList of test resource groups.\n\n")
context %>%
  azureListRG() %>%
  filter(grepl(name, pattern="^my_")) %>%
  select(name, location, resourceGroup)

cat("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")
