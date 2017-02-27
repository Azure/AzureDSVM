source("common.R")

# Connect to the Azure subscription and use this as the context for
# our activities.

context <- createAzureContext(tenantID=TID, clientID=CID, authKey=KEY)

# Check if the resource group already exists. Take note this script
# will not remove the resource group if it pre-existed.

rg_pre_exists <- existsRG(context, RG, LOC) %T>% print()

if (! rg_pre_exists)
{
  # Create a new resource group into which we create the VMs and
  # related resources. Resource group name is RG. 
  
  # Note that to create a new resource group one needs to add access
  # control of Active Directory application at subscription level.

  azureCreateResourceGroup(context, RG, LOC)

}

# Check that it now exists.

existsRG(context, RG, LOC)

# Create the required Linux DSVM - generally 4 minutes.

ldsvm <- deployDSVM(context, 
                    resource.group=RG,
                    location=LOC,
                    name=LDSVM,
                    username=USER,
                    size="Standard_DS1_v2",
                    os="Linux",
                    authen="Key",
                    pubkey=PUBKEY)
ldsvm

operateDSVM(context, RG, LDSVM, operation="Check")

azureListVM(context, RG)

# Send a simple system() command across to the new server to test its
# existence. Expect a single line with an indication of how long the
# server has been up and running.

cmd <- paste("ssh -q",
             "-o StrictHostKeyChecking=no",
             "-o UserKnownHostsFile=/dev/null",
             ldsvm, "uptime")
cmd
system(cmd, intern=TRUE)

