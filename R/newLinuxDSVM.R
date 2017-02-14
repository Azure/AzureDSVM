#' @title Create new Linux Data Science Virtual Machine.
#' @param context Authentication context of AzureSMR.
#' @param resource.group The Azure resource group where the DSVM is created.
#' @param location Location of the DSVM.
#' @param vmname Name of the DSVM. Note in the name lowercase characters or numbers are allowed, while any special characters are not. Incorrect naming may lead to unsuccessful deployment of DSVM - normally it returns a 400 error from REST call.
#' @param vmusername User name of the DSVM. It should be different from `vmname`.
#' @param vmsize Size of the DSVM. The default is "Standard_D3_v2". All available sizes can be obtained by function `getVMSizes`.
#' @param vmauthen Either "Key" or "Pass", meaning public-key based or password based authentication, respectively.
#' @param pubkey Public key for the DSVM. Only applicable for public-key based authentication.
#' @param mode Mode of virtual machine deployment. Default is "Sync".
newLinuxDSVM <- function(context,
                         resource.group,
                         location,
                         vmname,
                         vmusername,
                         vmsize="Standard_D3_v2",
                         vmauthen="Key",
                         pubkey,
                         mode="Sync")
{
  if(missing(context)) stop("Please specify a context.")
  if(missing(resource.group)) stop("Please specify a resouce group.")
  if(missing(location)) stop("Please specify a location.")
  if(missing(vmname)) stop("Please specify a virtual machine name.")
  if(!(vmsize %in% getVMSizes()$Sizes)) stop("Please use an allowed size for DSVM. More info can be found by running getVMSizes().")
  if(missing(vmauthen)) stop("Please specify authentication method, 'Key' for public key based and 'Pass' for password based.")

  # Specify the JSON for the parameters and template of a Linux Data
  # Science Virtual Machine.

  param <- '
"parameters": {
   "virtualMachines_newdsvm_adminPassword": {
        "value": "admin_pwd"
    },
    "virtualMachines_newdsvm_name": {
        "value": "<DEFAULT>"
    },
    "networkInterfaces_newdsvm_name": {
        "value": "<DEFAULT>nic"
    },
    "networkSecurityGroups_newdsvm_nsg_name": {
        "value": "<DEFAULT>sg"
    },
    "publicIPAddresses_newdsvm_ip_name": {
        "value": "<DEFAULT>ip"
    },
    "virtualNetworks_dsvm_vnet_name": {
        "value": "<DEFAULT>vnet"
    },
    "storageAccounts_dsvmdisks_name": {
        "value": "<DEFAULT>sa"
    }
}
'

  templ <- pastes0(
'
{
    "$schema": "https://schema.management.azure.com/schemas/2015-01-01/deploymentTemplate.json#",
    "contentVersion": "1.0.0.0",
    "variables": {},
    "parameters": {
        "virtualMachines_newdsvm_adminPassword": {
            "defaultValue": null,
            "type": "SecureString"
        },
        "virtualMachines_newdsvm_name": {
            "defaultValue": "newdsvm",
            "type": "String"
        },
        "networkInterfaces_newdsvm_name": {
            "defaultValue": "newdsvm",
            "type": "String"
        },
        "networkSecurityGroups_newdsvm_nsg_name": {
            "defaultValue": "newdsvm-nsg",
            "type": "String"
        },
        "publicIPAddresses_newdsvm_ip_name": {
            "defaultValue": "newdsvm-ip",
            "type": "String"
        },
        "virtualNetworks_dsvm_vnet_name": {
            "defaultValue": "dsvm-vnet",
            "type": "String"
        },
        "storageAccounts_dsvmdisks_name": {
            "defaultValue": "dsvmdisks",
            "type": "String"
        }
    },
    "resources": [
        {
            "type": "Microsoft.Compute/virtualMachines",
            "name": "[parameters(\'virtualMachines_newdsvm_name\')]",
            "apiVersion": "2015-06-15",
            "location": "[resourceGroup().location]",
            "plan": {
                "name": "linuxdsvm",
                "product": "linux-data-science-vm",
                "publisher": "microsoft-ads"
            },
            "properties": {
                "hardwareProfile": {
                    "vmSize": "<VMSIZE>"
                },
                "storageProfile": {
                    "imageReference": {
                        "publisher": "microsoft-ads",
                        "offer": "linux-data-science-vm",
                        "sku": "linuxdsvm",
                        "version": "latest"
                    },
                    "osDisk": {
                        "name": "[parameters(\'virtualMachines_newdsvm_name\')]",
                        "createOption": "FromImage",
                        "vhd": {
                            "uri": "[concat(\'https\', \'://\', parameters(\'storageAccounts_dsvmdisks_name\'), \'.blob.core.windows.net\', concat(\'/vhds/\', parameters(\'virtualMachines_newdsvm_name\'),\'20168192442.vhd\'))]"
                        },
                        "caching": "ReadWrite"
                    },
                    "dataDisks": [
                        {
                            "lun": 0,
                            "name": "[concat(parameters(\'virtualMachines_newdsvm_name\'),\'-disk-1\')]",
                            "createOption": "FromImage",
                            "vhd": {
                                "uri": "[concat(\'https\', \'://\', parameters(\'storageAccounts_dsvmdisks_name\'), \'.blob.core.windows.net\', concat(\'/vhds/\', parameters(\'virtualMachines_newdsvm_name\'),\'-disk-1-20168192442.vhd\'))]"
                            },
                            "caching": "None"
                        }
                    ]
                },',
                ifelse(vmauthen=="Key",
                '"osProfile": {
                    "computerName": "[parameters(\'virtualMachines_newdsvm_name\')]",
                    "adminUsername": "<USERNAME>",
                    "linuxConfiguration": {
                        "disablePasswordAuthentication": true,
                        "ssh": {
                            "publicKeys": [
                                {
                                    "path": "<KEYPATH>",
                                    "keyData": "<PUBKEY>"
                                }
                            ]
                        }
                    },
                    "secrets": [],
                    "adminPassword": "[parameters(\'virtualMachines_newdsvm_adminPassword\')]"
                },',
                '"osProfile": {
                    "computerName": "[parameters(\'virtualMachines_newdsvm_name\')]",
                    "adminUsername": "<USERNAME>",
                    "adminPassword": "[parameters(\'virtualMachines_newdsvm_adminPassword\')]"
                },',
                '"networkProfile": {
                    "networkInterfaces": [
                        {
                            "id": "[resourceId(\'Microsoft.Network/networkInterfaces\', parameters(\'networkInterfaces_newdsvm_name\'))]"
                        }
                    ]
                }
            },
            "resources": [],
            "dependsOn": [
                "[resourceId(\'Microsoft.Storage/storageAccounts\', parameters(\'storageAccounts_dsvmdisks_name\'))]",
                "[resourceId(\'Microsoft.Network/networkInterfaces\', parameters(\'networkInterfaces_newdsvm_name\'))]"
            ]
        },
        {
            "type": "Microsoft.Network/networkInterfaces",
            "name": "[parameters(\'networkInterfaces_newdsvm_name\')]",
            "apiVersion": "2016-03-30",
            "location": "[resourceGroup().location]",
            "properties": {
                "ipConfigurations": [
                    {
                        "name": "ipconfig1",
                        "properties": {
                            "privateIPAddress": "10.0.0.4",
                            "privateIPAllocationMethod": "Dynamic",
                            "publicIPAddress": {
                                "id": "[resourceId(\'Microsoft.Network/publicIPAddresses\', parameters(\'publicIPAddresses_newdsvm_ip_name\'))]"
                            },
                            "subnet": {
                                "id": "[concat(resourceId(\'Microsoft.Network/virtualNetworks\', parameters(\'virtualNetworks_dsvm_vnet_name\')), \'/subnets/default\')]"
                            }
                        }
                    }
                ],
                "dnsSettings": {
                    "dnsServers": []
                },
                "enableIPForwarding": false,
                "networkSecurityGroup": {
                    "id": "[resourceId(\'Microsoft.Network/networkSecurityGroups\', parameters(\'networkSecurityGroups_newdsvm_nsg_name\'))]"
                }
            },
            "resources": [],
            "dependsOn": [
                "[resourceId(\'Microsoft.Network/publicIPAddresses\', parameters(\'publicIPAddresses_newdsvm_ip_name\'))]",
                "[resourceId(\'Microsoft.Network/virtualNetworks\', parameters(\'virtualNetworks_dsvm_vnet_name\'))]",
                "[resourceId(\'Microsoft.Network/networkSecurityGroups\', parameters(\'networkSecurityGroups_newdsvm_nsg_name\'))]"
            ]
        },
        {
            "type": "Microsoft.Network/networkSecurityGroups",
            "name": "[parameters(\'networkSecurityGroups_newdsvm_nsg_name\')]",
            "apiVersion": "2016-03-30",
            "location": "[resourceGroup().location]",
            "properties": {
                "securityRules": [
                    {
                        "name": "Jupyter",
                        "properties": {
                            "protocol": "TCP",
                            "sourcePortRange": "*",
                            "destinationPortRange": "9999",
                            "sourceAddressPrefix": "*",
                            "destinationAddressPrefix": "*",
                            "access": "Allow",
                            "priority": 1010,
                            "direction": "Inbound"
                        }
                    },
                    {
                        "name": "JupyterHub",
                        "properties": {
                            "protocol": "TCP",
                            "sourcePortRange": "*",
                            "destinationPortRange": "8000",
                            "sourceAddressPrefix": "*",
                            "destinationAddressPrefix": "*",
                            "access": "Allow",
                            "priority": 1020,
                            "direction": "Inbound"
                        }
                    },
                    {
                        "name": "RServer",
                        "properties": {
                            "protocol": "TCP",
                            "sourcePortRange": "*",
                            "destinationPortRange": "8080",
                            "sourceAddressPrefix": "*",
                            "destinationAddressPrefix": "*",
                            "access": "Allow",
                            "priority": 1030,
                            "direction": "Inbound"
                        }
                    },
                    {
                        "name": "default-allow-ssh",
                        "properties": {
                            "protocol": "TCP",
                            "sourcePortRange": "*",
                            "destinationPortRange": "22",
                            "sourceAddressPrefix": "*",
                            "destinationAddressPrefix": "*",
                            "access": "Allow",
                            "priority": 1040,
                            "direction": "Inbound"
                        }
                    },
                    {
                        "name": "parallel-node",
                        "properties": {
                            "protocol": "TCP",
                            "sourcePortRange": "*",
                            "destinationPortRange": "11000-11999",
                            "sourceAddressPrefix": "*",
                            "destinationAddressPrefix": "*",
                            "access": "Allow",
                            "priority": 1050,
                            "direction": "Inbound"
                        }
                    }
                ]
            },
            "resources": [],
            "dependsOn": []
        },
        {
            "type": "Microsoft.Network/publicIPAddresses",
            "name": "[parameters(\'publicIPAddresses_newdsvm_ip_name\')]",
            "apiVersion": "2016-03-30",
            "location": "[resourceGroup().location]",
            "properties": {
                "publicIPAllocationMethod": "Dynamic",
                "idleTimeoutInMinutes": 4,
                "dnsSettings": {
                    "domainNameLabel": "<DNS_LABEL>"
                }
            },
            "resources": [],
            "dependsOn": []
        },
        {
            "type": "Microsoft.Network/virtualNetworks",
            "name": "[parameters(\'virtualNetworks_dsvm_vnet_name\')]",
            "apiVersion": "2016-03-30",
            "location": "[resourceGroup().location]",
            "properties": {
                "addressSpace": {
                    "addressPrefixes": [
                        "10.0.0.0/16"
                    ]
                },
                "subnets": [
                    {
                        "name": "default",
                        "properties": {
                            "addressPrefix": "10.0.0.0/24"
                        }
                    }
                ]
            },
            "resources": [],
            "dependsOn": []
        },
        {
            "type": "Microsoft.Storage/storageAccounts",
            "sku": {
                "name": "Standard_LRS",
                "tier": "Standard"
            },
            "kind": "Storage",
            "name": "[parameters(\'storageAccounts_dsvmdisks_name\')]",
            "apiVersion": "2016-01-01",
            "location": "[resourceGroup().location]",
            "tags": {},
            "properties": {},
            "resources": [],
            "dependsOn": []
        }
    ]
}')
)
  # Record the location of the local authorised keys file on the new
  # Linux DSVM.

  KEYPATH <- paste0("/home/", vmusername, "/.ssh/authorized_keys")

  # Update the parameter JSON with the virtual machine name.

  param %<>%
    gsub("<DEFAULT>", vmname, .) %>%
    paste0(collapse="")

  # jsonlite::prettify(para_json)

  dname <- paste0(vmname, "_dpl")

  # Update the template JSON with the appropriate parameters.

  templ %<>%
    gsub("<DNS_LABEL>", vmname, .) %>%
    gsub("<USERNAME>", vmusername, .) %>%
    gsub("<VMSIZE>", vmsize, .) %>%
    gsub("<KEYPATH>", KEYPATH, .) %>%
    gsub("<PUBKEY", pubkey, .) %>%
    paste0(collapse="")

  # jsonlite::prettify(temp_json)

  AzureSMR::azureDeployTemplate(context,
                                deplname=dname,
                                templateJSON=templ,
                                paramJSON=param,
                                resourceGroup=resource.group,
                                mode=mode)

  fqdn <- paste0(vmname, ".", location, ".cloudapp.azure.com")

  if (tolower(mode) == "sync")
    attr(fqdn, "ip") <-
      system(paste("dig", fqdn, "+short"), intern=TRUE) # Get from the VM meta data?

  return(fqdn)
}
