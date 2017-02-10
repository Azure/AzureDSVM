newLinuxDSVM <- function(context,
                         resource.group,
                         location,
                         vmname,
                         vmusername,
                         pubkey,
                         mode="Sync")
{
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
    "networkInterfaces_newdsvm161_name": {
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
    "storageAccounts_dsvmdisks490_name": {
        "value": "<DEFAULT>sa"
    }
}
'

  templ <- '
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
        "networkInterfaces_newdsvm161_name": {
            "defaultValue": "newdsvm161",
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
        "storageAccounts_dsvmdisks490_name": {
            "defaultValue": "dsvmdisks490",
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
                    "vmSize": "Basic_A3"
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
                            "uri": "[concat(\'https\', \'://\', parameters(\'storageAccounts_dsvmdisks490_name\'), \'.blob.core.windows.net\', concat(\'/vhds/\', parameters(\'virtualMachines_newdsvm_name\'),\'20168192442.vhd\'))]"
                        },
                        "caching": "ReadWrite"
                    },
                    "dataDisks": [
                        {
                            "lun": 0,
                            "name": "[concat(parameters(\'virtualMachines_newdsvm_name\'),\'-disk-1\')]",
                            "createOption": "FromImage",
                            "vhd": {
                                "uri": "[concat(\'https\', \'://\', parameters(\'storageAccounts_dsvmdisks490_name\'), \'.blob.core.windows.net\', concat(\'/vhds/\', parameters(\'virtualMachines_newdsvm_name\'),\'-disk-1-20168192442.vhd\'))]"
                            },
                            "caching": "None"
                        }
                    ]
                },
                "osProfile": {
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
                },
                "networkProfile": {
                    "networkInterfaces": [
                        {
                            "id": "[resourceId(\'Microsoft.Network/networkInterfaces\', parameters(\'networkInterfaces_newdsvm161_name\'))]"
                        }
                    ]
                }
            },
            "resources": [],
            "dependsOn": [
                "[resourceId(\'Microsoft.Storage/storageAccounts\', parameters(\'storageAccounts_dsvmdisks490_name\'))]",
                "[resourceId(\'Microsoft.Network/networkInterfaces\', parameters(\'networkInterfaces_newdsvm161_name\'))]"
            ]
        },
        {
            "type": "Microsoft.Network/networkInterfaces",
            "name": "[parameters(\'networkInterfaces_newdsvm161_name\')]",
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
                        "name": "default-allow-ssh",
                        "properties": {
                            "protocol": "TCP",
                            "sourcePortRange": "*",
                            "destinationPortRange": "22",
                            "sourceAddressPrefix": "*",
                            "destinationAddressPrefix": "*",
                            "access": "Allow",
                            "priority": 1030,
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
                            "priority": 1040,
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
            "name": "[parameters(\'storageAccounts_dsvmdisks490_name\')]",
            "apiVersion": "2016-01-01",
            "location": "[resourceGroup().location]",
            "tags": {},
            "properties": {},
            "resources": [],
            "dependsOn": []
        }
    ]
}
'

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
    gsub("<KEYPATH>", KEYPATH, .) %>%
    gsub("<PUBKEY", pubkey, .) %>%
    paste0(collapse="")
  
  # jsonlite::prettify(temp_json)

  azureDeployTemplate(context,
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
