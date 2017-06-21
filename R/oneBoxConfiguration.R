mrsOneBoxConfiguration <- function(context, 
                                   hostname) {
  assert_that(AzureSMR:::is_vm_name(vmName))
  
  status <- operateDSVM(context=context,
                        hostname=hostname, 
                        operation="Check")
  
  if (status != "Provisioning succeeded, VM running") {
    operateDSVM(context=context, 
                hostname=hostname, 
                operation="Start")
  } else {
    # do the one-box configuration.
    # TODO: check the OS to be Microsoft R Server.
  }
}