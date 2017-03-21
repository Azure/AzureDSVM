# AzureDSVM

The AzureDSVM (Azure Data Science Virtual Machine) is an R Package for Data Scientists
working with the Azure compute platform as a complement to the
underlying AzureSMR for controlling Azure Data Science Virtual Machines.

To install the package from github:

  > devtools::install_github("Azure/AzureDSVM")

Help pages are also provided for all functions within the
package. With RStudio for example type AzureDSVM into search when the
package is loaded to see a list of functions/help pages or else

  > library(help=AzureDSVM)

Note: The package will work with any open source R Session or with
Microsoft R extensions.

To get started with this package, see the Vignettes:

* [Get started](https://github.com/Azure/AzureDSVM/blob/master/vignettes/00Introduction.Rmd)
* [Deployment of a single DSVM](https://github.com/Azure/AzureDSVM/blob/master/vignettes/10Deploy.Rmd)
* [Deployment of multiple DSVMs](https://github.com/Azure/AzureDSVM/blob/master/vignettes/20Multi.Rmd)
* [Do computation on a single DSVM or a cluster of DSVMs](https://github.com/Azure/AzureDSVM/blob/master/vignettes/30Compute.Rmd)
* [Monitor data consumption and expense spent on using DSVM(s)](https://github.com/Azure/AzureDSVM/blob/master/vignettes/40Cost.Rmd)
* [Putting all together - use case of kmeans clustering](https://github.com/Azure/AzureDSVM/blob/master/vignettes/60Kmeans.Rmd)
* [Putting all together - use case of binary classification](https://github.com/Azure/AzureDSVM/blob/master/vignettes/80ModelSelect.Rmd)

# Code of Conduct

This project has adopted the [Microsoft Open Source Code of
Conduct](https://opensource.microsoft.com/codeofconduct/).
For more information see the [Code of Conduct
FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or
contact [opencode@microsoft.com](mailto:opencode@microsoft.com)
with any additional questions or comments.
