`personalityfacets` is an R package which provides various functions for examining the relationship between personality facets, factors, and criteria.

### Installation:
As an R package, naturally you need to have [installed R](http://www.r-project.org/).

If they are not already installed, install the package dependencies:

    install.packages('boot')
    install.packages('bootstrap')
    install.packages('hypergeo')
    install.packages('devtools')

Then install the `personalityfacets` package:

    library(devtools)
    install_github('personalityfacets', username='jeromyanglim')

Once installed run the following command to load the package:

    library(personalityfacets)

### Getting Started
Enter the following command to see an introduction to the main functions:

    ?personalityfacets

### Author
Jeromy Anglim

### Licence
License: GPL-2

### References
For further infomration about the approach, the following reference is currently under review and will provide further details:

Anglim, J., & Grant, S. Incremental Criterion Prediction of Personality Facets over Factors: Obtaining Unbiased Estimates and Confidence Intervals. Manuscript under review.

If you use this package as part of a publication, please cite the above reference.