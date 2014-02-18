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

Once installed run the following command to load the package

    library(personalityfacets)

### Getting Started
Enter the following command to see an introduction to the main functions.

    ?personalityfacets

### Author
Jeromy Anglim

### Licence
License: GPL-2

### References
For further infomration about the approach, see the following reference:

Anglim, J., Grant, S. Estimating Incremental Criterion Prediction of Personality Facets over Factors. http://dx.doi.org/10.6084/m9.figshare.917189

If you use this package as part of a publication, please cite the above reference.

