# NCf [<img src="man/paper_alt_hex.png" align="right" width=160 height=160 alt=""/>](https://makaho.sk8.inrae.fr/)

<!-- badges: start -->
[![R-CMD-check](https://github.com/super-lou/NCf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/super-lou/NCf/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-green)](https://lifecycle.r-lib.org/articles/stages.html)
![](https://img.shields.io/github/last-commit/super-lou/NCf)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md) 
<!-- badges: end -->

**NCf** is a R package which provide an efficient and simple way to create a NetCDF file only with R variable declarations.

This project was carried out for National Research Institute for Agriculture, Food and the Environment (Institut National de Recherche pour l’Agriculture, l’Alimentation et l’Environnement, [INRAE](https://agriculture.gouv.fr/inrae-linstitut-national-de-recherche-pour-lagriculture-lalimentation-et-lenvironnement) in french).


## Installation
For latest development version
``` r
remotes::install_github('super-lou/NCf')
```


## Documentation

### General Principle
The idea is to start with an empty environement variable created by [initialise_NCf()] and to store variable in this environnement variable. For example, the default environemment variable is named `NCf` so the variable `human` that take the character `"George"` can be add to `NCf` with `NCf$human = "George"`. In fact this variable will be un-used in the final NetCDF file because it does not indicate to which NetCDF variable or dimension the `human` info refers to. 

Thus, all  R variables that will be processed by the [generate_NCf()] function all have the same name format `NCf$xxxx.yyyy` or `NCf$xxxx.00.yyyy`. The latter format corresponds to a character string followed by a dollar, then an alphanumeric character string, followed by a period ".", then a two-digit number, followed by a new period "." and a new alphanumeric character string. That way, the first string `NCf` is the storage environment for variables specific to the current NetCDF build (... basically it's the NetCDF file ID) and it's the dollar to access it. The second string `xxxx` is the variable (or dimension) that is being discussed in the NetCDF and the third and last string `yyyy` is the attribute (or associated parameters) that we want to fill in. Finally, the number `00` is optional and is simply present to manage the order of appearance of the attributes in the final NetCDF.

Thus, adding a variable (or dimension) and modifying these attributes in the final NetCDF is simply a matter of modifying the name of an R variable and its associated value.

### Advanced principle

For a better understanding of how the code works it is important to specify some rules for editing R NCf variables.

* **NetCDF dimension** :
Defining a NetCDF dimension is done by creating an R variable `NCf$x.name = "a"`. Here `a` is the name displayed in the NetCDF of the dimension and `x` is the internal R code identification of this variable. It is also necessary to define an R variable `NCf$x.value = Value` where this time `Value` is for example a data vector which defines the values associated with the dimension identified in R by `x` and named in the NetCDF `a`. For the sake of clarity, it is often best to define a dimension named in the NetCDF in the same way as its internal identification in R such as `NCf$y.name = "y"`.

* **NetCDF variable** :
Defining a NetCDF variable is done in the same way as for a dimension via an R variable `NCf$var.name = "b"`. However, this time it is not necessary to associate another R variable of type `NCf$var.value = Value` but an R variable of type `NCf$var.dimension = "x"`. This last one allows to make the link between the variable `b` and the dimension `x`. If the dimension entered is "", no dimension will be associated with this variable but this declaration in R is still necessary.

* **Variable precision** :
For a variable (and more rarely for a dimension), it is possible to give the "precision" or rather the type of data associated with it. To do this, you have to define an R variable `NCf$x.precision = "type"` where `"type"` is chosen among 'short', 'integer', 'float', 'double', 'char' and 'byte'. WARNING ... if a character variable (thus of type 'char') is filled in and takes as input a dimension characterizing the length of this character string, it is imperative that the associated dimension takes as value a vector of 1 to the desired length of the character string and that this same dimension presents a parameter `NCf$dim.is_nchar_dimension = TRUE` which specifies this special behavior.

* **Variable attribute** :
Any other attribute of a NetCDF variable or dimension is defined by an R variable `NCf$var.00.att = "attribute"` where in general `"attribute"` is a string and preferably a two-digit number (in this case `00`) is used to specify its position in the NetCDF.

* **Global attribute** :
A NetCDF attribute defined as `NCf$global.00.att = "attribute"` specifies with the `global` tag that this attribute is global in the NetCDF.

* **NetCDF file title** :
A NetCDF attribute defined as `NCf$title.00.att = "attribute"` specifies with the `title` marker that this attribute allows the construction of the NetCDF file title by joining with `"_"` the set of non-empty supplied attributes.

### Framework
In that way, NetCDF file creation is more focused on the info and formatting than on the raw code of NetCDF creation. It is easy to manage multiple NetCDF file at the same time with different environment variables and to have separate scripts that contain NCf R variables definitions between R code line of data processing. 


## FAQ
*I have a question.*

-   **Solution**: Search existing issue list and if no one has a similar question create a new issue.

*I found a bug.*

-   **Good Solution**: Search existing issue list and if no one has reported it create a new issue.
-   **Better Solution**: Along with issue submission provide a minimal reproducible example of the bug.
-   **Best Solution**: Fix the issue and submit a pull request. This is the fastest way to get a bug fixed.


## Code of Conduct
Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.