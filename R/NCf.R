# Copyright 2022-2023 Louis Héraut (louis.heraut@inrae.fr)*1,
#                     Éric Sauquet (eric.sauquet@inrae.fr)*1,
#                     Jean-Philippe Vidal (jean-philippe.vidal@inrae.fr)*1
#                     
# *1   INRAE, France
#
# This file is part of NCf R package.
#
# NCf R package is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# NCf R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with NCf R package.
# If not, see <https://www.gnu.org/licenses/>.


#  _  _   ___   __ 
# | \| | / __| / _|
# | .` || (__ |  _|
# |_|\_| \___||_|   __________________________________________________
# Fast and simple way to create a NetCDF
# file only with variable declarations


#' @title initialise_NCf
#' @description Initialises the NCf way of NetCDF creation. Formally an environment variable is created and it will store all the different data and info that will later be gather as a NetCDF file.
#' @param environment_name *character, default="NCf"* Name of the R environment used to store NCf variables.
#' @return An environnement variable named `environment_name`.
#' @examples
#' ```
#' # by default
#' initialise_NCf()
#' # or with a personal name
#' initialise_NCf(environment_name="NCf2")
#' ```
#' @export
initialise_NCf = function (environment_name="NCf") {
    # Creation of the environment 
    assign(environment_name, new.env(), envir=.GlobalEnv)
}


extract_att_name = function (obj_name, lsNCf, notAtt="") {
    att_name = gsub("^[.]", "",
                    stringr::str_extract(lsNCf[grepl(paste0(obj_name,
                                                            "[.].*"),
                                            lsNCf)],
                                "([.][:digit:]+[.].*$)|([.].*$)"))
    att_name = att_name[!(att_name %in% notAtt)]
    if (identical(att_name, character(0))) {
        att_name = NULL
    }
    return (att_name)
}


#' @title generate_NCf
#' @description
#' It creates a NetCDF file according to the defined variables in the environemment variable previously created with the [initialise_NCf()] function. Each variable name needs to follow a precise pattern in order to be understandable by this function.
#' @details
#' # General Principle
#' ---
#' The idea is to start with an empty environement variable created by [initialise_NCf()] and to store variable in this environnement variable. For example, the default environemment variable is named `NCf` so the variable `human` that take the character `"George"` can be add to `NCf` with `NCf$human = "George"`. In fact this variable will be un-used in the final NetCDF file because it does not indicate to which NetCDF variable or dimension the `human` info refers to. 
#'
#' Thus, all  R variables that will be processed by the [generate_NCf()] function all have the same name format `NCf$xxxx.yyyy` or `NCf$xxxx.00.yyyy`. The latter format corresponds to a character string followed by a dollar, then an alphanumeric character string, followed by a period ".", then a two-digit number, followed by a new period "." and a new alphanumeric character string. That way, the first string `NCf` is the storage environment for variables specific to the current NetCDF build (... basically it's the NetCDF file ID) and it's the dollar to access it. The second string `xxxx` is the variable (or dimension) that is being discussed in the NetCDF and the third and last string `yyyy` is the attribute (or associated parameters) that we want to fill in. Finally, the number `00` is optional and is simply present to manage the order of appearance of the attributes in the final NetCDF.
#'
#' Thus, adding a variable (or dimension) and modifying these attributes in the final NetCDF is simply a matter of modifying the name of an R variable and its associated value.
#'
#' # Advanced principle
#' ---
#' For a better understanding of how the code works it is important to specify some rules for editing R NCf variables.
#'
#' ## NetCDF dimension
#' Defining a NetCDF dimension is done by creating an R variable `NCf$x.name = "a"`. Here `a` is the name displayed in the NetCDF of the dimension and `x` is the internal R code identification of this variable. It is also necessary to define an R variable `NCf$x.value = Value` where this time `Value` is for example a data vector which defines the values associated with the dimension identified in R by `x` and named in the NetCDF `a`. For the sake of clarity, it is often best to define a dimension named in the NetCDF in the same way as its internal identification in R such as `NCf$y.name = "y"`.
#'
#' ## NetCDF variable
#' Defining a NetCDF variable is done in the same way as for a dimension via an R variable `NCf$var.name = "b"`. However, this time it is not necessary to associate another R variable of type `NCf$var.value = Value` but an R variable of type `NCf$var.dimension = "x"`. This last one allows to make the link between the variable `b` and the dimension `x`. If the dimension entered is "", no dimension will be associated with this variable but this declaration in R is still necessary.
#'
#' ## Variable precision
#' For a variable (and more rarely for a dimension), it is possible to give the "precision" or rather the type of data associated with it. To do this, you have to define an R variable `NCf$x.precision = "type"` where `"type"` is chosen among 'short', 'integer', 'float', 'double', 'char' and 'byte'. WARNING ... if a character variable (thus of type 'char') is filled in and takes as input a dimension characterizing the length of this character string, it is imperative that the associated dimension takes as value a vector of 1 to the desired length of the character string and that this same dimension presents a parameter `NCf$dim.is_nchar_dimension = TRUE` which specifies this special behavior.
#'
#' ## Variable attribute
#' Any other attribute of a NetCDF variable or dimension is defined by an R variable `NCf$var.00.att = "attribute"` where in general `"attribute"` is a string and preferably a two-digit number (in this case `00`) is used to specify its position in the NetCDF.
#'
#' ## Global attribute
#' A NetCDF attribute defined as `NCf$global.00.att = "attribute"` specifies with the `global` tag that this attribute is global in the NetCDF.
#'
#' ## NetCDF file title
#' A NetCDF attribute defined as `NCf$title.00.att = "attribute"` specifies with the `title` marker that this attribute allows the construction of the NetCDF file title by joining with `"_"` the set of non-empty supplied attributes.
#'
#' # Framework
#' ---
#' In that way, NetCDF file creation is more focused on the info and formatting than on the raw code of NetCDF creation. It is easy to manage multiple NetCDF file at the same time with different environment variables and to have separate scripts that contain NCf R variables definitions between R code line of data processing. 
#' @param out_dir *character, default=""* Name of the output directory for the NetCDF file.
#' @param environment_name *character, default="NCf"* Name of the R environment used to store NCf variables.
#' @param overwrite *logical, default=TRUE* If a NetCDF file already exists in the `out_dir` directory, will it be overwrite by the new generated one ?
#' @param chunksizes_list *named integer vector, default=c("time"=365)* Defines the `chunksizes` value of the [ncdf4::ncvar_def()] function for a defined variable explained as the name of this current vector value. For example, by default with `c("time"=365)` the dimension `time` will have a `chunksizes` of 365.
#' @param unlim_list *character vector, default=c("time")* Defines as `TRUE` the `unlim` variable of the [ncdf4::ncdim_def()] function for set of dimensions named listed in this `unlim_list` variables.
#' @param verbose *logical, default=FALSE* Prints basic info about the current execution in order to debug possible format issues.
#' @return NetCDF file in the `out_dir` directory.
#' @seealso [GitHub developpement page](https://github.com/super-lou/NCf) and [an advanced use of the NCf package](https://github.com/super-lou/Ex2D_toolbox/tree/main/help/DRIAS_export/DRIAS_export_1D)
#' @examples
#' ```
#' ## 0. LIBRARY ________________________________________________________
#' if (!require (remotes)) install.packages("remotes")
#' if (!require (NCf)) remotes::install_github("super-lou/NCf")
#'
#' 
#' ## 1. INITIALISATION _________________________________________________
#' initialise_NCf()
#'
#' 
#' ## 2. TITLE __________________________________________________________
#' NCf$title.01.title = paste0("MODEL_", Sys.Date())
#'
#' 
#' ## 3. GLOBAL ATTRIBUTS _______________________________________________
#' NCf$global.01.data_type = "diagnostic"
#' NCf$global.02.contact = "@"
#'
#' 
#' ## 4. DIMENSIONS _____________________________________________________
#' ### 4.1. Time ________________________________________________________
#' start = "2000-01-01"
#' end = "2000-01-31"
#' timezone = "UTC"
#' step = "days"
#' 
#' from = as.POSIXct(start, tz=timezone)
#' to = as.POSIXct(end, tz=timezone)
#' origin = as.POSIXct("1950-01-01", tz=timezone)
#' units = paste0(step, " since ", origin)
#' time = seq.POSIXt(from=from, to=to, by=step)
#' time = as.numeric(time - origin)
#' 
#' NCf$time.name = "time"
#' NCf$time.value = time
#' NCf$time.01.standard_name = "time"
#' NCf$time.02.units = units
#' 
#' ### 4.2. Station _____________________________________________________
#' NCf$station.name = "station"
#' NCf$station.value = 1:3
#' 
#' NCf$code.name = "code"
#' NCf$code.dimension = "station, code_strlen"
#' NCf$code.precision = "char"
#' NCf$code.value = c("AAAAAAAA", "BBBBBBBB", "CCCCCCCC")
#' NCf$code.01.standard_name = "code"
#' NCf$code_strlen.name = "code_strlen"
#' NCf$code_strlen.value = 1:max(nchar(NCf$code.value))
#' NCf$code_strlen.is_nchar_dimension = TRUE
#' 
#' 
#' ## 5. VARIABLES ______________________________________________________
#' ### 5.1. Flow ________________________________________________________
#' NCf$Q.name = "Q"
#' NCf$Q.dimension = "station, time"
#' NCf$Q.precision = "float"
#' NCf$Q.value = matrix(
#'     data=round(x=runif(length(NCf$time.value)*length(NCf$station.value)),
#'                digits=2),
#'     ncol=length(NCf$time.value))
#' NCf$Q.01.standard_name = "flow"
#' NCf$Q.02.units = "m3.s-1"
#' NCf$Q.03.missing_value = NaN
#' 
### 5.2. Surface ________________________________________________________
#' NCf$surface_model.name = "surface_model"
#' NCf$surface_model.dimension = "station"
#' NCf$surface_model.precision = "float"
#' NCf$surface_model.value = round(x=runif(length(NCf$station.value)),
#'                                 digits=2)
#' NCf$surface_model.01.standard_name = "surface in the model world"
#' NCf$surface_model.02.units = "km2"
#' NCf$surface_model.03.missing_value = NaN
#'
#' 
#' ## 6. SAVING _________________________________________________________
#' generate_NCf(out_dir="./")
#' ```
#' @export
generate_NCf = function (out_dir="",
                         environment_name="NCf",
                         overwrite=TRUE,
                         chunksizes_list=c("time"=365),
                         unlim_list=c("time"),
                         verbose=FALSE) {

    NCf = get(environment_name, envir=.GlobalEnv)
    lsNCf = ls(envir=NCf)
    
## 1. NETCDF FILENAME ________________________________________________
### 1.1. Getting title parts _________________________________________
    title_names = unlist(lapply("title",
                                extract_att_name, lsNCf=lsNCf))
    title_names = paste0("title", ".", title_names)
    title_parts = unlist(lapply(title_names, get, envir=NCf))

### 1.2. Title formatting ____________________________________________
    title_parts = title_parts[title_parts != ""]
    filename = paste0(title_parts, collapse="_")
    filename = paste0(filename, ".nc")
    filepath = file.path(out_dir, filename)

### 1.3. Overwite old file ___________________________________________
    if (overwrite) {        
        if (file.exists(filepath)) {
            file.remove(filepath)
        }
    }
    
## 2. INFORMATION GATHERING __________________________________________
### 2.1. Getting variable names and dimensions _______________________
    obj_names = stringr::str_extract(lsNCf[grepl("[.]name$", lsNCf)],
                                     "[^.]+")
    obj_names = c(obj_names, "global")

### 2.2. Getting attributes __________________________________________
    notAtt = c("name",
               "dimension", "is_nchar_dimension",
               "precision", "value")
    att_names = lapply(obj_names, extract_att_name,
                       lsNCf=lsNCf, notAtt=notAtt)
    names(att_names) = obj_names


## 3. DIMENSIONS CREATION ____________________________________________
### 3.1. Initialisation ______________________________________________
    dim_names = c()
    var_names = c()
    nObj = length(obj_names) 
    for (i in 1:nObj) {
        name = obj_names[i]
        if (name != "global") {

### 3.2. Finding dimension variables _________________________________
            obj_dim = paste0(name, ".dimension")
            if (!exists(obj_dim, envir=NCf)) {
                
                if (verbose) {
                    print(paste0("Creation of dimension ", name))
                }
                
                dim_name = paste0(name, "_dim")
                dim_value = get(paste0(name, ".value"), envir=NCf)

### 3.3. Checking if the dimension is a number of characters _________
                dim_is_nchar = paste0(name, ".is_nchar_dimension")
                if (exists(dim_is_nchar, envir=NCf)) { 
                    create_dimvar = FALSE
                } else {
                    create_dimvar = TRUE
                }
                
### 3.4. Creation ____________________________________________________
                assign(dim_name,
                       ncdf4::ncdim_def(name, units="",
                                        vals=dim_value,
                                        unlim=name %in% unlim_list,
                                        create_dimvar=create_dimvar),
                       envir=NCf)
                dim_names = c(dim_names, name)
                
            } else {
                var_names = c(var_names, name)
            }
        }
    }

    
## 4. VARIABLES CREATION _____________________________________________
    vars = list()
    nVar = length(var_names) 
    for (i in 1:nVar) {
        name = var_names[i]
        
        if (name != "global") {
            if (verbose) {
                print(paste0("Creation of variable ", name))
            }
            var_name = paste0(name, "_var")

### 4.1. Obtaining accuracy __________________________________________
            var_prec = paste0(name, ".precision")
            if (exists(var_prec, envir=NCf)) {
                prec = get(var_prec, envir=NCf)
            } else {
                prec = "float"
            }
            
### 4.2. Formatting dimensions _______________________________________
            var_dim = paste0(name, ".dimension")
            dimStr = get(var_dim, envir=NCf)
            dimStr = gsub(" ", "", dimStr)
            if (grepl(",", dimStr)) {
                dimStr = unlist(strsplit(dimStr, ","))
            }
            if (all(dimStr %in% dim_names)) {
                dimStr_is_nchar = paste0(dimStr, ".is_nchar_dimension")
                is_nchar = sapply(dimStr_is_nchar,
                                  exists, USE.NAMES=FALSE, envir=NCf)

                if (prec == "char" & !any(is_nchar)) {
                    stop (paste0('Which dimension of the ', name,
                                 " variable is a dimension that ",
                                 "indicate the number of character of ",
                                 "the variable ? Specify it by adding ",
                                 '"dim.is_nchar_dimension = TRUE" to ',
                                 "the corresponding dimension."))
                }
                
                if (any(is_nchar)) {
                    id_is_nchar = which(is_nchar)
                    dimStr = c(dimStr[id_is_nchar],
                               dimStr[-id_is_nchar])
                }
                
                dim = lapply(paste0(dimStr, "_dim"), get, envir=NCf)
                
            } else {
                dim = list()
            }

            if (!is.null(chunksizes_list)) {
                chunksizes = rep(1, length(dim))
                for (i in 1:length(chunksizes_list)) {
                    chunksizes[dimStr == names(chunksizes_list)[i]] =
                        chunksizes_list[i]
                }
            } else {
                chunksizes = NA
            }

            if (verbose) {
                print(paste0("Chunk size : ", paste0(chunksizes,
                                                     collapse=", ")))
            }
            
### 4.3. Creation ____________________________________________________
            assign(var_name,
                   ncdf4::ncvar_def(name, units="",
                                    prec=prec, dim=dim,
                                    chunksizes=chunksizes),
                   envir=NCf)
            vars = append(vars, list(get(var_name, envir=NCf)))
        }
    }


## 5. CREATION OF THE NETCDF FILE ____________________________________  
    NCdata = ncdf4::nc_create(filepath,
                              vars=vars, force_v4=TRUE)


## 6. ADDING VALUES AND ATTRIBUTES ___________________________________
    actual_dim_names = names(NCdata$dim)

    for (i in 1:nObj) {
        name = obj_names[i]

        if (verbose) {
            print(paste0("Adding information for ", name, " :"))
        }
        
        if (name == "global" | name %in% actual_dim_names | name %in% var_names) {
            
### 6.1. Adding values _______________________________________________
            obj_dim = paste0(name, ".dimension")
            if (exists(obj_dim, envir=NCf)) {
                var_value = paste0(name, ".value")
                if (exists(var_value, envir=NCf)) {

                    if (verbose) {
                        print("  value")
                    }
                    
                    value = get(var_value, envir=NCf)
                    ncdf4::ncvar_put(NCdata, name, value)
                }
            }

### 6.2. Adding attributes ___________________________________________
            obj_att_names = unlist(att_names[names(att_names) == name])
            obj_att_fullnames = paste0(name, ".", obj_att_names)
            names(obj_att_names) = NULL
            if (!is.null(obj_att_names)) {
                nAtt = length(obj_att_names)
                for (j in 1:nAtt) {
                    if (name == "global") {
                        name = 0
                    }

                    attname = gsub("^.*[.]", "", obj_att_names[j])
                    attval = get(obj_att_fullnames[j], envir=NCf)

                    if (verbose) {
                        print(paste0("  ", attname))
                    }
                    
                    if (attname %in% c("_FillValue", "missing_value")) {
                        ncdf4::ncvar_change_missval(NCdata,
                                                    name,
                                                    attval)
                    } else {
                        ncdf4::ncatt_put(NCdata, name, attname, attval)
                    }
                }
            }
        }
    }


## 7. CLOSING AND SAVING OF THE NETCDF FILE __________________________
    if (verbose) {
        print("Writing NetCDF")
    }
    print(NCdata)
    ncdf4::nc_close(NCdata)
    rm (list=ls(envir=NCf), envir=NCf)
}
