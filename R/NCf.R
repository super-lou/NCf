# \\\
# Copyright 2022 Louis Héraut (louis.heraut@inrae.fr)*1
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
# NCf R package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with NCf R package.
# If not, see <https://www.gnu.org/licenses/>.
# ///


#  _  _   ___   __ 
# | \| | / __| / _|
# | .` || (__ |  _|
# |_|\_| \___||_|   __________________________________________________
# Fast and simple way to create a NetCDF
# file only with variable declarations


#' @title initialise_NCf
#' @description Initialises the NCf way of NetCDF creation
#' @param environment_name Name of the R environment used to store
#'                         NCf variables
#' @return Nothing
#' @examples
#' initialise_NCf()
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
#' @description Initialises the NCf way of NetCDF creation
#' @param out_dir Name of the output directory for the NetCDF file
#' @param environment_name Name of the R environment used to store
#'                         NCf variables
#' @return NetCDF file in the 'out_dir' directory
#' @examples
#' generate_NCf()
#' @export
generate_NCf = function (out_dir="", environment_name="NCf") {

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

                
                dimStr = paste0(dimStr, "_dim")
                dim = lapply(dimStr, get, envir=NCf)
            } else {
                dim = list()
            }

### 4.3. Creation ____________________________________________________
            assign(var_name,
                   ncdf4::ncvar_def(name, units="",
                                    prec=prec, dim=dim),
                   envir=NCf)
            vars = append(vars, list(get(var_name, envir=NCf)))
        }
    }


## 5. CREATION OF THE NETCDF FILE ____________________________________
    NCdata = ncdf4::nc_create(file.path(out_dir, filename),
                              vars=vars, force_v4=TRUE)


## 6. ADDING VALUES AND ATTRIBUTES ___________________________________
    actual_dim_names = names(NCdata$dim)

    for (i in 1:nObj) {
        name = obj_names[i]

        if (name == "global" | name %in% actual_dim_names | name %in% var_names) {
### 6.1. Adding values _______________________________________________
            obj_dim = paste0(name, ".dimension")
            if (exists(obj_dim, envir=NCf)) {
                var_value = paste0(name, ".value")
                if (exists(var_value, envir=NCf)) {
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
                    ncdf4::ncatt_put(NCdata,
                                     name, gsub("^.*[.]", "",
                                                obj_att_names[j]),
                                     get(obj_att_fullnames[j],
                                         envir=NCf))
                }
            }
        }
    }


## 7. CLOSING AND SAVING OF THE NETCDF FILE __________________________
    print(NCdata)
    ncdf4::nc_close(NCdata)
    rm (list=ls(envir=NCf), envir=NCf)
}
