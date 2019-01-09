#******************************************************************************#
#                                                                              #
#                          Lab 2 - CPE Standard                                #
#                                                                              #
#              Arnau Sangra Rocamora - Data Driven Securty                     #
#                                                                              #
#******************************************************************************#

if (!require("xml2")) install.packages("xml2")
if (!require("stringr")) install.packages("stringr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("dplyr")) install.packages("dplyr")
library(xml2)
library(stringr)
library(tidyr)
library(dplyr)

compressed_cpes_url <- "https://nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml.zip"
cpes_filename <- "cpes.zip"
download.file(compressed_cpes_url, cpes_filename)
unzip(zipfile = cpes_filename)
cpe.file <- "./official-cpe-dictionary_v2.3.xml"

GetCPEItems <- function(cpe.raw) {
  #cpe <- NewCPEItem()
  #cpe.raw <- xml2::xml_find_all(cpe.raw)

  # transform the list to data frame
  cpes <- data.frame(title = xml2::xml_text(xml2::xml_find_all(cpe.raw, "//*[cpe-23:cpe23-item]/*[@xml:lang='en-US'][1]")),
                     cpe.23 = xml2::xml_text(xml2::xml_find_all(cpe.raw, "//*[cpe-23:cpe23-item]/*/@name")),
                     stringsAsFactors = F)

  # return data frame
  return(cpes)
}

CleanCPEs <- function(cpes){

  # data manipulation
  col.names <- c("std", "std.v", "part", "vendor", "product",
                "version", "update", "edition", "language", "sw_edition",
                "target_sw", "target_hw", "other")

  cpes$cpe.23 <- stringr::str_replace_all(cpes$cpe.23, "\\\\:", ";")

  cpes <- tidyr::separate(data = cpes, col = cpe.23, into = col.names, sep = ":", remove = F)
  cpes <- dplyr::select(.data = cpes, -std, -std.v)

  cpes$vendor <- as.factor(cpes$vendor)
  cpes$product <- as.factor(cpes$product)
  cpes$language <- as.factor(cpes$language)
  cpes$sw_edition <- as.factor(cpes$sw_edition)
  cpes$target_sw <- as.factor(cpes$target_sw)
  cpes$target_hw <- as.factor(cpes$target_hw)

  #return(data.frame())
  return(cpes)
}

ParseCPEData <- function(cpe.file) {

  # load cpes as xml file
  cpes <- xml2::read_xml(cpe.file)

  # get CPEs
  cpes <- GetCPEItems(cpes)

  # transform, clean, arrange parsed cpes as data frame
  df <- CleanCPEs(cpes)

  # return data frame
  return(df)
}

#cpes <- ParseCPEData(cpe.file)
