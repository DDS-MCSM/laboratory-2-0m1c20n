#******************************************************************************#
#                                                                              #
#                          Lab 2 - CPE Standard                                #
#                                                                              #
#              Oriol Mominó Villegas - Data Driven Securty                     #
#                                                                              #
#******************************************************************************#
DownloadCPEData <- function() {
  compressed_cpes_url <- "https://nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml.zip"
  cpes_filename <- "cpes.zip"
  if(!file.exists(cpes_filename)){
    download.file(compressed_cpes_url, cpes_filename)
  }
  unziped_file <- "official-cpe-dictionary_v2.3.xml"
  if(!file.exists(unziped_file)){
    unzip(zipfile = cpes_filename)
  }
  cpe.file <- "./official-cpe-dictionary_v2.3.xml"
  return(cpe.file)
}

ReadXMLCPEFile <- function(cpe.file) {
  cpes.xml <- xml2::read_xml(cpe.file)
  return(cpes.xml)
}

GetCPEItems <- function(cpes.xml) {
  cpes <- data.frame(title = xml2::xml_text(xml2::xml_find_all(cpes.xml, "//*[cpe-23:cpe23-item]/*[@xml:lang='en-US'][1]")),
                     cpe.23 = xml2::xml_text(xml2::xml_find_all(cpes.xml, "//*[cpe-23:cpe23-item]/*/@name")),
                     stringsAsFactors = F)

  return(cpes)
}

CleanCPEs <- function(cpes){
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

  return(cpes)
}
