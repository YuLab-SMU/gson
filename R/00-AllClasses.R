setClassUnion("data.frame_OR_NULL", c("data.frame", "NULL"))
setClassUnion("character_OR_NULL", c("character", "NULL"))

##' Class "GSON"
##' This class represents gene set information.
##'
##'
##' @name GSON-class
##' @aliases GSON-class show,GSON-method
##'
##' @docType class
##' @slot gsid2gene data.frame with two columns of 'gsid' and 'gene'
##' @slot gsid2name data.frame with two columns of 'gsid' and 'name'
##' @slot gene2name data.frame with two columns of 'gene' and 'name'
##' @slot species species of the annotation
##' @slot gsname gene set name, e.g., GO, KEGG
##' @slot version version of the gene set
##' @slot accessed_date time to obtain the gene set data
##' @slot info extra information
##' @exportClass GSON
##' @author Guangchuang Yu \url{https://yulab-smu.top}
##' @keywords classes
setClass("GSON",
         representation=representation(
             gsid2gene = "data.frame",
             gsid2name = "data.frame_OR_NULL",
             gene2name = "data.frame_OR_NULL",
             species =  "character_OR_NULL",
             gsname = "character_OR_NULL",
             version = "character_OR_NULL",
             accessed_date =  "character_OR_NULL",
             info = "character_OR_NULL"
         ),
         prototype=prototype(
             gsid2name = NULL,
             gene2name = NULL,
             species =  NULL,
             gsname = NULL,
             version = NULL,
             accessed_date =  NULL,
             info = NULL
         )
         )

