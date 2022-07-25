# build a Gene Ontology gson object

#' Build a gson object that annotate Gene Ontology
#'
#' @param data a two-column data.frame of original GO annotation. The columns are "gene_id" and "go_id".
#' @param ont type of GO annotation, which is "ALL", "BP", "MF", or "CC". default: "ALL".
#' @param species name of species. Default: NULL.
#' @param ... pass to `gson::gson()` constructor.
#'
#' @return a `gson` instance
#' @export
#'
#' @examples
#'  data = data.frame(gene_id = "gene1", 
#'                    go_id = c("GO:0035492", "GO:0009764", "GO:0031063", "GO:0033714", "GO:0036349"))
#'  go_mapper(data, species = "E. coli")
go_mapper = function(data, 
                     ont = c("ALL", "BP", "CC", "MF"), 
                     species = NULL, 
                     ...){
  ont = match.arg(ont)
  
  data = unique(data) # cleanup
  if (nrow(data) == 0) {
    simpleError("Data is empty in this call.")
  }
  
  # resources from `GO.db`
  goterms = AnnotationDbi::Ontology(GO.db::GOTERM)
  termname = AnnotationDbi::Term(GO.db::GOTERM)
  go.db_info = GO.db::GO_dbInfo()
  go.db_source_date = go.db_info[go.db_info$name == "GOSOURCEDATE", "value"]
  ancestor_map = lapply(c(GO.db::GOBPANCESTOR, 
                          GO.db::GOCCANCESTOR,
                          GO.db::GOMFANCESTOR),
                        as.list) %>%
    unlist(recursive = FALSE)
  
  # filter GO terms
  data[["ontology"]] = goterms[data[["go_id"]]]
  n_na_ont = sum(is.na(data[["ontology"]]))
  if ( n_na_ont > 0){
    warning(sprintf("%s GO term(s) are too new for current `GO.db` [source date: %s],\n  and are to be dropped. Consider to update `GO.db` if possible.",
                    n_na_ont, 
                    go.db_source_date))
  }
  
  # map to GO ancestor
  ancestor_list = ancestor_map[data$go_id]
  names(ancestor_list) = data$gene_id
  ancestor_go = AnnotationDbi::unlist2(ancestor_list)

  # gsid2gene
  gsid2gene = data.frame(
    gsid = c(ancestor_go, data$go_id),
    gene = c(names(ancestor_go), data$gene_id),
    ontology = goterms[c(ancestor_go, data$go_id)]
  ) %>%
    dplyr::filter(.data$gsid != "all") %>%
    unique()
  
  if (ont != "ALL"){
    gsid2gene = gsid2gene %>%
      dplyr::filter(.data$ontology == ont)
  }
  
  # gsid2name
  uniq_gsid = unique(gsid2gene$gsid) %>% as.character()
  gsid2name = data.frame(
    gsid = uniq_gsid,
    name = termname[uniq_gsid] %>% as.character()
  )
  
  # construct `gson` object
  gson(
    gsid2gene = gsid2gene,
    gsid2name = gsid2name,
    species = species,
    gsname = paste0("Gene Ontology: ", ont),
    version = sprintf("[GO.db source date: %s]", go.db_source_date),
    accessed_date = as.character(Sys.Date()),
    ...
  )
}
