#' Build KEGG annotation for novel species using KEGG Mapper
#' KEGG Mapper service can annotate protein sequences for novel species with KO database, 
#' and KO annotation need to be converted into Pathway or Module annotation, which
#' can then be used in `clusterProfiler`
#' 
#' @export
#' @param file the name of the file which comes from the KEGG Mapper service
#' @param format string indicate format of KEGG Mapper result
#' @param type string indicate annotation database
#' @param ... pass to gson::gson()
#' @examples 
#' 
kegg_mapper = function(file, 
                       format = c("BLAST","Ghost","Kofam"), 
                       type = c("pathway","module"),
                       ...){
  format = match.arg()
  if (format %in% c("BLAST", "Ghost")){
    protein2ko = read.delim(file, header = FALSE, col.names = c("id","ko")) %>% 
      dplyr::filter(ko != "")
  } else if (format == "Kofam"){
    protein2ko = read.delim(file, comment.char = "#", header = FALSE) %>% 
      tidyr::separate(1, into = c("star", 'id', 'ko'), sep = " +", extra = "drop") %>% 
      dplyr::select(-1) %>%
      tidyr::drop_na()
  } else {
    simpleError("Please specify a valid file format for your KEGG Mapper result.")
  }
  
  if (type == "pathway"){
    all_pathway = KEGGREST::keggList("pathway")
    all_pathway = stack(all_pathway)
    colnames(all_pathway) = c("pathway_name","pathway")
    all_pathway = all_pathway %>%
      dplyr::mutate_at(.vars = "pathway", .funs = "remove_db_prefix")
    
    ko2pathway = KEGGREST::keggLink("pathway", "ko")
    ko2pathway = stack(ko2pathway)
    colnames(ko2pathway) = c("pathway","ko")
    ko2pathway = ko2pathway %>%
      dplyr::mutate_all(.funs = "remove_db_prefix")
    gsid2gene = protein2ko %>%
      dplyr::left_join(ko2pathway, by = "ko") %>%
      dplyr::select(c("pathway","id")) %>%
      dplyr::arrange(pathway)
    
    gsid2name = gsid2gene %>%
      dplyr::distinct(pathway) %>%
      dplyr::left_join(all_pathway, by = "pathway")
  } else if (type == "module"){
    
  } else {
    
  }
}

remove_db_prefix = function(x){
  gsub("^[a-z]+:", "", x)
}