#' Build KEGG annotation for novel species using KEGG Mapper
#' KEGG Mapper service can annotate protein sequences for novel species with KO database, 
#' and KO annotation need to be converted into Pathway or Module annotation, which
#' can then be used in `clusterProfiler`
#' 
#' @export
#' @return  a gson instance
#' @param file the name of the file which comes from the KEGG Mapper service
#' @param format string indicate format of KEGG Mapper result
#' @param type string indicate annotation database
#' @param species your species, NULL if ignored
#' @param ... pass to gson::gson()
#' @examples 
#'  file = system.file('extdata', "kegg_mapper_blast.txt", package='gson')
#'  kegg_mapper(file, format = "BLAST", type = "pathway")
kegg_mapper = function(file, 
                       format = c("BLAST","Ghost","Kofam"), 
                       type = c("pathway","module"),
                       species = NULL,
                       ...){
  message("Please be aware that this process need a active internet connection and may take a while to finish.")
  format = match.arg(format)
  type = match.arg(type)
  if (format %in% c("BLAST", "Ghost")){
    protein2ko = utils::read.delim(file, header = FALSE, col.names = c("id","ko")) %>% 
      dplyr::filter(.data[["ko"]] != "")
  } else if (format == "Kofam"){
    protein2ko = utils::read.delim(file, comment.char = "#", header = FALSE) %>% 
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
    colnames(ko2pathway) = c("pathway", "ko")
    ko2pathway = ko2pathway %>%
      dplyr::mutate_all(.funs = "remove_db_prefix") %>%
      # remove redundant ko prefix pathway ids, only keep those like map00010
      dplyr::filter(stringr::str_starts(.data[["pathway"]], "map"))
    
    # some KOs don't have pathway mapping, in which case NA will be induced at pathway
    gsid2gene = protein2ko %>%
      dplyr::left_join(ko2pathway, by = "ko") %>%
      dplyr::select(c("pathway","id"))
    na_id = gsid2gene %>% 
      dplyr::filter(is.na(.data[["pathway"]])) %>%
      dplyr::pull(.data[["id"]])
    if (length(na_id) > 0){
      warning(sprintf("%d lines of KEGG mapper result didn't have pathway map, and they are ignored.\n  Totally affected %d unique gene ids.\n  The first five id are:\n%s.",
                    length(na_id), 
                    dplyr::n_distinct(na_id), 
                    paste0("    ", na_id[1:5], collapse = "\n")))
      gsid2gene = gsid2gene %>%
        dplyr::filter(!is.na(.data[["pathway"]]))
    }
    
    gsid2name = gsid2gene %>%
      dplyr::distinct(.data[["pathway"]]) %>%
      dplyr::left_join(all_pathway, by = "pathway")
  } else if (type == "module"){
    # all kegg module
    all_module = KEGGREST::keggList("module")
    all_module = stack(all_module)
    colnames(all_module) = c("module_name","module")
    all_module = all_module %>%
      dplyr::mutate_at(.vars = "module", .funs = "remove_db_prefix")
    
    # ko2module
    ko2module = KEGGREST::keggLink("module", "ko")
    ko2module = stack(ko2module)
    colnames(ko2module) = c("module", "ko")
    ko2module = ko2module %>%
      dplyr::mutate_all(.funs = "remove_db_prefix")
    
    # module to gene
    gsid2gene = protein2ko %>%
      dplyr::left_join(ko2module, by = "ko") %>%
      dplyr::select(c("module", "id"))
    
    # similar to pathway, some KOs don't mapp to any module
    na_id = gsid2gene %>% 
      dplyr::filter(is.na(.data[["module"]])) %>%
      dplyr::pull(.data[["id"]])
    if (length(na_id) > 0){
      warning(sprintf("%d lines of KEGG mapper result didn't belong to any module, and they are ignored.\n  Totally affected %d unique gene ids.\n  The first five id are:\n%s.",
                      length(na_id), 
                      dplyr::n_distinct(na_id), 
                      paste0("    ", na_id[1:5], collapse = "\n")))
      gsid2gene = gsid2gene %>%
        dplyr::filter(!is.na(.data[["module"]]))

    }
    
    # gsid2name
    gsid2name = gsid2gene %>%
      dplyr::distinct(.data[["module"]]) %>%
      dplyr::left_join(all_module, by = "module")
  } else {
    simpleError('Please specify your target database (currently "pathway" or "module").')
  }
  
  # construct gson object
  dbinfo = KEGGREST::keggInfo(type)
  version = stringr::str_extract(dbinfo, "Release [^\n]+")
  colnames(gsid2gene) = c("gsid", "gene")
  colnames(gsid2name) = c("gsid", "name")
  gson(
    gsid2gene = gsid2gene,
    gsid2name = gsid2name,
    species = species,
    gsname = paste0("KEGG_", type),
    version = version,
    accessed_date = as.character(Sys.Date()),
    ...
  )
}

remove_db_prefix = function(x){
  gsub("^[a-z]+:", "", x)
}