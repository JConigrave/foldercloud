utils::globalVariables(c(".","errors","filepaths","subfolders","exclude", "file_paths"))


#' search_pdfs
#'
#' Search through pdfs for target words
#' @param folder the path to a folder
#' @param pattern regex search pattern
#' @param exec a bool. If true, the pdfs will be openeded (windows only)
#' @param ... additional arguments to grepl
#' @export search_pdfs

search_pdfs = function(folder, pattern, exec = T, ...){

  corpus = pdf_words(folder, unnest_tokens = F)
  names(corpus) = c("path", "content")

  rows = grepl(pattern, corpus$content, ignore.case = T, ...)

  results = unique(corpus[rows,])

  pdfs = unique(results$path)

  if(exec & length(pdfs) > 0 ){
    for(p in pdfs) shell.exec(p)
  }

  return(pdfs)

}
