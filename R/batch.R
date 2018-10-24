#' foldercloud
#'
#' takes in the path of a folder containing pdfs. Performs frequency analysis and produces a wordcloud.
#' @param folder a string. The path to a folder, or the path to a pdf
#' @param cloudname a string. The name of the destination file. Include document type. Default is "wordcloud.png".
#' @param subfolders a logical. If TRUE, the algorithm will find all pdfs in all subfolders of the target path
#' @param max.words a numeric. Maximum number of words to be plotted. least frequent terms dropped
#' @param min.freq a numeric. Words with frequency below min.freq will not be plotted
#' @param scale a numeric vector. Scale indicates how minimum and maximum word size.
#' @param width a numeric. How many units wide to make the final plot.
#' @param height a numeric. How many units high to make the final plot.
#' @param units a string. The units in which height and width are given. Can be px (pixels), in (inches), cm(default) or mm.
#' @param res a numeric. The nominal resolution in ppi (pixels per inch) which will be recorded in the bitmap file. Default is 600ppi.
#' @param exclude a charcater vector. Words in this vector will be excluded from the final plot.
#' @param save a boolean. If save is set to True, a wordcloud will be saved to your working directory.
#' @param ... additional arguments may be passed to the word cloud here.
#' @export foldercloud
#' @importFrom pdftools pdf_text
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate as_tibble tibble arrange desc rename
#' @importFrom tm stripWhitespace stopwords
#' @importFrom tidytext unnest_tokens
#' @importFrom stringr str_extract
#' @importFrom wordcloud wordcloud
#' @importFrom grDevices dev.off png
#' @importFrom stats na.omit
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @return A list containing a word frequency table, and a wordcloud

foldercloud = function(folder,
                       cloudname = NULL,
                       subfolders = TRUE,
                       max.words = 500,
                       scale = c(4, .28),
                       min.freq = 3,
                       width = 17,
                       height = 17,
                       units = "cm",
                       res = 600,
                       save = T,
                       exclude = c(),
                       ...) {

  getPdf = function(filename) {
    out = NA
    tryCatch({
      suppressMessages(out <-
                         filename %>%
                         pdf_text %>%
                         tm::stripWhitespace() %>%
                         paste(collapse = " "))
    }, error = function(e) {

    })
    return(out)
  }

  prevent_duplicates = function(path = NULL){
    if(is.null(path)){
      path = "wordcloud.png"
    }
    ext = paste0(".",tools::file_ext(path))
    if(ext == "."){
      ext = ".png"
      path = paste0(path,ext)
    }
    filename = basename(path)
    folder = dirname(path) %>%
      ifelse(nchar(.)<2,getwd(),.)

    while(filename %in% dir(folder)){ #while the folder contains a file with the same name
      if(!grepl("\\[\\d\\]",filename)){ #if there's not brackets with a name inside
        filename = paste0(gsub(ext,paste0("[1]",ext),filename)) #add brackets with the number 1 in side
      }else{
        pod = str_extract(filename, "\\[\\d\\]") #otherwise grab the brackets
        num = as.numeric(gsub("\\D","", pod))+1 #get the number out and add 1 to it
        filename = gsub(paste0("\\[\\d\\]",ext),"",filename) #remove the old brackets with numbers in it

        filename = paste0(filename,"[",num,"]",ext) #add the new brackets with higher number in it
      }
    }
    return(paste(folder,filename,sep = "/")) #return the filename with the folder address pasted to it.
  }

  cloudname = prevent_duplicates(cloudname)

  ##define global variables
  .<-content<-word<-Freq<-Var1<-NULL #this is so R won't freak out that these global varibales are in this function yet seemingly never defined.

  stop_words = tibble(word = tm::stopwords(), lexicon = "NA")

  file_names = list.files(
    folder,
    pattern = ".pdf",
    recursive = subfolders,
    full.names = F
  )
  file_paths = list.files(
    folder,
    pattern = ".pdf",
    recursive = subfolders,
    full.names = T
  )

  if(length(file_names) == 0 & tools::file_ext(tolower(folder)) == "pdf"){
  file_names = basename(folder)
  file_paths = folder
  }

  if(length(file_names) == 0){
    stop("There aren't any .pdfs in that folder.", call. = F)
  }

  message(paste0("loading ", length(file_names), " pdfs..."))
  pb <- txtProgressBar(min = 0,
                       max = length(file_names),
                       style = 3)
  corpus = lapply(seq_along(file_names), function(x) {
    out = tibble(path = file_names[x], content = getPdf(file_paths[x]))
    setTxtProgressBar(pb, x)
    return(out)
  }) %>%
    do.call("rbind", .) %>%
    mutate(content = as.character(content))
  close(pb)
  errors = corpus[is.na(corpus$content), "path"]
  corpus = na.omit(corpus)
  if (nrow(corpus) == 0) {
    stop("No pdf files could be read. Try different ones.")
  }
  backup = corpus
  message("cleaning...")
  suppressMessages(corpus <- corpus %>%
                     unnest_tokens(word, content))
  corpus$word = tolower(corpus$word)
  corpus$word = gsub("[[:digit:]]", "", corpus$word)
  corpus$word = gsub("[[:punct:]]", "", corpus$word)
  suppressMessages(
    corpus <- corpus %>%
      dplyr::filter(nchar(word) > 2) %>%
      dplyr::filter(!word %in% exclude) %>%
      dplyr::anti_join(stop_words) %>%
      as_tibble
  )

  words = data.frame(table(corpus$word)) %>%
    arrange(desc(Freq)) %>%
    as_tibble %>%
    rename(word = Var1, freq = Freq) %>%
    mutate(word = as.character(word))
if(save == T){
  "saving wordcloud" %>%
    paste0(": ", cloudname) %>%
    message
  png(
    cloudname,
    width = width,
    height = height,
    units = units,
    res = res
  )
}
  wordcloud(
    words = words$word,
    freq = words$freq,
    max.words = max.words,
    scale = scale,
    min.freq = min.freq,
    ...
  )
  if(save == T){
  dev.off()
  }
if(nrow(errors)>0){
  warning(paste0(nrow(errors), " (",round(nrow(errors)/length(file_paths)*100,1),"%) pdfs could not be read"),call.=F)
}
  return(list(unread = errors,words = words))
}
