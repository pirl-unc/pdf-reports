### validate image being used has necessary libraries
validate_requirements <- function( required_system_tools ) {
  can_proceed = TRUE
  for ( required_system_tool in required_system_tools ){
    tool_exists = suppressWarnings(length(system(
      paste0("which ",required_system_tool," 2>/dev/null"),
      intern = TRUE
    )) > 0)
    
    if(! tool_exists){
      message(required_system_tool, " was not found")
      can_proceed = FALSE
    }
  }
  if ( !can_proceed ) {
    stop("Please install the required tools or run via an image: \nrun_rserver -i /datastore/nextgenout5/share/labs/Vincent_Lab/tools/raft/imgs/benjaminvincentlab-rserver-binfotron-4.2.1.57.img -c 1 -m 1g -H /datastore/nextgenout5/share/labs/Vincent_Lab/tools/raft/projects/H37004_2024
  ")
  }
}

# there are some screwy requirements for formatting things in latex ... the following seems to work
clean_content_for_latex <- function(content) {
  content <- gsub(">", "\x3E", content)
  content <- gsub("<", "\x3C", content)
  content <- gsub("&", "\\\\&", content)
  content <- gsub("%", "\\\\%", content)
  content <- gsub("\\$", "\x24", content)
  content <- gsub("#", "\\\\#", content)
  content <- gsub("_", "\\\\_", content)
  content <- gsub("~", "\x7E", content)
  content <- gsub("\\^", "\x5E", content)
  return(content)
}

# helper for pulling dimensions of pdf files from the system
get_plot_dimensions <- function( plot_path ) {
  my_meta = system(paste0('pdftk ', file.path(input_dirpath, plot_path),' dump_data'), intern = T)
  
  my_dim = my_meta[grepl("PageMediaDimensions:", my_meta)]
  my_dim = gsub("PageMediaDimensions: ","",my_dim)
  my_dim = as.numeric(strsplit(my_dim,split=" ")[[1]])
  
  return( my_dim )
}

# simple helper to add arbitrary text to the my_tex variable
t = function(...){
  new_text = paste0(...)
  note_envir = pryr::where('my_tex')
  assign('my_tex', c(get('my_tex', envir = note_envir), new_text), envir = note_envir)
}



readme_contents <- function( readme_path ) {
  content <- readLines(readme_path)
  # turns out first line of most readmes lists the path to this readme file ... turns out that is too line for one line and latex tends to choke on hyphenating
  #.  since that information is irrelevant and probably just confusing for end viewers of this report, lets get rid of it
  #.  fortunately the code that writes out that line is within the housekeeping package so the output is consistent
  if ( content[1] %like% "Readme path" ) {
    content %<>% .[ -1 ]
  }
  # escape the { and } before adding header which uses {} that mustn't be escaped
  content <- gsub("\\{", "\x7B", content)
  content <- gsub("\\}", "\x7D", content)
  
  # Escape other special LaTeX characters in the content
  clean_content <- clean_content_for_latex(content)
  clean_content %<>% paste0("\\newline") # append end of line character otherwise there are no newlines in readme content
  
  # we don't want a bunch of newlines at the end of the readme content
  while( length(clean_content) > 1 & clean_content[ length(clean_content) ] == "\\newline" ) {
    clean_content %<>% .[ -length(.) ]
  }
  
  return(clean_content)
}

# find the readmes associated with each page
# as with data files, this script updates my_df in global scope and thus must be run after that variable is created
add_readmes <- function ( primary_df, readme_dirpath=input_dirpath ) {
  my_readmes = list.files(readme_dirpath, include.dirs = F, recursive = T, pattern = "readme.txt")
  
  primary_df$Readme_Path <- NA
  for( idx in 1:nrow(primary_df) ) {
    # idx <- 1
    readme_name <- paste0(primary_df$Filename[idx], "_readme.txt")
    this_readme <- grep(paste0("/", readme_name), my_readmes, value = T)
    if( !length(this_readme) ) {
      print(paste0("The readme for ", primary_df$Filename, " could not be found."))
    } else if ( length(this_readme) > 1 ) {
      print(paste0("More than 1 readme was found for ", primary_df$Filename, "."))
    } else {
      primary_df$Readme_Path[ idx ] <- this_readme
    }
  }
  return( primary_df )
}


boilerplate_header <- function( margin_txt=".1in" ) {
  return(c(
    "\\documentclass[10pt,linkcolor = blue,titlepage]{article}", # can't define documentclass with landscape option as it conflicts with pdflscape package
    paste0("\\usepackage[margin=", margin_txt, "]{geometry}"),
    "\\usepackage{pdfpages}",
    "\\usepackage{hyperref}",
    "\\usepackage{pdflscape}",
    "\\usepackage[english]{babel}",       # Allows automatic hyphenation
    "\\usepackage{nopageno}",
    "\\usepackage{multicol}",
    "\\usepackage{pdflscape}",
    "\\usepackage{enumitem}",
    "\\usepackage[utf8]{inputenc}", # Ensure UTF-8 encoding
    "\\usepackage{lmodern}", # Use Latin Modern font
    "\\hypersetup{",
    "  colorlinks = true,",
    "}",
    "\\begin{document}"
  ))
}

# helper to create a title page with image and report data
create_title_page = function(	pdf_title=report_title, pdf_logo=my_logo, pdf_author=report_author, pdf_assistant_author=report_assistant_author ){
  my_output = c(
    "\\hypersetup{pageanchor=false}",
    "\\begin{titlepage}",
    "\\begin{landscape}",
    "\\centering",
    "\\hspace{0pt}",
    "\\vfill",
    paste0("\\includegraphics[height=4in,keepaspectratio]{", pdf_logo, "}\\par"), # yes we have to keep the \\par here, otherwise latex tries to put the following text on the same line
    "\\vspace{.2in}", 
    paste0("{\\LARGE ", pdf_title, "}\\par"),
    "\\vspace{.2in}", 
    paste0("{\\large\\textsc{", pdf_author, "}}\\par")
  )
  if ( !is.na(report_assistant_author) ) {
    my_output %<>% c("\\vspace{.05in}", paste0("{\\textsc{and ", pdf_assistant_author, "}}\\par"))
  }
  my_output %<>% c(
    "\\vfill",
    paste0("{\\large ", format(Sys.time(), "%B %d, %Y"), "}"),
    "\\vspace{.2in}",
    "\\end{landscape}",
    "\\end{titlepage}",
    "\\hypersetup{pageanchor=true}"
  )
  return(my_output)
}

make_single_page <- function( content ) {
  return( c("\\begin{landscape}", 
            "\\centering", 
            "\\hspace{0pt}", 
            "\\vfill", 
            content, 
            "\\vfill", 
            "\\end{landscape}") )
}

# helpers for outputting links and link headers

link_group_header <- function( dep_var_name, label=NA ) {
  # default is no underline but if there is a page to link to, underline the text
  hyperlink_text <- dep_var_name
  if( !is.na(label) ) {
    hyperlink_text <- paste0("\\underline{\\hyperlink{", label, "}{", dep_var_name, "}}")
  }
  #  {\underline{\hyperlink{page.3}{Pre vs Post Treatment}}}
  return(paste0("\\hspace*{1em}{", hyperlink_text, "}")) #\\\\"))
}

current_link <- function( link_text ) {
  return( paste0("\\hspace*{2em}{\\small\\textbf{", link_text, "}}")) #\\\\" ) )
}

clickable_link <- function( link_text, label, color=NA ) {
  clr_text <- ""
  if ( !is.na(color) ) {
    #clr_text <- paste0("\\textcolor{", color, "}")
  }
  return( paste0("\\hspace*{2em}{\\small", clr_text, "\\hyperlink{", label, "}{", link_text, "}}")) #\\\\"))
}

