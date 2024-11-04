

####. these are the methods shared across several projects ecotyper and cibersortx subtypes pdf
##. they depend as currently written on specific fields available in the primary_df
##. as this is used in other projects, these clms will have to be extracted into parameters 
##      OR
##. perhaps we can call column/filename formatting functions that are written within each projects primary workflow


#  get data files and add paths to primary df
# primary_df <- my_df
# files_path <- data_files_path
add_data_files <- function( primary_df, files_path=data_files_path ) {
  
  # start with ecotyper data files which end in .tsv
  data_files <- list.files(files_path, pattern="*.tsv") %>% gsub("\\t", "", .)
  cibersort_data_files <- list.files(files_path, pattern="*.csv")
  
  for ( idx in 1:nrow(primary_df) ) {
    # idx <- 5
    if ( primary_df$Platform[idx] == "ecotyper" ) {
      primary_df$Data_File[ idx ] <- paste0(primary_df$Configuration[ idx ], "_", primary_df$Metric[idx], "_heatmap_data.tsv")
    } else {
      timepoint <- gsub("\\_.*$", "", primary_df$Configuration[idx])
      primary_df$Data_File[ idx ] <- paste0("merged_CIBERSORTx_", primary_df$Metric[ idx ], "_", timepoint, ".csv")
    }
  }
  
  return(primary_df)
  
}


# this method wil override the default add_readmes method which is in the reports shared methods file
## !! this must be loaded after sourcing the shared file

warning("Ensure that the ecotyper_and_cybersortx.R file is included AFTER any inclusion of shared_methods.R because this add_readmes must overload the share_methods one.")

add_readmes <- function ( primary_df, readme_dirpath=input_dirpath ) {
  # at present we only have readmes for the cibersort processes, and recall that both corrected and uncorrected results are from the same process so have a shared readme files
  primary_df$Readme_Path <- NA
  for( idx in 1:nrow(primary_df) ) {
    # idx <- 1
    if ( primary_df$Platform[idx] == "cibersortx" ) {
      m <- gsub("^((on)|(pre))_by_", "", primary_df$Configuration[idx])
      t <- gsub("_.*$", "", primary_df$Configuration[idx])
      # print(file.path(readme_dirpath, "cibersort", paste0("cibersort_", m, "_", t, "_plot_cibersort_readme.txt")))
      primary_df$Readme_Path[idx] <- file.path(readme_dirpath, "cibersort", paste0("cibersort_", m, "_", t, "_plot_cibersort_readme.txt"))
    }
  }
  return( primary_df )
}

# ===========================================================================
# ===========================================================================
# ===========================================================================
# my_header is the header of the current page
# my_link is the link to the current page
make_primary_navigation <- function( primary_df, my_header, my_link, links_enabled=T, list_leftmargin_txt=".1in", list_fontsize_lbl="small" ) {
  begin_list_tex <- paste0("\\begin{description}[leftmargin=", list_leftmargin_txt, ",topsep=1pt,itemsep=4pt,parsep=0pt,partopsep=0pt]")
  page_links <- c(begin_list_tex, paste0("\\begin{", list_fontsize_lbl, "}"))
  # primary_df <- my_df
  # my_header <- primary_df$Header[ 8 ]
  # my_link <- primary_df$Link[ 8 ]
  # make links to plots for each platform/metric in this analysis
  for ( header in unique(primary_df$Header) ) {
    # header <- unique(primary_df$Header)[2]
    # if this header is NOT the header of the current page ...
    if ( header != my_header ) {
      # this will find the plot of the same metric in the other header group for this platform, if one exists
      this_page <- primary_df$Page_Key[ primary_df$Header == header & primary_df$Link == my_link ]
      if( length(this_page) == 0 ) {
        # default to first plot in the given header group
        this_page <- primary_df$Page_Key[ primary_df$Header == header ][1]
      }
      page_links %<>% c(paste0("\\item{", link_group_header( header, label=this_page), "}"))
    } else {
      page_links %<>% c(paste0("\\item{", link_group_header( header, NA ), "}"))
      sub_df <- primary_df[ primary_df$Header == header, ]
      page_links %<>% c(begin_list_tex)
      for ( index in 1:nrow(sub_df) ) {
        # index <- 2
        link <- sub_df$Link[index] %>% as.character()
        page_key <- sub_df$Page_Key[index]
        #  		    page_links %<>% c("\\smallskip")
        if ( link == my_link ) {
          page_links %<>% c( paste0("\\item{", current_link( link ), "}") )
        } else {
          page_links %<>% c( paste0("\\item{", clickable_link( link, page_key ), "}") )
        }
      }
      page_links %<>% c("\\end{description}")
    }
  }
  page_links %<>% c(paste0("\\end{", list_fontsize_lbl, "}"), "\\end{description}")
  
  return(page_links)
}

# workhorse method that creates a single page with clickable links to all slides and whatever plot goes on that page
make_new_pdf_page = function(primary_df, my_index, this_prefix="plot", nav_proportion=.3, links_enabled=T, nav_only=F, metric_lookup=metric_lut, nav_leftmargin_txt="0in", nav_fontsize_lbl="small"){
  # my_index <- 1
  my_path = primary_df$Path[my_index]
  my_platform = primary_df$Platform[my_index] %>% as.character
  my_header = primary_df$Header[my_index] %>% as.character
  my_configuration = primary_df$Configuration[my_index] %>% as.character
  my_metric = primary_df$Metric[ my_index ] %>% as.character
  my_link = primary_df$Link[ my_index ] %>% as.character
  my_page_key = primary_df$Page_Key[ my_index ]
  my_width = primary_df$Px_Width[ my_index ]
  my_height = primary_df$Px_Height[ my_index ]
  my_data_file <- primary_df$Data_File[ my_index ]
  my_readme_file <- primary_df$Readme_Path[ my_index ]
  
  
  nav_latex <- paste0("\\begin{minipage}[c][\\textheight][c]{", nav_proportion %>% as.character(), "\\linewidth}")
  nav_latex %<>% c(make_primary_navigation(primary_df, my_header, my_link, links_enabled, list_leftmargin_txt=nav_leftmargin_txt, list_fontsize_lbl=nav_fontsize_lbl))
  nav_latex %<>% c("\\end{minipage}\\hspace{0px}%") # \hspace is to override any default spacing minipage may automatically add between pages
  
  target_latex <- paste0("\\hypertarget{", my_page_key, "}")
  
  if ( this_prefix == "plot" ) {
    content_latex <- paste0("\\begin{minipage}[c][\\textheight][c]{", .98-nav_proportion, "\\linewidth}")
    content_latex %<>% c(target_latex, "\\vfill")
    content_latex %<>% c("\\centering") # horizontal centering
    if ( my_platform == "ecotyper" ) {
      # add plot title here since ecotyper doesn't include them
      plot_title <- clean_content_for_latex( paste0("Ecotyper: ", metric_lookup[my_metric], " ( ", stringr::str_to_title(my_configuration), " Treatment )" ))
      content_latex %<>% c(paste0("\\begin{large}\\textbf{", plot_title, "}\\end{large}\\par\\vspace{.05in}"))
    }
    if ( nav_only ) {
      content_latex %<>% c("\\begin{huge} Navigation ONLY Test\\end{huge}")
    } else {
      if ( my_width > my_height ) {
        image_tag <- paste0("width=.96\\linewidth,keepaspectratio") # using \\linewidth rather than \\textwidth because it adapts to the environment ( such as within a minipage )
      } else {
        image_tag <- paste0("height=.9\\textheight,keepaspectratio")
      }
      content_latex %<>% c( paste0("\\includegraphics[", image_tag, "]{", file.path(input_dirpath, my_path), "}") )
    }
    content_latex %<>% c(
      "\\par", 
      "\\vfill",
      paste0("Data From: \\verb|", my_data_file, "|\\par\\vspace{.2in}")
    )
  }
  # else if ( this_prefix == "readme" ) {
  #   # for now assuming just one column worth of readme content ...
  #   content_latex <- paste0("\\hspace{.1\\linewidth}\\begin{minipage}[c][\\textheight][c]{", .97-nav_proportion-.1, "\\linewidth}")
  #   content_latex %<>% c(target_latex, "\\ttfamily", "\\small", "\\vfill")
  #   # must use \raggedright or minipage will choke on any actual empty lines ...
  #   content_latex %<>% c("\\raggedright")
  #   if( is.na(my_readme_path) ) {
  #     content_latex %<>% c("No readme available for this file.\\par")
  #   } else {
  #     content_latex %<>% c( readme_contents(file.path(input_dirpath, my_readme_path)) )
  #   }
  #   content_latex %<>% c("\\vfill")
  # }
  
  #  content_latex %<>% c(paste0("\\vspace{.1in}", my_to_link),
  #                       "\\vspace{.2in}") # pulling the links off the bottom of the page
  content_latex %<>% c("\\end{minipage}")
  
  
  return(c("\\begin{landscape}", nav_latex, "\\vrule\\hspace{0px}%", content_latex, "\\end{landscape}"))
  
}
