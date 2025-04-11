
### this combination of navigation and page layout works when there are two sets of files that need to link to each other
## for instance, where each ( or most ) plots have a readme file that goes along with them, here you can create pages that link from plot to readme and back
##. for examples of this in use, check out the PIRL Patel_SCCHN_2024 raft project's make_report.R file found in its {project}/workflow/pdf-reports/reports folder
## it can be used for any combination of pages where there are or are not sub-items

#. Responder vs Non-Responder
#     Volcano Plot
#     Line plot
#     Another plot
#  Immune Subtypes Plot
#  Treated vs Not-Treated
#     Barplot
#     Heatmap
#  Other Categories ...

###!!!  This code depends upon the shared_methods.R code for several methods ###

# my_dep refers to the primary navigation level while my_metric refers to the sub navigation list under each of the primary navigation items
# pages - all pages in this report as dataframe with Page_Key, Dep_Var, Metric columns where Page_Key should be unique for each combination of Dep_Var and Metric
# my_dep - the Dep_Var for the current page ... the one that should be shown as active in the navigation
# my_metric - the Metric for the current page ... the one that should be shown as active in the navigation
# dep_var_lut - the lookup table for pretty/descriptive labels of Dep_Var column values ( which may have to be valid for filenames, etc. so not pretty for display )
# my_prefix - the prefix for this set of pages ... this method will be called twice, once for each of the mirrored groups of files ... 
#     i.e. once with my_prefix = "readme" and once as my_prefix = "plot" so that links with the same Page_Key can be differentiated 
# links_enabled - whether to print menu items as links, or as normal text - to test navigation as standalone
# list_leftmargin_txt - how far to shift nav elements to the right
# list_fontsize_lbl - how large is font for the list elements
make_primary_navigation <- function( pages, my_dep, my_metric, dep_var_lut, my_prefix, links_enabled=T, list_leftmargin_txt="0in", list_fontsize_lbl="small" ) {
  begin_list_tex <- paste0("\\begin{description}[leftmargin=", list_leftmargin_txt, ",topsep=1pt,itemsep=4pt,parsep=0pt,partopsep=0pt]")
  page_links <- c(begin_list_tex, paste0("\\begin{", list_fontsize_lbl, "}")) # description is a list without enumeration or bulleting
  # first link will be the standalone immune subtypes ...
  # if ( my_dep == "immune_subtypes" | !links_enabled ) {
  #   page_links %<>% c(paste0("\\item{", link_group_header( "Immune Subtypes" ), "}"))
  # } else {
  #   page_links %<>% c(paste0("\\item{", link_group_header("Immune Subtypes", label=paste0(my_prefix, "_Immune_Subtypes")), "}"))
  # }
  #  page_links %<>% c("\\vspace{.5cm}")
  #		page_links %<>% c("\\bigskip")
  #		page_links %<>% c("\\noindent")
  # now make links to all plots for each dependent var in this analysis
  #   the metric link for this page's dep_var will be unclickable text
  #   we will use colored links under each dep_var header for the plot of the same metric as this page
  #   we will use normal links for all other plots
  for ( dep in unique(pages$Dep_Var) ) {
    #dep <- pages$Dep_Var[1]
    if ( my_dep == dep | !links_enabled ){
      this_key <- NA
    } else {
      pk <- pages$Page_Key[pages$Dep_Var == dep & pages$Metric == my_metric]
      if ( length(pk) == 0 ) { # in instances where there is no plot for a given metric, default to the first plot for that dep_var
        pk <- pages$Page_Key[ pages$Dep_Var == dep ][1]
      }
      this_key <- paste0( my_prefix, "_", pk )
    }
    page_links %<>% c(paste0("\\item{", link_group_header( dep_var_lut[ dep ], label=this_key), "}"))
    # page_links %<>% c("\\vspace{-0.5cm}")
    # page_links %<>% c("\\raggedright")
    # page_links %<>% c("\\noindent")
    if ( dep == my_dep ) {
      sub_df <- pages[ pages$Dep_Var == dep, ]
      page_links %<>% c(begin_list_tex) #, paste0("\\begin{", list_fontsize_lbl, "}"))
      for ( met in sub_df$Metric %>% as.character() ) {
        #  		    page_links %<>% c("\\smallskip")
        # met <- sub_df$Metric[2] %>% as.character()
        if ( met == my_metric | !links_enabled ) {
          #  		      if ( my_dep == dep ){
          page_links %<>% c( paste0("\\item{", current_link( met ), "}") )
          #  		      } else {
          #  		        page_links %<>% c( clickable_link( met, sub_df$Page[sub_df$Metric == met], color="green" ) )
          #  		      }
        } else {
          page_links %<>% c( paste0("\\item{", clickable_link( met, label=paste0(my_prefix, "_", sub_df$Page_Key[sub_df$Metric == met] ) ), "}"))
        }
      }
      page_links %<>% c("\\end{description}") # paste0("\\end{", list_fontsize_lbl, "}"), 
    }
    #    page_links %<>% c("\\smallskip")
  }
  page_links %<>% c(paste0("\\end{", list_fontsize_lbl, "}"), "\\end{description}")
  #  page_links %<>% c("\\end{raggedright}")
  # if ( nav_clms > 1 ) {
  #   #		  page_links %<>% c("\\end{raggedright}")
  #   page_links %<>% c("\\end{multicols}")
  # } 
  return(page_links)
}

# workhorse method that creates a single page with clickable links to all slides and whatever plot goes on that page
make_new_pdf_page = function(my_df, my_index, nav_proportion=.3, this_prefix="plot", to_prefix="readme", links_enabled=T, nav_leftmargin_txt="0in", nav_fontsize_lbl="small" ){
  # my_index <- 1
  # if ( my_index <= 0 ) { # this is one of the standalone immune subtypes plots 
  #   subtypes_index <- my_index + 2
  #   my_path = immune_subtypes_df$Path[subtypes_index]
  #   my_dep = immune_subtypes_df$Dep_Var[subtypes_index] %>% as.character
  #   my_metric = immune_subtypes_df$Metric[ subtypes_index ] #metrics[1]
  #   my_width = immune_subtypes_df$Px_Width[ subtypes_index]
  #   my_height = immune_subtypes_df$Px_Height[ subtypes_index]
  #   my_data_file = immune_subtypes_df$Data_File[ subtypes_index]
  #   my_page_key = "Immune_Subtypes"
  #   my_readme_path = NA
  #   my_to_link <- ""
  # } else { # this is one of the plots in the main df
    my_path = my_df$Path[my_index]
    my_dep = my_df$Dep_Var[my_index] %>% as.character
    my_metric = my_df$Metric[ my_index ] %>% as.character
    my_width = my_df$Px_Width[ my_index ]
    my_height = my_df$Px_Height[ my_index ]
    my_data_file = my_df$Data_File[ my_index ]
    # old method of hyperlinking ... my_readme_link <- paste0("\\hyperlink{", my_df$Filename[ my_index ], "}{View Readme}")
    my_page_key <- my_df$Page_Key[ my_index ]
    my_readme_path = my_df$Readme_Path[ my_index ]
    if ( is.na(my_readme_path) ) {
      my_to_link <- ""
    } else if ( links_enabled ) {
      my_to_link <- paste0("\\hyperlink{", to_prefix, "_", my_page_key, "}{View ", stringr::str_to_title(to_prefix), "}")
    } else {
      my_to_link <- paste0("View ", stringr::str_to_title(to_prefix), " ( links disabled for testing )")
    }
  # }	
  
  
  #######.  WRITE NAV LINKS. #######
  # set up for multi-column display of dep vars / metrics plot links
  # use the [c] option to center on both height and width
  nav_latex <- paste0("\\begin{minipage}[c][\\textheight][c]{", nav_proportion %>% as.character(), "\\linewidth}\\centering")
  nav_latex %<>% c(make_primary_navigation(my_df, my_dep, my_metric, dep_var_lut, this_prefix, links_enabled, list_leftmargin_txt=nav_leftmargin_txt, list_fontsize_lbl=nav_fontsize_lbl))
  nav_latex %<>% c("\\end{minipage}")
  
  # if this is a readme page, we need to hfill the left and make the minipage narrower or add an hspace ...
  
  # no reason to hide the hypertargets whether links are enabled or not ... 
  target_latex <- paste0("\\hypertarget{", this_prefix, "_", my_page_key, "}")
  
  if ( this_prefix == "plot" ) {
    content_latex <- paste0("\\begin{minipage}[c][\\textheight][c]{", .97-nav_proportion, "\\linewidth}")
    content_latex %<>% c(target_latex, "\\vfill")
    content_latex %<>% c("\\centering") # horizontal centering
    if ( my_width > my_height ) {
      image_tag <- paste0("width=.95\\linewidth,keepaspectratio") # using \\linewidth rather than \\textwidth because it adapts to the environment ( such as within a minipage )
    } else {
      image_tag <- paste0("height=.9\\textheight,keepaspectratio")
    }
    content_latex %<>% c(
      paste0("\\includegraphics[", image_tag, "]{", file.path(input_dirpath, my_path), "}\\par"), 
      "\\vfill",
      paste0("Data From: \\verb|", my_data_file, "|\\par")
    )
  } else if ( this_prefix == "readme" ) {
    # for now assuming just one column worth of readme content ...
    content_latex <- paste0("\\hspace{.04\\linewidth}\\begin{minipage}[c][\\textheight][c]{", .97-nav_proportion-.1, "\\linewidth}") # -.1 here will allow for margins of .04 on the left ( added this line ) and .06 on the right ( added near the end of this method )
    content_latex %<>% c(target_latex, "\\ttfamily", "\\small", "\\vfill")
    # must use \raggedright or minipage will choke on any actual empty lines ...
    content_latex %<>% c("\\raggedright")
    if( is.na(my_readme_path) ) {
      content_latex %<>% c("No readme available for this file.\\par")
    } else {
      content_latex %<>% c( readme_contents(file.path(input_dirpath, my_readme_path)) )
    }
    content_latex %<>% c("\\vfill")
  } else {
    # lets put a dummy content page here so we can test navigation with smaller pdf
    content_latex <- c(paste0("\\begin{minipage}[c][\\textheight][c]{", .97-nav_proportion-.1, "\\linewidth}"), target_latex, "\\centering","This is only a test.\\newline")
  }
  
  content_latex %<>% c(paste0("\\vspace{.1in}", my_to_link),
                       "\\vspace{.2in}") # pulling the links off the bottom of the page
  content_latex %<>% c("\\end{minipage}")
  if ( this_prefix == "readme" ) {
    content_latex %<>% c("\\hspace{.06\\linewidth}")
  }
  
  return(c("\\begin{landscape}", nav_latex, "\\hspace{0in}\\vrule\\hspace{0in}", content_latex, "\\end{landscape}"))
}
