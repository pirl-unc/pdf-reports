
# pages <- my_df
# page_key <- "Immune_Lymphoid:t_cell:fp:1"
make_primary_navigation <- function( pages, page_key, links_enabled=T, list_fontsize_lbl="small" ) {
  
  my_category <- pages$Category[ pages$Page_Key == page_key ]
  primary_nav <- pages$Pretty_Category %>% unique
  first_category_keys <- mapply(function(pc) return(pages$Page_Key[ pages$Pretty_Category == pc & pages$Plot_Order == 1 ][1]), primary_nav)
  which_page <- which(primary_nav == pages$Pretty_Category[ pages$Page_Key == page_key] )
  primary_nav[ which_page ] %<>% paste0("\\textbf{", ., "}")
  primary_nav[ -which_page ] %<>% {paste0("\\underline{\\hyperlink{", first_category_keys[ . ], "}{", ., "}}")}
  
  primary_nav %<>% paste0(., collapse="\\quad") %>% c()
  # T Cell \\hyperlink{page_key}{p1}\\quad\\textbf{p2}\\quad p3 p4 \\hfill  B Cell p1
  secondary_nav_df <- pages[ pages$Category == my_category, c("Sub_Category", "Plot_Order", "Pretty_Sub_Category", "Page_Key") ]
  secondary_nav <- c()
  for ( subcat in unique(secondary_nav_df$Sub_Category)) {
    # subcat <- unique(secondary_nav_df$Sub_Category)[1]
    tmp_df <- secondary_nav_df[ secondary_nav_df$Sub_Category == subcat, ]
    this_tmp_nav <- c(paste0(tmp_df$Pretty_Sub_Category[1], ":\\quad"))
    for ( idx in 1:nrow(tmp_df) ) {
      # idx <- 2
      if ( tmp_df$Page_Key[[idx]] == page_key ) {
        this_tmp_nav %<>% c(paste0("\\textbf{", tmp_df$Plot_Order[[idx]], "}"))
      } else {
        this_tmp_nav %<>% c(paste0("\\hyperlink{", tmp_df$Page_Key[[idx]], "}{", tmp_df$Plot_Order[[idx]], "}"))
      }
      this_tmp_nav %<>% c("\\quad")
    }
    # the last \\quad should be larger space between subcategories
    secondary_nav %<>% c(paste0(c(this_tmp_nav[-length(this_tmp_nav)], "\\hspace{1cm}"), collapse=" "))
  }

  return(c("\\small{",primary_nav, "}\\\\\\vspace{.2cm}\\small{", secondary_nav, "}\\\\"))
}


# workhorse method that creates a single page with clickable links to all slides and whatever plot goes on that page
# assumption is that plots already have a title, but if they don't one will be created based on the Sub_Category
make_new_pdf_page = function(my_df, page_key, links_enabled=T, include_title=F){

  my_height <- my_df$Px_Height[ my_df$Page_Key == page_key ]
  my_width <- my_df$Px_Width[ my_df$Page_Key == page_key ]
  my_path <- my_df$Path[ my_df$Page_Key == page_key ]
  
  target_latex <- paste0("\\hypertarget{", page_key, "}")
  nav_latex <- make_primary_navigation(my_df, page_key)
  
  if ( include_title ) {
    content_latex <- c(paste0("\\vfill\\large{Feature Plot ", my_df$Pretty_Sub_Category[ my_df$Page_Key == page_key ], "}\\\\\\vspace{.2cm}"))
  } else {
    content_latex <- "\\vfill"
  }
  
  image_tag <- paste0("width=.95\\linewidth,keepaspectratio") # using \\linewidth rather than \\textwidth because it adapts to the environment ( such as within a minipage )

  content_latex %<>% c(
    paste0("\\includegraphics[", image_tag, "]{", file.path(input_dirpath, my_path), "}\\par"), 
    "\\vfill"
  )
  rtn <- c(target_latex, "\\centering", nav_latex, "\\vspace{.1in}\\hrule\\vspace{.1in}", content_latex)
  if ( pdfreport_layout_landscape ) {
    rtn %<>% c("\\begin{landscape}", ., "\\end{landscape}")
  }
  return(rtn)
}
