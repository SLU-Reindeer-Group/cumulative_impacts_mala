# Helper functions for conditional Quarto HTML/PDF rendering
# These functions provide consistent table and link formatting across both output formats

#' Detect Output Format
#'
#' Internal function to detect whether we're rendering to PDF or HTML/other format.
#' Works correctly with Quarto's output format detection.
#'
#' @return Logical: TRUE if rendering to PDF/LaTeX, FALSE otherwise
#'
#' @keywords internal
.is_pdf_output <- function() {
  # Try multiple detection methods for robustness
  # Method 1: Check knitr's is_latex_output
  if (knitr::is_latex_output()) {
    return(TRUE)
  }
  
  # Method 2: Check if we have a PDF engine specified
  # This works for Quarto PDF rendering
  pdf_engine <- Sys.getenv("QUARTO_PDF_ENGINE")
  if (nzchar(pdf_engine)) {
    return(TRUE)
  }
  
  # Method 3: Check the quarto metadata
  # This is set by Quarto during rendering
  if (exists("quarto_format", where = .GlobalEnv)) {
    format <- .GlobalEnv$quarto_format
    if (is.list(format) && !is.null(format$pdf)) {
      return(TRUE)
    }
  }
  
  # Default to FALSE (HTML/web format)
  return(FALSE)
}

#' Render Table with Format-Specific Formatting
#'
#' Conditionally renders a table using DT::datatable() for HTML output or
#' knitr::kable() with kableExtra styling for PDF output. PDF tables use
#' full page width and smaller font for better fit.
#'
#' @param data A data frame or tibble to render
#' @param escape Logical (for HTML output). If FALSE, allows HTML tags in cells.
#'              Default TRUE for security.
#' @param options List of options passed to DT::datatable() for HTML output.
#'               Default NULL.
#' @param filter Character. Filter options for DT: "none", "top", "bottom".
#'              Default "bottom" for interactive HTML.
#' @param font_size Integer. Font size for PDF tables. Default 8pt.
#' @param ... Additional arguments passed to knitr::kable() for PDF
#'
#' @return Rendered table object (DT for HTML, kable with styling for PDF)
#'
#' @examples
#' \dontrun{
#' # In a Quarto document:
#' make_table(mtcars)
#' }
#'
#' @export
make_table <- function(data, escape = TRUE, options = NULL, filter = "bottom",
                       font_size = 8, landscape = FALSE, 
                       col_widths = NULL, ...) {
  # Check if rendering to LaTeX/PDF
  if (.is_pdf_output()) {
    # Render as PDF table using kable with kableExtra styling
    # Full width + small font size helps tables fit on the page
    
    table <- knitr::kable(data, format = "latex", 
                          booktabs = TRUE, 
                          longtable = TRUE, 
                          ...)
    
    # Apply kableExtra styling for better PDF formatting
    # full_width = TRUE stretches table to page width
    # condensed reduces vertical spacing
    table <- kableExtra::kable_styling(
      table,
      # latex_options = c("striped", "condensed"),
      latex_options = c("striped", "repeat_header"),
      font_size = font_size,
      full_width = FALSE
    )
    
    if(!is.null(col_widths)) {
      stopifnot(length(col_widths) == ncol(data))
      
      for (i in seq_along(col_widths)) {
        table <- kableExtra::column_spec(
          table,
          column = i,
          width = col_widths[[i]]
        )
      }
    }
    if(landscape) table <- table |> landscape()
    
    return(table)
  } else {
    # Render as interactive HTML table
    if (is.null(options)) {
      options <- list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50),
        scrollX = TRUE,
        dom = 'lftip'
      )
    }
    DT::datatable(data, escape = escape, options = options, filter = filter)
  }
}

#' Convert URLs to Format-Specific Link Format
#'
#' Converts URLs in a data frame to clickable hyperlinks. For HTML output,
#' creates anchor tags with full URLs. For PDF output, creates LaTeX \href{} 
#' commands with shortened display text (domain only) to prevent overflow.
#'
#' @param data A data frame or tibble
#' @param url_columns Character vector of column names containing URLs.
#'                    Can be a single column name or multiple.
#' @param link_text Optional character vector with display text for links.
#'                  If NULL, uses the URL text itself for HTML, domain only for PDF.
#'
#' @return Modified data frame with URLs converted to appropriate link format
#'
#' @examples
#' \dontrun{
#' # In a Quarto document:
#' df <- data.frame(
#'   Name = c("Example 1", "Example 2"),
#'   Source = c("https://example.com", "https://example.org")
#' )
#' df <- make_links(df, url_columns = "Source")
#' make_table(df, escape = FALSE)
#' }
#'
#' @export
make_links <- function(data, url_columns, link_text = NULL) {
  for (col in url_columns) {
    if (!(col %in% names(data))) {
      warning(glue::glue("Column '{col}' not found in data frame"))
      next
    }

    urls <- data[[col]]

    if (.is_pdf_output()) {
      # For PDF: replace URLs with numbered references [1], [2], etc.
      # Initialize the global reference list if it doesn't exist
      if (!exists(".quarto_links_references", where = .GlobalEnv)) {
        .GlobalEnv$.quarto_links_references <- list()
      }
      
      # Process each URL
      new_values <- character(length(urls))
      for (i in seq_along(urls)) {
        url <- urls[i]
        
        # Skip NA, empty, or "Derived data" entries
        if (is.na(url) | url == "" | url == "Derived data") {
          new_values[i] <- url
          next
        }
        
        # Check if URL already exists in reference list
        ref_list <- .GlobalEnv$.quarto_links_references
        existing_idx <- NULL
        
        if (length(ref_list) > 0) {
          for (j in seq_along(ref_list)) {
            if (identical(ref_list[[j]]$url, url)) {
              existing_idx <- j
              break
            }
          }
        }
        
        if (!is.null(existing_idx)) {
          # URL already exists, reuse its number
          ref_num <- existing_idx
        } else {
          # New URL, add to reference list
          ref_num <- length(ref_list) + 1
          ref_list[[ref_num]] <- list(url = url, number = ref_num)
          .GlobalEnv$.quarto_links_references <- ref_list
        }
        
        # Store the numbered reference
        new_values[i] <- paste0("[", ref_num, "]")
      }
      
      data[[col]] <- new_values
    } else {
      # For HTML and other formats: create anchor tags
      data[[col]] <- ifelse(
        is.na(urls) | urls == "" | urls == "Derived data",
        urls,
        paste0('<a href="', urls, '" target="_blank">', urls, '</a>')
      )
    }
  }

  return(data)
}

#' Conditionally Include HTML-Only Content
#'
#' Wrapper function to include content only in HTML output. Useful for
#' interactive widgets or HTML-specific visualizations.
#'
#' @param expr Expression to evaluate only in HTML output
#'
#' @return Expression result if HTML output, NULL otherwise
#'
#' @examples
#' \dontrun{
#' html_only(DT::datatable(mtcars))
#' }
#'
#' @export
html_only <- function(expr) {
  if (!.is_pdf_output()) {
    expr
  }
}

#' Conditionally Include PDF-Only Content
#'
#' Wrapper function to include content only in PDF output. Useful for
#' PDF-specific formatting or layout adjustments.
#'
#' @param expr Expression to evaluate only in PDF output
#'
#' @return Expression result if PDF output, NULL otherwise
#'
#' @examples
#' \dontrun{
#' pdf_only(cat("This appears only in PDF"))
#' }
#'
#' @export
pdf_only <- function(expr) {
  if (.is_pdf_output()) {
    expr
  }
}

#' Print Link References
#'
#' Outputs a formatted reference section with all the numbered links used in the document.
#' This function should be called at the end of the document, after all tables with links.
#' Only outputs in PDF format; does nothing for HTML output.
#'
#' @return NULL (invisible). Outputs formatted reference section to the document.
#'
#' @examples
#' \dontrun{
#' # In a Quarto document, after using make_links():
#' print_link_references()
#' }
#'
#' @export
print_link_references <- function(section_title = "Data source references") {
  # Only print references for PDF output
  if (!.is_pdf_output()) {
    return(invisible(NULL))
  }
  
  # Check if references exist
  if (!exists(".quarto_links_references", where = .GlobalEnv) || 
      length(.GlobalEnv$.quarto_links_references) == 0) {
    return(invisible(NULL))
  }
  
  ref_list <- .GlobalEnv$.quarto_links_references
  
  # Create output
  cat(paste0("\n# ", section_title, "\n\n"))
  
  for (i in seq_along(ref_list)) {
    url <- ref_list[[i]]$url
    # url_short <- url |> 
    #   strsplit(split = "/") |> 
    #   dplyr::first() |> dplyr::last()
    # cat(sprintf("[%d] [%s](%s)\n\n", i, url_short, url))
    cat(sprintf("[%d] %s\n\n", i, url))
  }
  
  # Clear the reference list after printing
  .GlobalEnv$.quarto_links_references <- list()
  
  return(invisible(NULL))
}
