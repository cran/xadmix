#' Admixture Data Stacked Barplot
#'
#' Stacked barplot optimized for admixture data.
#' @param data Data frame containing the admixture data.
#' @param K Positions of the columns containing the ancestry percentages in the provided data frame; default is second to last column.
#' @param individuals Position of the column with the names for the x-axis; default is the first column.
#' @param sortkey Name of the column containing ancestry percentages to sort the stacked barplot with.
#' @param grouping Name of the column by which the stacked bars are to be grouped.
#' @param palette Either a color palette object, or a string to use one of the predefined color palettes ("viridis", "turbo", "alternating"); default is a modified ggplot palette.
#' @param names Whether to show the x-axis bar labels or not; default is "TRUE".
#' @param xlab A label for the x-axis.
#' @param ylab A label for the y-axis.
#' @param main A main title for the plot.
#' @param noclip Directly draw the plot, with clipping removed from elements. Then function does not return an object; default is set to "FALSE". Setting to "TRUE" may require launching a new R graphics device.
#' @return A ggplot object of the stacked barplot.
#' @examples
#' # load simulated admixture data
#' data("xadmixture")
#' 
#' # for data frame with ancestries (K) in fourth to last column,
#' # without showing bar labels
#' admix_barplot(xadmixture,
#'     K = 4:ncol(xadmixture),
#'     names = FALSE
#' )
#'
#' # grouping data by column "country",
#' # and sorting each group by ancestry column "K1"
#' admix_barplot(xadmixture,
#'     K = 4:ncol(xadmixture),
#'     grouping = "country",
#'     sortkey = "K1",
#'     names = FALSE
#' )
#'
#' # changing color palette to "turbo" from package 'viridis',
#' admix_barplot(xadmixture,
#'     K = 4:ncol(xadmixture),
#'     palette = "turbo",
#'     names = FALSE
#' )
#'
#' # removing title and changing axis labels text
#' admix_barplot(xadmixture,
#'     K = 4:ncol(xadmixture),
#'     main = "",
#'     xlab = "Accessions",
#'     ylab = "Ancestry [%]",
#'     names = FALSE
#' )
#'
#' # directly output grouped plot with clipping removed from elements
#' # (useful if there are groups with a low number of observations)
#' # create a subset of the data
#' xadmixture_sub <- admix_subset(xadmixture,
#'                               anc = c("K3", "K4"), 
#'                               pct = c(0.3, 0.2))
#' # generate a grouped & sorted stacked barplot 
#' # setting "noclip" to "TRUE" may require opening a new graphics device
#' dev.new()
#' admix_barplot(xadmixture_sub, 
#'              K = 4:ncol(xadmixture),          
#'              sortkey = "K5",
#'              grouping = "country", 
#'              palette = "viridis", 
#'              names = FALSE, 
#'              noclip = TRUE)
#' dev.off()
#' @import dplyr
#' @import forcats
#' @import ggplot2
#' @import viridis
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_sort
#' @importFrom methods hasArg
#' @importFrom rlang .data
#' @export
admix_barplot <- function(data, K = 2:ncol(data), individuals = 1, sortkey = NULL, grouping = NULL, palette = "default",
                          names = TRUE, xlab = "Individuals", ylab = "Ancestry", main = "Admixture Plot", noclip = FALSE) {
    # throw error if there are now observations
    if (nrow(data) == 0) {
        stop("No observations found!")
    }

    # throw error if ancestry columns are not of class numeric
    if (!is.numeric(as.matrix(data[, K]))) {
        stop("Please select only numeric columns!")
    }

    # rename a column to "individual"; default is first column
    names(data)[individuals] <- "individual"

    # convert to tidy format for ggplot
    data_tidy <- pivot_longer(data, K, names_to = "ancestry", values_to = "percentage")

    # sort the bars descending, using sortkey arg
    if (hasArg(sortkey)) {
        df_sortpos <- data_tidy %>%
            filter(.data$ancestry == sortkey) %>%
            arrange(desc(.data$percentage))
        data_tidy$individual <- factor(data_tidy$individual, levels = df_sortpos$individual)
    }

    # fct_relevel is used to apply str_sort(numeric = TRUE) on factors, then fct_rev to reverse the order
    str_sort_numeric <- function(x) {
        return(str_sort(x, numeric = TRUE))
    }
    plt <- ggplot(data_tidy, aes(.data$individual, .data$percentage, fill = fct_rev(fct_relevel(.data$ancestry, str_sort_numeric)))) +
        geom_col(width = 1) +
        theme_minimal() + # minimal theme to remove tick marks etc.
        labs(x = xlab, y = ylab, title = main) + # assign labels using arguments
        theme( # adjust title position and remove grid
            plot.title = element_text(hjust = 0.04, vjust = -5),
            panel.grid = element_blank()
        ) +
        # make x-axis names toggable via argument
        if (names == TRUE) {
            theme(
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            )
        } else {
            theme(
                axis.text.x = element_blank(),
            )
        }

    # change color palette
    # check first whether "palette" arg is single string to use premade palette
    if (is.character(palette) & length(palette) == 1) {
        if (palette == "viridis") {
            # viridis for colorblind support
            plt <- plt + scale_fill_viridis(discrete = TRUE, name = "")
        } else if (palette == "turbo") {
            # much improved rainbow color map
            plt <- plt + scale_fill_viridis(discrete = TRUE, name = "", option = "turbo")
        } else if (palette == "default") {
            # default with slightly increased chromaticity and reduced luminance
            plt <- plt + scale_fill_hue(c = 130, l = 55, name = "")
        } else if (palette == "alternating") {
            hexcols <- c(
                "#821E00", "#C08D00", "#AAF300", "#00D647", "#007294", "#1F007B", "#600082", "#8A007E",
                "#E35500", "#FFC115", "#C4FF3B", "#24FF6D", "#00BAF2", "#3800DF", "#A600E3", "#EA00D7"
            )
            plt <- plt + scale_fill_manual(values = hexcols, name = "")
        }
        # if no single string was provided, use "palette" arg as palette object
    } else {
        plt <- plt + scale_fill_manual(values = palette, name = "")
    }

    # group variables by grouping arg
    if (hasArg(grouping)) {
        plt <- plt + facet_grid(~ fct_inorder(data_tidy[[grouping]]), switch = "x", scales = "free", space = "free_x")
    }

    # disable clipping for all elements if argument is set to TRUE
    # might require to reload graphics device with dev.new()
    if (noclip) {
        pg <- ggplotGrob(plt)
        for (i in which(grepl("*", pg$layout$name))) {
            pg$grobs[[i]]$layout$clip <- "off"
        }
        grid::grid.draw(pg)
    } else {
        return(plt)
    }
}
