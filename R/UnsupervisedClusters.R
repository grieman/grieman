#' UnsupervisedClusters
#'
#' This function performs unsupervised clustering on a dataset. This technique can be useful to detect hidden trends in the data, as well as outliers and odd behaviors.
#' Works best with smaller datasets - more than a few thousand can slow greatly and be less useful
#'
#' @param datain a dataframe which will be clustered
#' @param plottype one of the following: "dendogram", "fan", "radial","unrooted","cladogram"
#' @param forceclusters allows for specification of number of clusters. If NULL, the best number will be calculated.
#' @param classification optional - a vector with true class values. If given, a confusion matrix will be output
#'
#' @return datain, but with an added column with clusters
#' @export
#'
#' @examples
#' set.seed(1)
#' datain <- data.frame(matrix(rnorm(400), nrow=100))
#'
#' dataout <- UnsupervisedClusters(datain)
#'
#' dataout$index <- 1:length(dataout[,1])
#' graphme <- reshape::melt(dataout, id=c("index", "cluster"))
#' ggplot2::ggplot(graphme, ggplot2::aes(x=index, y=value, color=cluster)) +
#'  ggplot2::geom_point() + ggplot2::facet_wrap(~variable) + theme_GR()


UnsupervisedClusters <-
  function(datain,
           plottype = "fan",
           forceclusters = NULL,
           classification = NULL) {
    if (plottype %in% c("dendogram", "fan", "radial", "unrooted", "cladogram") == FALSE) {
      stop("Not a valid plottype")
    }

    colors = c(
      "#004990",
      "#a7a9ac",
      "#00AFD5",
      "#cddc38",
      "#bed3e4",
      "#000000",
      "#c6dbd8",
      "#246987",
      "#768d99",
      "blue"
    )

    #### CLUSTER ANALYSIS ####
    #finds the best number of clusters. Centroid method is used to better handle outliers
    if (is.null(forceclusters)) {
      clustnums <- max.col(t(table(
        NbClust::NbClust(
          datain,
          distance = "euclidean",
          min.nc = 2,
          max.nc = 10,
          method = "centroid",
          index = "all"
        )$Best.nc[1, ]
      )),
      "last")
    } else {
      clustnums <- forceclusters
    }


    res <- factoextra::hcut(datain, k = clustnums, stand = TRUE)

    graphics::par(mfrow = c(1, 1))

    #### PLOTS ####
    if (plottype == "dendogram") {
      factoextra::fviz_dend(res,
                            rect = TRUE,
                            cex = 0.5,
                            k_colors = colors[1:clustnums])
    } else {
      clus = stats::cutree(res, clustnums)
      graphics::plot(
        ape::as.phylo(res),
        type = plottype,
        tip.color = colors[clus],
        label.offset = .1,
        cex = 0.7
      )
    }

    #### CLASSIFICATION INFO ####
    if (is.null(classification) == 0) {
      if (length(classification) != length(datain[, 1])) {
        stop("classification vector must be the same length as the observations")
      }
      tabl <- table(observed = res$cluster, expected = classification)
      print(tabl)
    }

    datain$cluster <- as.factor(res$cluster)
    return(datain)
  }
