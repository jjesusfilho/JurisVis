#' Creates a ggplot funnel based on a decreasing numeric vector
#'
#' @param x Decreasing numeric vector
#' @param text Text vector to annotate the funnel.
#' @param color Vector of colors
#'
#' @return ggplot funnel object
#' @details  This function needs improvement. I made it ad doc.
#' @export
#'
gg_funnel <- function(x, text = NULL, color = NULL){


  #### type safety ####
  if (!is.numeric(x)){

    stop("x must be a numeric vector")
  }

  if(rlang::is_scalar_atomic(x)){

    stop("The length of x must be bigger than 1")

  }

  if (any(x<0)){

    stop("This function doesn't accept negative values")

  }

 if(any(duplicated(x))){

   stop("There are duplicated numbers. Remove them.")

 }

  x <- sort(x, decreasing =  T)


  if (is.null(color)){

    color <- colorspace::qualitative_hcl(length(x), palette = "Dark 3")

  }

  if (is.null(text)){

    text <- as.character(x)

  }


  if(!all.equal(length(x), length(text), length(color))){

    stop("x, text, and color must have the same length")

  }

  #### Create x coordinates ####

  l1 <- vector("list", length(x))

  for (i in 1:length(x)){

    if (i == 1){



      x3 <- x[1]

      x4 <- 0

      x1 <- seq(x4, x3, length.out = 6)[2]

      x2 <- seq(x4, x3, length.out = 6)[5]


    } else {

      x4 <- l1[[i-1]][1]

      x3 <- l1[[i-1]][2]



      x1 <- seq(x4, x3, length.out = 6)[2]

      x2 <- seq(x4, x3, length.out = 6)[5]




    }

    l1[[i]] <- c(x1,x2, x3, x4)
  }


  #### Create y coordinates ####

  l2 <- purrr::map(length(x):1, ~{

    c(.x*5-5, .x*5-5,.x*5, .x*5)


  })


  #### Creates data.frame based on x and y coordinates. ####


  dfs <- purrr::map2(l1, l2,~{

    df <- data.frame(
      x = .x,
      y = .y
    )

  }
  )



  ### Creates plots one by one and pile them.


  p <- ggplot2::ggplot()

  for (i in 1:length(dfs)){

    p <-  p +
      ggplot2::geom_polygon(data = dfs[[i]], ggplot2::aes(x = x, y = y), fill = color[i])+
      ggplot2::geom_text()+
      ggplot2::annotate("text",label = text[i], x  = x[1]/2,
                        y  = mean(dfs[[i]]$y),
                        size = log2(x[i])/ggplot2::.pt)+
      ggplot2::theme_minimal()+
      ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_blank(),
                     axis.ticks.x=ggplot2::element_blank(),
                     axis.title.y=ggplot2::element_blank(),
                     axis.text.y=ggplot2::element_blank(),
                     axis.ticks.y=ggplot2::element_blank()
      )

  }

  p

}

