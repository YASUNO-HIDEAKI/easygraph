#' @title create line graph
#' @description \code{f.graph} linegraph
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @import ggplot2
#' @importFrom plotrix std.error
#'
#' @param data use data
#' @param time experiment period
#' @param tre treatment
#' @param meth measurement
#' @param ylab y label name
#'
#' @return graph
#'
#'@export
#'
f.graph <- function(data, time, tre, meth) {
        ##Data.setting process
        graph.sum <- data.frame()

        for (i in time) {
                graph <-
                        data %>%
                        dplyr::group_by(Time) %>%
                        dplyr::filter(Time %in% i) %>%
                        dplyr::group_by(Treatment)

                if(missing(tre)) {

                }else{
                        graph <-
                                graph %>%
                                dplyr::group_by(Treatment) %>%
                                dplyr::filter(Treatment %in% tre)
                }

                graph2 <-
                        graph %>%
                        dplyr::summarise(mean = mean({{meth}}, na.rm = T),
                                         se = std.error({{meth}}, na.rm = T),
                                         Time = i)

                graph.sum <-
                        rbind(graph.sum, graph2)
        }

        ##plot process
        graph <-
                graph.sum %>%
                ggplot(., aes(x = Time, y = mean,
                              color = Treatment,
                              shape = Treatment)) +
                geom_line(aes(linetype = Treatment), size = 1.5) +
                geom_point(aes(shape = Treatment),
                           size = 5) +
                geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                              width = 2,
                              size = 1,
                              color = "black") +
                theme_classic(base_size = 24) +
                labs(x = "Day") +
                theme(axis.title = element_text(size = 24),
                      axis.line = element_line(size = 1),
                      axis.text = element_text(size = 24, color = "black"),
                      plot.margin = unit(c(1,1,0.5,0.5), "cm"),
                      legend.title = element_text(size = 16))

        ##output process
        print(graph)
        graph <<- graph
}
