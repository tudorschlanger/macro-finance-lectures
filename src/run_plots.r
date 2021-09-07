# Source all the plotting scripts
# The user has to input a vector/list of numbers associated with the lecture notes they would like to reproduce

run_plots <- function(list_lecture_nr, data_vintage) {

    list_lecture_nr <- as.character(list_lecture_nr)

    for (lecture_nr in list_lecture_nr) {

        str <- sprintf("lectures/lec%s/code/plot_all.r", lecture_nr)
        path_to_script <- here(str)
        source(path_to_script)
        do.call(plot_all, list(lecture_nr, data_vintage))
    }

}