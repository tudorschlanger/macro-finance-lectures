## Save the ggplot image

save_plots <- function(filename, path) {
    extensions <- c("png", "eps")
    filename_split <- strsplit(filename, "\\.") # split at . if necessary

    for (extension in extensions){
        if (grepl(extension, filename_split, fixed = TRUE)) {
            # if extension is in the filename, do nothing
        } else if (grepl(extensions[extensions != extension], filename_split, fixed = TRUE)) {
            # search that the other extensions exist and replace it 
            idx <- grep(extensions[extensions != extension], filename_split, fixed = TRUE)
            ext_to_be_replaced <- extensions[idx]
            filename <- sub(ext_to_be_replaced, extension, filename)
        } else {
            # if not, just tag the current extension at the end
            filename <- paste(filename, ".", extension, sep = "")
        }

        ggsave(filename,
            dpi = 300,
            width = 14,
            height = 9,
            path = path
        )
        message(paste("Saved ", filename, " to ", path, " folder."))
    }

}
