## Write to Excel ################################################
##
##  tab is a list of data frames to be written each in a reparate worksheet
##
writeTables2Excel <- function(tab, fname, rownames=FALSE)
{
    fname <- paste(fname, ".xlsx", sep="")
    if(file.exists(fname))
        file.remove(fname)

    wb  <- loadWorkbook(fname, create=TRUE)

    cs <- createCellStyle(wb)       # Create a custom anonymous cell style
    setWrapText(cs, wrap=FALSE)     # Specify not to wrap the text

    for(xname in names(tab))
    {
        df <- tab[[xname]]
        if(rownames)
            df <- cbind.data.frame(rownames=rownames(df), df)
        createSheet(wb, name=xname)
        writeWorksheet(wb, df, sheet=xname)

        rowIndex <- 1:nrow(df)
        colIndex <- 1:ncol(df)
##        setCellStyle(wb, sheet=xname, row=rowIndex, col=colIndex, cellstyle=cs)
        setColumnWidth(wb, sheet=xname, column=colIndex, width=-1)
    }
    saveWorkbook(wb)
}
