library(Rtesseract)

if(FALSE) {
    p = unlist(lapply(list.files(pattern = "Screen Shot 2021-11-05"), participants))
    p = p[nchar(p) > 1]
    unique(p)
}

participants =
function(f = "Screen Shot 2021-11-05 at 10.32.46 AM.png", a = tesseract(f),  bb = GetBoxes(a),
         start = max(min(bb$left[grep("Duncan", bb$text)]), 80))
{
       # Find the right-most part of the text before the muted microphone and camera
    i = grep("\\(his|hers|Host|Guest\\)", bb$text)

    end = max(bb$left[i+1]) - 2L

    # Find the start of the text after the images/icons.
    if(start == Inf)
        start = min(bb$left[grep("^[A-Z].+", bb$text)]) - 2L

    nms = bb[bb$left >=  start - 10 & bb$right < end - 10, ]

    # arrange by line 
    v = sort(nms$bottom)
    els = split(nms, cut(nms$bottom, c(0, v[diff(v) > 10], Inf)))


    # Glue elements on each line into a string.
    who = unname(sapply(els, function(x) paste(x$text[order(x$left)], collapse =  " ")))

    # Clean up the strings.
    who = who [ who != "" ]
    # remove content in (  ) , e.g., Guest, pronouns.
    who = gsub("\\(.*\\)", "", who)
    # remove white space at start and end
    who = trimws(who)
    # Fix smart apostrophe being backward.
    gsub("‘([A-Z])", "'\\1", who)
}
