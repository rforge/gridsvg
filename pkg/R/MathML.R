
indentInc <- "  "

incind <- function(indent) {
    paste(indent, indentInc, sep="")
}

explicitMathVariant <- function(fontfamily) {
    currentFonts <- getSVGFonts()
    stackname <- fontStackFromFontFamily(fontfamily, currentFonts)
    switch(stackname,
           sans="sans-serif",
           serif="normal",
           mono="monospace",
           "sans-serif")
}

mmlJuxta <- function(e, indent, fontfamily) {
    do.call("paste", c(lapply(e[-1], toMML, indent, fontfamily), list(sep="")))
}

mmlBinOp <- function(e, indent, fontfamily, op) {
    paste(toMML(e[[2]], indent, fontfamily),
          indent, "<mo>", op, "</mo>\n",
          toMML(e[[3]], indent, fontfamily),
          sep="")
}

mmlParen <- function(e, indent, fontfamily) {
    paste(indent, "<mfenced><mrow>\n",
          toMML(e[[2]], incind(indent), fontfamily),
          indent, "</mrow></mfenced>\n",
          sep="")
}

mmlSup <- function(e, indent, fontfamily) {
    paste(indent, "<msup>\n",
          toMML(e[[2]], incind(indent), fontfamily),
          toMML(e[[3]], incind(indent), fontfamily),
          indent, "</msup>\n",
          sep="")
}

mmlSub <- function(e, indent, fontfamily) {
    paste(indent, "<msub>\n",
          toMML(e[[2]], incind(indent), fontfamily),
          toMML(e[[3]], incind(indent), fontfamily),
          indent, "</msub>\n",
          sep="")
}

mmlSqrt <- function(e, indent, fontfamily) {
    if (length(e) > 2) {
        paste(indent, "<mroot>\n",
              toMML(e[[2]], incind(indent), fontfamily),
              toMML(e[[3]], incind(indent), fontfamily),
              indent, "</mroot>\n",
              sep="")
    } else {
        paste(indent, "<msqrt>\n",
              toMML(e[[2]], incind(indent), fontfamily),
              indent, "</msqrt>\n",
              sep="")
    }
}

toMML <- function(x, indent, ...) {
    UseMethod("toMML")
}

toMML.numeric <- function(x, indent, ...) {
    paste(indent, "<mn>", as.character(x), "</mn>\n", sep="")
}

toMML.character <- function(x, indent, fontfamily, ...) {
    paste(indent, '<mtext mathvariant="', explicitMathVariant(fontfamily),
          '">', x, "</mtext>\n", sep="")
}

toMML.name <- function(x, indent, fontfamily, ...) {
    # R does NOT automatically italicize symbols
    paste(indent, '<mtext mathvariant="', explicitMathVariant(fontfamily),
          '">', x, "</mtext>\n", sep="")
}

# A "language" object may have class "call" or "(" 
"toMML.(" <- function(x, ...) {
    toMML.call(x, ...)
}

toMML.call <- function(x, indent, fontfamily, ...) {
    op <- as.character(x[[1]])
    switch(op,
           "+"=,
           "/"=mmlBinOp(x, indent, fontfamily, op),
           "-"=mmlBinOp(x, indent, fontfamily, "&#x2212;"),
           "*"=mmlBinOp(x, indent, fontfamily, "&#x2062;"),
           "%+-%"=mmlBinOp(x, indent, fontfamily, "&#x00B1;"),
           "%/%"=mmlBinOp(x, indent, fontfamily, "&#x00F7;"),
           "%*%"=mmlBinOp(x, indent, fontfamily, "&#x00D7;"),
           "%.%"=mmlBinOp(x, indent, fontfamily, "&#x22C5;"),
           "["=mmlSub(x, indent, fontfamily),
           "^"=mmlSup(x, indent, fontfamily),
           "paste"=mmlJuxta(x, indent, fontfamily),
           "sqrt"=mmlSqrt(x, indent, fontfamily),
           "("=mmlParen(x, indent, fontfamily),
           stop("Help! I can't cope!"))
}

# fontfamily is used to set explicit 'mathvariant' when it is not
# implicit in the formula element
expr2mml <- function(e, fontfamily) {
    bits <- lapply(e, toMML, indent="    ", fontfamily=fontfamily)
    paste("<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n",
          paste(sapply(bits,
                       function(x) paste("  <mrow>\n", x, "  </mrow>\n",
                                         sep="")),
                collapse="\n"),
          "</math>\n", sep="")
}

