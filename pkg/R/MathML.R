
explicitMathVariant <- function(fontfamily, fontface) {
    currentFonts <- getSVGFonts()
    stackname <- fontStackFromFontFamily(fontfamily, currentFonts)
    switch(stackname,
           sans=switch(fontface,
                       plain="sans-serif",
                       bold="bold-sans-serif",
                       italic=,
                       oblique="sans-serif-italic",
                       bold.italic="sans-serif-bold-italic"),
           serif=switch(fontface,
                        plain="normal",
                        bold="bold",
                        italic=,
                        oblique="italic",
                        bold.italic="bold-italic"),
           mono="monospace",
           switch(fontface,
                  plain="sans-serif",
                  bold="bold-sans-serif",
                  italic=,
                  oblique="sans-serif-italic",
                  bold.italic="sans-serif-bold-italic"))
}

unicode <-
c("&#x0020;", "&#x0021;", "&#x2200;", "&#x0023;", "&#x2203;", 
"&#x0025;", "&#x0026;", "&#x220B;", "&#x0028;", "&#x0029;", "&#x2217;", 
"&#x002B;", "&#x002C;", "&#x2212;", "&#x002E;", "&#x002F;", "&#x0030;", 
"&#x0031;", "&#x0032;", "&#x0033;", "&#x0034;", "&#x0035;", "&#x0036;", 
"&#x0037;", "&#x0038;", "&#x0039;", "&#x003A;", "&#x003B;", "&#x003C;", 
"&#x003D;", "&#x003E;", "&#x003F;", "&#x2245;", "&#x0391;", "&#x0392;", 
"&#x03A7;", "&#x0394;", "&#x0395;", "&#x03A6;", "&#x0393;", "&#x0397;", 
"&#x0399;", "&#x03D1;", "&#x039A;", "&#x039B;", "&#x039C;", "&#x039D;", 
"&#x039F;", "&#x03A0;", "&#x0398;", "&#x03A1;", "&#x03A3;", "&#x03A4;", 
"&#x03A5;", "&#x03C2;", "&#x03A9;", "&#x039E;", "&#x03A8;", "&#x0396;", 
"&#x005B;", "&#x2234;", "&#x005D;", "&#x22A5;", "&#x005F;", "&#xF8E5;", 
"&#x03B1;", "&#x03B2;", "&#x03C7;", "&#x03B4;", "&#x03B5;", "&#x03C6;", 
"&#x03B3;", "&#x03B7;", "&#x03B9;", "&#x03D5;", "&#x03BA;", "&#x03BB;", 
"&#x03BC;", "&#x03BD;", "&#x03BF;", "&#x03C0;", "&#x03B8;", "&#x03C1;", 
"&#x03C3;", "&#x03C4;", "&#x03C5;", "&#x03D6;", "&#x03C9;", "&#x03BE;", 
"&#x03C8;", "&#x03B6;", "&#x007B;", "&#x007C;", "&#x007D;", "&#x223C;", 
"&#x20AC;", "&#x03D2;", "&#x2032;", "&#x2264;", "&#x2044;", "&#x221E;", 
"&#x0192;", "&#x2663;", "&#x2666;", "&#x2665;", "&#x2660;", "&#x2194;", 
"&#x2190;", "&#x2191;", "&#x2192;", "&#x2193;", "&#x00B0;", "&#x00B1;", 
"&#x2033;", "&#x2265;", "&#x00D7;", "&#x221D;", "&#x2202;", "&#x2022;", 
"&#x00F7;", "&#x2260;", "&#x2261;", "&#x2248;", "&#x2026;", "&#xF8E6;", 
"&#xF8E7;", "&#x21B5;", "&#x2135;", "&#x2111;", "&#x211C;", "&#x2118;", 
"&#x2297;", "&#x2295;", "&#x2205;", "&#x2229;", "&#x222A;", "&#x2283;", 
"&#x2287;", "&#x2284;", "&#x2282;", "&#x2286;", "&#x2208;", "&#x2209;", 
"&#x2220;", "&#x2207;", "&#xF6DA;", "&#xF6D9;", "&#xF6DB;", "&#x220F;", 
"&#x221A;", "&#x22C5;", "&#x00AC;", "&#x2227;", "&#x2228;", "&#x21D4;", 
"&#x21D0;", "&#x21D1;", "&#x21D2;", "&#x21D3;", "&#x25CA;", "&#x2329;", 
"&#xF8E8;", "&#xF8E9;", "&#xF8EA;", "&#x2211;", "&#xF8EB;", "&#xF8EC;", 
"&#xF8ED;", "&#xF8EE;", "&#xF8EF;", "&#xF8F0;", "&#xF8F1;", "&#xF8F2;", 
"&#xF8F3;", "&#xF8F4;", "&#x232A;", "&#x222B;", "&#x2320;", "&#xF8F5;", 
"&#x2321;", "&#xF8F6;", "&#xF8F7;", "&#xF8F8;", "&#xF8F9;", "&#xF8FA;", 
"&#xF8FB;", "&#xF8FC;", "&#xF8FD;", "&#xF8FE;")

# See ~/Research/Rstuff/SVG/PlotMath/greek.R
greek <-
structure(c("&#x03B1;", "&#x03B2;", "&#x03B3;", "&#x03B4;", "&#x03B5;", 
"&#x03B6;", "&#x03B7;", "&#x03B8;", "&#x03B9;", "&#x03BA;", "&#x03BB;", 
"&#x03BC;", "&#x03BD;", "&#x03BE;", "&#x03BF;", "&#x03C0;", "&#x03C1;", 
"&#x03C2;", "&#x03C3;", "&#x03C4;", "&#x03C5;", "&#x03D5;", "&#x03C7;", 
"&#x03C8;", "&#x03C9;", "&#x0391;", "&#x0392;", "&#x0393;", "&#x0394;", 
"&#x0395;", "&#x0396;", "&#x0397;", "&#x0398;", "&#x0399;", "&#x039A;", 
"&#x039B;", "&#x039C;", "&#x039D;", "&#x039E;", "&#x039F;", "&#x03A0;", 
"&#x03A1;", "&#x03A2;", "&#x03A3;", "&#x03A4;", "&#x03A5;", "&#x03A6;", 
"&#x03A7;", "&#x03A8;", "&#x03A9;"), .Names = c("alpha", "beta", 
"gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", 
"kappa", "lambda", "mu", "nu", "xi", "omicron", "pi", "rho", 
"", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega", 
"Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta", "Eta", 
"Theta", "Iota", "Kappa", "Lambda", "Mu", "Nu", "Xi", "Omicron", 
"Pi", "Rho", "", "Sigma", "Tau", "Upsilon", "Phi", "Chi", "Psi", 
"Omega"))

symbolNames <-
    c("..."="&#x2026;",
      cdots="&#x22EF;",
      ldots="&#x2026;",
      greek,
      theta1="&#x03D1;",
      vartheta="&#x03D1;",
      phi1="&#x03C6;",
      sigma1="&#x03C2;",
      varsigma="&#x03C2;",
      omega1="&#x03D6;",
      Upsilon1="&#x03D2;",
      aleph="&#x05D0;",
      infinity="&#x221E;",
      partialdiff="&#x2202;",
      nabla="&#x2207;",
      degree="&#x00B0;",
      minute="&#x2032;",
      second="&#x2033;")

# The general idea with each of these mml*() functions is to
# create a single MathML element.
# This means that, if the output is a collection of several
# elements, we wrap the whole collection in an <mrow>
mmlJuxta <- function(e, fontfamily, fontface, svgdev) {
    mrow <- newXMLNode("mrow", parent = svgDevParent(svgdev))
    svgDevChangeParent(mrow, svgdev)

    e <- e[-1]
    lapply(e, function(x) {
        toMML(x, fontfamily, fontface, svgdev)
    })

    svgDevChangeParent(xmlParent(mrow), svgdev)
}

mmlBinOp <- function(e, fontfamily, fontface, op, svgdev) {
    mrow <- newXMLNode("mrow", parent = svgDevParent(svgdev))
    svgDevChangeParent(mrow, svgdev)

    toMML(e[[2]], fontfamily, fontface, svgdev)
    newXMLNode("mo", parent = svgDevParent(svgdev),
               newXMLTextNode(I(op)))
    toMML(e[[3]], fontfamily, fontface, svgdev)

    svgDevChangeParent(xmlParent(mrow), svgdev)
}

mmlParen <- function(e, fontfamily, fontface, svgdev) {
    mfenced <- newXMLNode("mfenced", parent = svgDevParent(svgdev))
    mrow <- newXMLNode("mrow", parent = mfenced)
    svgDevChangeParent(mrow, svgdev)

    toMML(e[[2]], fontfamily, fontface, svgdev)

    svgDevChangeParent(xmlParent(mfenced), svgdev)
}

mmlBrace <- function(e, fontfamily, fontface, svgdev) {
    mfenced <- newXMLNode("mfenced", parent = svgDevParent(svgdev),
                          attrs = list(open = "", close = ""))
    mrow <- newXMLNode("mrow", parent = mfenced)    
    svgDevChangeParent(mrow, svgdev)

    toMML(e[[2]], fontfamily, fontface, svgdev)

    svgDevChangeParent(xmlParent(mfenced), svgdev)
}

delimiters <- c(lfloor="&#x230A;",
           rfloor="&#x230B;",
           lceil="&#x2308;",
           rceil="&#x2309;")

convertDelim <- function(delim) {
    if (delim %in% names(delimiters))
        delimiters[delim]
    else
        delim
}

mmlGroup <- function(e, fontfamily, fontface, svgdev) {
    # e[[2]] and e[[4]] are the delimiters
    if (length(e) < 4)
        stop("Invalid plotmath group()")
    delim1 <- convertDelim(as.character(e[[2]]))
    delim2 <- convertDelim(as.character(e[[4]]))

    mfenced <- newXMLNode("mfenced", parent = svgDevParent(svgdev),
                          attrs = list(open = delim1, close = delim2))
    mrow <- newXMLNode("mrow", parent = mfenced)    
    svgDevChangeParent(mrow, svgdev)

    toMML(e[[3]], fontfamily, fontface, svgdev)

    svgDevChangeParent(xmlParent(mfenced), svgdev)
}

mmlSup <- function(e, fontfamily, fontface, svgdev) {
    msup <- newXMLNode("msup", parent = svgDevParent(svgdev))
    svgDevChangeParent(msup, svgdev)

    toMML(e[[2]], fontfamily, fontface, svgdev)
    toMML(e[[3]], fontfamily, fontface, svgdev)

    svgDevChangeParent(xmlParent(msup), svgdev)
}

mmlSub <- function(e, fontfamily, fontface, svgdev) {
    msub <- newXMLNode("msub", parent = svgDevParent(svgdev))
    svgDevChangeParent(msub, svgdev)

    toMML(e[[2]], fontfamily, fontface, svgdev)
    toMML(e[[3]], fontfamily, fontface, svgdev)

    svgDevChangeParent(xmlParent(msub), svgdev)
}

mmlSqrt <- function(e, fontfamily, fontface, svgdev) {
    if (length(e) > 2) {
        mroot <- newXMLNode("mroot", parent = svgDevParent(svgdev))
        svgDevChangeParent(mroot, svgdev)

        toMML(e[[2]], fontfamily, fontface, svgdev)
        toMML(e[[3]], fontfamily, fontface, svgdev)

        svgDevChangeParent(xmlParent(mroot), svgdev)
    } else {
        msqrt <- newXMLNode("msqrt", parent = svgDevParent(svgdev))
        svgDevChangeParent(msqrt, svgdev)

        toMML(e[[2]], fontfamily, fontface, svgdev)

        svgDevChangeParent(xmlParent(msqrt), svgdev)
    }
}

mmlFont <- function(e, fontfamily, fontface, svgdev) {
    toMML(e[[2]], fontfamily, fontface, svgdev)
}

mmlStyle <- function(e, fontfamily, fontface, style, svgdev) {
    displaystyle <- switch(style,
                           display="true",
                           "false")
    scriptlevel <- switch(style,
                          display=0,
                          text=0,
                          script=1,
                          scriptscript=2)

    mstyle <- newXMLNode("mstyle", parent = svgDevParent(svgdev),
                         attrs = list(displaystyle = displaystyle,
                                      scriptlevel = scriptlevel))
    svgDevChangeParent(mstyle, svgdev)

    toMML(e[[2]], fontfamily, fontface, svgdev)

    svgDevChangeParent(xmlParent(mstyle), svgdev)
}

mmlSymbol <- function(e, fontfamily, fontface, svgdev) {
    newXMLNode("mtext", parent = svgDevParent(svgdev),
               attrs = list(mathvariant =
                   explicitMathVariant(fontfamily, fontface)),
               newXMLTextNode(I(unicode[as.integer(charToRaw(e[[2]])) - 31])))
}

mmlCSL <- function(e, fontfamily, fontface, svgdev) {
    mfenced <- newXMLNode("mfenced", parent = svgDevParent(svgdev),
                         attrs = list(open = "", close = ""))
    svgDevChangeParent(mfenced, svgdev)

    sapply(e[-1], toMML, fontfamily, fontface, svgdev)

    svgDevChangeParent(xmlParent(mfenced), svgdev)
}

mmlAccent <- function(e, fontfamily, fontface, accent, svgdev) {
    mover <- newXMLNode("mover", parent = svgDevParent(svgdev),
                         attrs = list(accent = "true",
                                      align = "center"))
    mrow <- newXMLNode("mrow", parent = mover)
    svgDevChangeParent(mrow, svgdev)

    toMML(e[[2]], fontfamily, fontface, svgdev)
    newXMLNode("mo", parent = mover,
               attrs = list(stretchy = "false"),
               newXMLTextNode(I(accent)))

    svgDevChangeParent(xmlParent(mover), svgdev)
}

mmlWideAccent <- function(e, fontfamily, fontface, accent, svgdev) {
    mover <- newXMLNode("mover", parent = svgDevParent(svgdev),
                         attrs = list(accent = "true",
                                      align = "center"))
    mrow <- newXMLNode("mrow", parent = mover)
    svgDevChangeParent(mrow, svgdev)

    toMML(e[[2]], fontfamily, fontface, svgdev)
    newXMLNode("mo", parent = mover,
               attrs = list(stretchy = "true"),
               newXMLTextNode(I(accent)))

    svgDevChangeParent(xmlParent(mover), svgdev)
}

mmlUnderline <- function(e, fontfamily, fontface, svgdev) {
    # NOTE: <mstack> and <msline> are currently not supported
    #       by Mozilla-based browsers (2011-11-21)
    munder <- newXMLNode("munder", parent = svgDevParent(svgdev))
    mrow <- newXMLNode("mrow", parent = munder)
    svgDevChangeParent(mrow, svgdev)

    toMML(e[[2]], fontfamily, fontface, svgdev)
    newXMLNode("mo", parent = munder,
               attrs = list(stretchy = "true"),
               newXMLTextNode(I("&#x00AF;")))

    svgDevChangeParent(xmlParent(munder), svgdev)
}

mmlSpace <- function(e, fontfamily, fontface, svgdev) {
    mrow <- newXMLNode("mrow", parent = svgDevParent(svgdev))
    svgDevChangeParent(mrow, svgdev)

    if (length(e) > 2) {
        toMML(e[[2]], fontfamily, fontface, svgdev)
        newXMLNode("mtext", parent = svgDevParent(svgdev),
                   attrs = list(mathvariant =
                                explicitMathVariant(fontfamily, fontface)),
                   newXMLTextNode(I("&#x00A0;")))
        toMML(e[[3]], fontfamily, fontface, svgdev)
    } else {
        newXMLNode("mtext", parent = svgDevParent(svgdev),
                   attrs = list(mathvariant =
                                explicitMathVariant(fontfamily, fontface)),
                   newXMLTextNode(I("&#x00A0;")))
        toMML(e[[2]], fontfamily, fontface, svgdev)
    }

    svgDevChangeParent(xmlParent(mrow), svgdev)
}

mmlPhantom <- function(e, fontfamily, fontface, svgdev) {
    mphantom <- newXMLNode("mphantom", parent = svgDevParent(svgdev))
    svgDevChangeParent(mphantom, svgdev)

    toMML(e[[2]], fontfamily, fontface, svgdev)

    svgDevChangeParent(xmlParent(mphantom), svgdev)
}

mmlFrac <- function(e, fontfamily, fontface, svgdev, lwd="medium") {
    mfrac <- newXMLNode("mfrac", parent = svgDevParent(svgdev),
                        attrs = list(linethickness = lwd))
    svgDevChangeParent(mfrac, svgdev)

    toMML(e[[2]], fontfamily, fontface, svgdev)
    toMML(e[[3]], fontfamily, fontface, svgdev)

    svgDevChangeParent(xmlParent(mfrac), svgdev)
}

mmlBigOp <- function(e, fontfamily, fontface, svgdev, op=NULL) {
    mrow <- newXMLNode("mrow", parent = svgDevParent(svgdev))

    # When checking for is.null(op),
    # Either specify op special character or format the first
    # element of the expression
    if (length(e) < 3) {
        if (is.null(op)) {
            opmrow <- newXMLNode("mrow", parent = mrow)
            svgDevChangeParent(opmrow, svgdev)
            toMML(e[[1]], fontfamily, fontface, svgdev)
            svgDevChangeParent(xmlParent(opmrow), svgdev)
            newXMLNode("mtext", parent = opmrow,
                       attrs = list(mathvariant =
                           explicitMathVariant(fontfamily, fontface)),
                       newXMLTextNode(I("&#x00A0;")))
        } else {
            newXMLNode("mo", parent = mrow,
                       newXMLTextNode(I(op)))
        }

        svgDevChangeParent(mrow, svgdev)
        toMML(e[[2]], fontfamily, fontface, svgdev)
    } else if (length(e) < 4) {
        munder <- newXMLNode("munder", parent = mrow)

        if (is.null(op)) {
            opmrow <- newXMLNode("mrow", parent = munder)
            svgDevChangeParent(opmrow, svgdev)
            toMML(e[[1]], fontfamily, fontface, svgdev)
            svgDevChangeParent(xmlParent(opmrow), svgdev)
            newXMLNode("mtext", parent = opmrow,
                       attrs = list(mathvariant =
                           explicitMathVariant(fontfamily, fontface)),
                       newXMLTextNode(I("&#x00A0;")))
        } else {
            newXMLNode("mo", parent = munder,
                       newXMLTextNode(I(op)))
        }

        svgDevChangeParent(munder, svgdev)
        toMML(e[[3]], fontfamily, fontface, svgdev)
        svgDevChangeParent(xmlParent(munder), svgdev)
        toMML(e[[2]], fontfamily, fontface, svgdev)
    } else {
        munderover <- newXMLNode("munderover", parent = mrow)

        if (is.null(op)) {
            opmrow <- newXMLNode("mrow", parent = munderover)
            svgDevChangeParent(opmrow, svgdev)
            toMML(e[[1]], fontfamily, fontface, svgdev)
            svgDevChangeParent(xmlParent(opmrow), svgdev)
            newXMLNode("mtext", parent = opmrow,
                       attrs = list(mathvariant =
                           explicitMathVariant(fontfamily, fontface)),
                       newXMLTextNode(I("&#x00A0;")))
        } else {
            newXMLNode("mo", parent = munderover,
                       newXMLTextNode(I(op)))
        }

        svgDevChangeParent(munderover, svgdev)
        toMML(e[[3]], fontfamily, fontface, svgdev)
        toMML(e[[4]], fontfamily, fontface, svgdev)
        svgDevChangeParent(xmlParent(munderover), svgdev)
        toMML(e[[2]], fontfamily, fontface, svgdev)
    }

    svgDevChangeParent(xmlParent(mrow), svgdev)
}

mmlFun <- function(e, fontfamily, fontface, svgdev) {
    mrow <- newXMLNode("mrow", parent = svgDevParent(svgdev))
    mtext <- newXMLNode("mtext", parent = mrow,
                        attrs = list(mathvariant = explicitMathVariant(fontfamily, fontface)),
                        newXMLTextNode(I(e[[1]])))
    mfenced <- newXMLNode("mfenced", parent = mrow)
    svgDevChangeParent(mfenced, svgdev)

    toMML(e[[2]], fontfamily, fontface, svgdev)

    svgDevChangeParent(xmlParent(mrow), svgdev)
}

toMML <- function(x, fontfamily, fontface, svgdev, ...) {
    UseMethod("toMML")
}

toMML.numeric <- function(x, fontfamily, fontface, svgdev, ...) {
    newXMLNode("mn", parent = svgDevParent(svgdev),
               newXMLTextNode(as.character(x)))
}

toMML.character <- function(x, fontfamily, fontface, svgdev, ...) {
    newXMLNode("mtext", parent = svgDevParent(svgdev),
               attrs = list(mathvariant =
                   explicitMathVariant(fontfamily, fontface)),
               newXMLTextNode(x))
}

toMML.name <- function(x, fontfamily, fontface, svgdev, ...) {
    # Convert special names
    if (as.character(x) %in% names(symbolNames))
        x <- symbolNames[as.character(x)]
    # R does NOT automatically italicize symbols
    newXMLNode("mtext", parent = svgDevParent(svgdev),
               attrs = list(mathvariant =
                   explicitMathVariant(fontfamily, fontface)),
               newXMLTextNode(x))
}

# A "language" object may have class "call" or "(" or "{"
"toMML.(" <- function(x, fontfamily, fontface, svgdev, ...) {
    toMML.call(x, fontfamily, fontface, svgdev, ...)
}

"toMML.{" <- function(x, fontfamily, fontface, svgdev, ...) {
    toMML.call(x, fontfamily, fontface, svgdev, ...)
}

funCallToMML <- function(x, fontfamily, fontface, svgdev) {
    funName <- as.character(x[[1]])
    switch(funName,
           "+"=,
           "/"=mmlBinOp(x, fontfamily, fontface, funName, svgdev),
           "-"=mmlBinOp(x, fontfamily, fontface, "&#x2212;", svgdev),
           "*"=mmlBinOp(x, fontfamily, fontface, "&#x2062;", svgdev),
           "%+-%"=mmlBinOp(x, fontfamily, fontface, "&#x00B1;", svgdev),
           "%/%"=mmlBinOp(x, fontfamily, fontface, "&#x00F7;", svgdev),
           "%*%"=mmlBinOp(x, fontfamily, fontface, "&#x00D7;", svgdev),
           "%.%"=mmlBinOp(x, fontfamily, fontface, "&#x22C5;", svgdev),
           "["=mmlSub(x, fontfamily, fontface, svgdev),
           "^"=mmlSup(x, fontfamily, fontface, svgdev),
           "paste"=mmlJuxta(x, fontfamily, fontface, svgdev),
           "sqrt"=mmlSqrt(x, fontfamily, fontface, svgdev),
           "("=mmlParen(x, fontfamily, fontface, svgdev),
           "{"=mmlBrace(x, fontfamily, fontface, svgdev),
           "=="=mmlBinOp(x, fontfamily, fontface, "=", svgdev),
           "!="=mmlBinOp(x, fontfamily, fontface, "&#x2260;", svgdev),
           "<"=mmlBinOp(x, fontfamily, fontface, "&lt;", svgdev),
           "<="=mmlBinOp(x, fontfamily, fontface, "&#x2264;", svgdev),
           ">"=mmlBinOp(x, fontfamily, fontface, "&gt;", svgdev),
           ">="=mmlBinOp(x, fontfamily, fontface, "&#x2265;", svgdev),
           "%~~%"=mmlBinOp(x, fontfamily, fontface, "&#x2248;", svgdev),
           "%=~%"=mmlBinOp(x, fontfamily, fontface, "&#x2245;", svgdev),
           "%==%"=mmlBinOp(x, fontfamily, fontface, "&#x2261;", svgdev),
           "%prop%"=mmlBinOp(x, fontfamily, fontface, "&#x221D;", svgdev),
           "plain"=mmlFont(x, fontfamily, "plain", svgdev),
           "bold"=mmlFont(x, fontfamily, "bold", svgdev),
           "italic"=mmlFont(x, fontfamily, "italic", svgdev),
           "bolditalic"=mmlFont(x, fontfamily, "bold.italic", svgdev),
           "symbol"=mmlSymbol(x, fontfamily, fontface, svgdev),
           "list"=mmlCSL(x, fontfamily, fontface, svgdev),
           "%subset%"=mmlBinOp(x, fontfamily, fontface, "&#x2282;", svgdev),
           "%subseteq%"=mmlBinOp(x, fontfamily, fontface, "&#x2286;", svgdev),
           "%notsubset%"=mmlBinOp(x, fontfamily, fontface, "&#x2284;", svgdev),
           "%supset%"=mmlBinOp(x, fontfamily, fontface, "&#x2283;", svgdev),
           "%supseteq%"=mmlBinOp(x, fontfamily, fontface, "&#x2287;", svgdev),
           "%notsupset%"=mmlBinOp(x, fontfamily, fontface, "&#x2285;", svgdev),
           "%in%"=mmlBinOp(x, fontfamily, fontface, "&#x2208;", svgdev),
           "%notin%"=mmlBinOp(x, fontfamily, fontface, "&#x2209;", svgdev),
           "hat"=mmlAccent(x, fontfamily, fontface, "&#x005E;", svgdev),
           "tilde"=mmlAccent(x, fontfamily, fontface, "&#x007E;", svgdev),
           "dot"=mmlAccent(x, fontfamily, fontface, "&#x02D9;", svgdev),
           "ring"=mmlAccent(x, fontfamily, fontface, "&#x02DA;", svgdev),
           # Used "macron"
           "bar"=mmlAccent(x, fontfamily, fontface, "&#x00AF;", svgdev),
           # FIXME:  these are just normal accents positioned as limits
           "widehat"=mmlWideAccent(x, fontfamily, fontface,
                                   "&#x005E;", svgdev),
           "widetilde"=mmlWideAccent(x, fontfamily, fontface,
                                     "&#x007E;", svgdev),
           "%<->%"=mmlBinOp(x, fontfamily, fontface, "&#x2194;", svgdev),
           "%->%"=mmlBinOp(x, fontfamily, fontface, "&#x2192;", svgdev),
           "%<-%"=mmlBinOp(x, fontfamily, fontface, "&#x2190;", svgdev),
           "%up%"=mmlBinOp(x, fontfamily, fontface, "&#x2191;", svgdev),
           "%down%"=mmlBinOp(x, fontfamily, fontface, "&#x2193;", svgdev),
           "%<=>%"=mmlBinOp(x, fontfamily, fontface, "&#x21D4;", svgdev),
           "%=>%"=mmlBinOp(x, fontfamily, fontface, "&#x21D2;", svgdev),
           "%<=%"=mmlBinOp(x, fontfamily, fontface, "&#x21D0;", svgdev),
           "%dblup%"=mmlBinOp(x, fontfamily, fontface, "&#x21D1;", svgdev),
           "%dbldown%"=mmlBinOp(x, fontfamily, fontface, "&#x21D3;", svgdev),
           "displaystyle"=mmlStyle(x, fontfamily, fontface, "display", svgdev),
           "textstyle"=mmlStyle(x, fontfamily, fontface, "text", svgdev),
           "scriptstyle"=mmlStyle(x, fontfamily, fontface, "script", svgdev),
           "scriptscriptstyle"=mmlStyle(x, fontfamily, fontface,
                                        "scriptscript", svgdev),
           "underline"=mmlUnderline(x, fontfamily, fontface, svgdev),
           "~"=mmlSpace(x, fontfamily, fontface, svgdev),
           "phantom"=mmlPhantom(x, fontfamily, fontface, svgdev),
           "over"=,
           "frac"=mmlFrac(x, fontfamily, fontface, svgdev),
           "atop"=mmlFrac(x, fontfamily, fontface, lwd="0em", svgdev),
           "sum"=mmlBigOp(x, fontfamily, fontface, svgdev, "&#x2211;"),
           "prod"=mmlBigOp(x, fontfamily, fontface, svgdev, "&#x220F;"),
           "integral"=mmlBigOp(x, fontfamily, fontface, svgdev, "&#x222B;"),
           "union"=mmlBigOp(x, fontfamily, fontface, svgdev, "&#x22C3;"),
           "intersect"=mmlBigOp(x, fontfamily, fontface, svgdev, "&#x22C2;"),
           "prod"=mmlBigOp(x, fontfamily, fontface, svgdev, "&#x220F;"),
           "lim"=mmlBigOp(x, fontfamily, fontface, svgdev),
           "min"=mmlBigOp(x, fontfamily, fontface, svgdev),
           "inf"=mmlBigOp(x, fontfamily, fontface, svgdev),
           "sup"=mmlBigOp(x, fontfamily, fontface, svgdev),
           "group"=mmlGroup(x, fontfamily, fontface, svgdev),
           "bgroup"=mmlGroup(x, fontfamily, fontface, svgdev),
           mmlFun(x, fontfamily, fontface, svgdev))
}

# Table of Unicode math ops at
# http://www.w3.org/TR/MathML2/022.html
toMML.call <- function(x, fontfamily, fontface, svgdev, ...) {
    if (is.name(x[[1]])) {
        funCallToMML(x, fontfamily, fontface, svgdev)
    } else {
        mrow <- newXMLNode("mrow", parent = svgDevParent(svgdev))
        svgDevChangeParent(mrow, svgdev)
        toMML(x[[1]], fontfamily, fontface, svgdev)
        mfenced <- newXMLNode("mfenced", parent = mrow)
        svgDevChangeParent(mfenced, svgdev) 
        toMML(x[[2]], fontfamily, fontface, svgdev)
        svgDevChangeParent(xmlParent(mrow), svgdev) 
    }
}

# fontfamily is used to set explicit 'mathvariant' when it is not
# implicit in the formula element
expr2mml <- function(e, fontfamily, fontface, svgdev) {
    math <- newXMLNode("math", parent = svgDevParent(svgdev),
                       attrs = list(display = "inline"),
                       namespaceDefinitions = "http://www.w3.org/1998/Math/MathML")
    lapply(e, function(x) {
        mrow <- newXMLNode("mrow", parent = math)
        svgDevChangeParent(mrow, svgdev)
        toMML(x, fontfamily = fontfamily, fontface = fontface,
              svgdev = svgdev)
    })

    svgDevChangeParent(xmlParent(math), svgdev)
}

