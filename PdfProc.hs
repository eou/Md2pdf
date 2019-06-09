{-
    File : PdfProc.hs
    Copyright : (c) Hangyi Wang (hangyi)
    Md2pdf
-}

{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}

module PdfProc (
    generateParagraph,
    generatePage,
    generatePdfFile
)
where

import Block
import MdProc

import Graphics.PDF
import Data.Vector as DV
            
data MyVertStyles = NormalPara
                  | CodePara
                  | QuotePara

data MyParaStyles = Normal
                  | Bold
                  | Itali
                  | InlineC
                  | Cod
                  | Dotlist
                  | Hyperlnk
                  | HeadingFst
                  | HeadingSnd
                  | HeadingThi
                  | HeadingFor
                  | HeadingFif
                  | HeadingSix

instance ComparableStyle MyVertStyles where
    isSameStyleAs NormalPara NormalPara = True
    isSameStyleAs CodePara CodePara = True
    isSameStyleAs QuotePara QuotePara = True
    isSameStyleAs _ _ = False

instance ComparableStyle MyParaStyles where
    isSameStyleAs Normal Normal = True
    isSameStyleAs Bold Bold = True
    isSameStyleAs Itali Itali = True
    isSameStyleAs InlineC InlineC = True
    isSameStyleAs Cod Cod = True
    isSameStyleAs Dotlist Dotlist = True
    isSameStyleAs Hyperlnk Hyperlnk = True
    isSameStyleAs HeadingFst HeadingFst = True
    isSameStyleAs HeadingSnd HeadingSnd = True
    isSameStyleAs HeadingThi HeadingThi = True
    isSameStyleAs HeadingFor HeadingFor = True
    isSameStyleAs HeadingFif HeadingFif = True
    isSameStyleAs HeadingSix HeadingSix = True
    isSameStyleAs _ _ = False

instance Style MyParaStyles where
    textStyle Normal = TextStyle (PDFFont Times_Roman 5) black black FillText 1.0 1.0 1.0 1.0
    textStyle Bold = TextStyle (PDFFont Times_Bold 5) black black FillText 1.0 1.0 1.0 1.0
    textStyle Itali = TextStyle (PDFFont Times_Italic 5) black black FillText 1.0 1.0 1.0 1.0
    textStyle InlineC = TextStyle (PDFFont Helvetica_Oblique 5) blue blue FillText 1.0 1.0 1.0 1.0
    textStyle Cod = TextStyle (PDFFont Helvetica_Oblique 5) black black FillText 1.0 1.0 1.0 1.0
    textStyle Dotlist = TextStyle (PDFFont Times_BoldItalic 5) black black FillText 1.0 1.0 1.0 1.0
    textStyle Hyperlnk = TextStyle (PDFFont Courier_Oblique 5) black black FillText 1.0 1.0 1.0 1.0
    textStyle HeadingFst = TextStyle (PDFFont Courier_Bold 18) black black FillText 1.0 1.0 1.0 1.0
    textStyle HeadingSnd = TextStyle (PDFFont Courier_Bold 12) black black FillText 1.0 1.0 1.0 1.0
    textStyle HeadingThi = TextStyle (PDFFont Courier_Bold 10) black black FillText 1.0 1.0 1.0 1.0
    textStyle HeadingFor = TextStyle (PDFFont Courier_Bold 9) black black FillText 1.0 1.0 1.0 1.0
    textStyle HeadingFif = TextStyle (PDFFont Courier_Bold 8) black black FillText 1.0 1.0 1.0 1.0
    textStyle HeadingSix = TextStyle (PDFFont Courier_Bold 7) black black FillText 1.0 1.0 1.0 1.0
    
instance ParagraphStyle MyVertStyles MyParaStyles where
    lineWidth _ w _ = w
    linePosition _ _ _ = 0.0
    interline _ = Nothing
    paragraphStyle CodePara = Just $ \(Rectangle (xa :+ ya) (xb :+ yb)) block -> do
        let f = Rectangle ((xa-3) :+ (ya-3)) ((xb+10) :+ (yb+3))
        fillColor $ Rgb 0.74 0.83 0.9
        fill f
        block
        return ()
    paragraphStyle QuotePara = Just $ \(Rectangle (xa :+ ya) (xb :+ yb)) block -> do
        let f = Rectangle ((xa-3) :+ (ya-3)) ((xb+10) :+ (yb+3))
        fillColor $ Rgb 0.96 0.96 0.86
        fill f
        block
        return ()
    paragraphStyle _ = Nothing


generateText :: Block -> Para MyParaStyles ()
generateText (Text str) 
    -- crude way
    | Prelude.last str == '\n' = do
        setStyle Normal 
        txt str
        forceNewLine
    | otherwise = do
        setStyle Normal 
        txt str

generateHeading :: Block -> Para MyParaStyles ()
generateHeading (Heading ordNum (Text str)) = case ordNum of
    1 -> do
        setStyle HeadingFst 
        txt str
    2 -> do
        setStyle HeadingSnd
        txt str
    3 -> do
        setStyle HeadingThi
        txt str
    4 -> do
        setStyle HeadingFor
        txt str
    5 -> do
        setStyle HeadingFif 
        txt str
    _ -> do
        setStyle HeadingSix
        txt str

generateItalic :: Block -> Para MyParaStyles ()
generateItalic (Italic (Text str)) = do
    setStyle Itali 
    txt str

generateEmphasis :: Block -> Para MyParaStyles ()
generateEmphasis (Emphasis (Text str)) = do
    setStyle Bold 
    txt str

generateCode :: Block -> Para MyParaStyles ()
generateCode (Code (Text str)) = do
    setStyle Cod
    txt str

generateInlineCode :: Block -> Para MyParaStyles ()
generateInlineCode (InlineCode (Text str)) = do
    setStyle InlineC 
    txt str

generateHyperlink :: Block -> Para MyParaStyles ()
generateHyperlink (Hyperlink (Text str)) = do
    setStyle Hyperlnk 
    txt str

generateDotList :: Block -> Para MyParaStyles ()
generateDotList (DotList (Text str))
    | Prelude.length str == 0 = do
        setStyle Dotlist
        txt "* "
    | otherwise = do
        setStyle Dotlist 
        txt $ "* " Prelude.++ str
        forceNewLine

generateQuote :: Block -> Para MyParaStyles ()
generateQuote (Quote (Text str)) = do
    setStyle Itali 
    txt str

generateParagraph :: Markdown Block -> [TM MyVertStyles MyParaStyles ()] -> Para MyParaStyles () -> [TM MyVertStyles MyParaStyles ()]
generateParagraph (Markdown v) lst ctx
    | DV.length v == 0 = lst Prelude.++ [setParaStyle NormalPara >> paragraph ctx]
    | otherwise = case DV.head v of
        Text b -> generateParagraph (Markdown (DV.tail v)) lst (ctx >> (generateText $ DV.head v))
        Italic b -> generateParagraph (Markdown (DV.tail v)) lst (ctx >> (generateItalic $ DV.head v))
        Emphasis b -> generateParagraph (Markdown (DV.tail v)) lst (ctx >> (generateEmphasis $ DV.head v))
        InlineCode b -> generateParagraph (Markdown (DV.tail v)) lst (ctx >> (generateInlineCode $ DV.head v))
        Hyperlink b -> generateParagraph (Markdown (DV.tail v)) lst (ctx >> (generateHyperlink $ DV.head v))
        (Heading ordNum b) -> generateParagraph (Markdown (DV.tail v)) (lst Prelude.++ [setParaStyle NormalPara >> paragraph ctx] Prelude.++ [setParaStyle NormalPara >> paragraph (generateHeading $ DV.head v)]) (txt "")
        Code b -> generateParagraph (Markdown (DV.tail v)) (lst Prelude.++ [setParaStyle NormalPara >> paragraph ctx] Prelude.++ [setParaStyle CodePara >> paragraph (generateCode $ DV.head v)]) (txt "")
        DotList b -> generateParagraph (Markdown (DV.tail v)) lst (ctx >> (generateDotList $ DV.head v))
        Quote b -> generateParagraph (Markdown (DV.tail v)) (lst Prelude.++ [setParaStyle NormalPara >> paragraph ctx] Prelude.++ [setParaStyle QuotePara >> paragraph (generateQuote $ DV.head v)]) (txt "")

 

generatePage :: Markdown Block -> PDFReference PDFPage -> PDF ()
generatePage markdown page = do
    drawWithPage page $ do
        displayFormattedText (Rectangle (15 :+ 0) (180 :+ 285)) NormalPara Normal paragraphs where
            paragraphs = Prelude.foldl (>>) (paragraph $ txt "") (generateParagraph markdown [] (txt ""))

getOutputFileName :: String -> String
getOutputFileName inputMdFileName = (Prelude.reverse $ Prelude.drop 3 $ Prelude.reverse inputMdFileName) Prelude.++ ".pdf"

generatePdfFile :: String -> Markdown Block -> IO ()
generatePdfFile inputMdFileName markdown = do
    let pdfFileName = getOutputFileName inputMdFileName
    let documentInfo = standardDocInfo 
    let defaultPageSize = PDFRect 0 0 210 300

    runPdf pdfFileName documentInfo defaultPageSize $ do
        page <- addPage Nothing
        generatePage markdown page