{-
    File : mdProc.hs
    Copyright : (c) Hangyi Wang (hangyi)
    Md2pdf
-}

module MdProc (
    mdProcessor
)
where

import Block

import Data.Vector as DV
import Data.Char as DC

{-
    preProcessor: split the markdown text as a string into several substrings
-}
preProcessor :: String -> Markdown String -> String -> Markdown String
preProcessor md markdown curStr
    -- reach the end
    | Prelude.length md == 0 = if Prelude.length curStr /= 0 then (mdAdd curStr markdown) else markdown
    -- a new start
    | Prelude.length curStr == 0 = preProcessor (Prelude.tail md) markdown [Prelude.head md]
    -- start parsing heading
    | (Prelude.head md == '#' && Prelude.head curStr /= '#') = preProcessor (Prelude.tail md) (mdAdd curStr markdown) "#"
    -- finish parsing heading
    | (Prelude.head curStr == '#' && Prelude.head md == '\n') = preProcessor (Prelude.tail md) (mdAdd curStr markdown) ""
    -- start parsing italic
    | (Prelude.head md == '*' && Prelude.head curStr /= '*' && md !! 1 /= '*') = preProcessor (Prelude.tail md) (mdAdd curStr markdown) "*"
    -- finish parsing italic
    | (Prelude.head md == '*' && Prelude.head curStr == '*' && (curStr !! 1) /= '*') = preProcessor (Prelude.tail md) (mdAdd (curStr Prelude.++ "*") markdown) ""
    -- start parsing emphasis
    | (Prelude.head md == '*' && md !! 1 == '*' && Prelude.head curStr /= '*') = preProcessor (Prelude.drop 2 md) (mdAdd curStr markdown) "**"
    -- finish parsing emphasis
    | (Prelude.head md == '*' && md !! 1 == '*' && Prelude.head curStr == '*') = preProcessor (Prelude.drop 2 md) (mdAdd (curStr Prelude.++ "**") markdown) ""
    -- start parsing InlineCode
    | (Prelude.head md == '`' && Prelude.head curStr /= '`' && md !! 1 /= '`') = preProcessor (Prelude.tail md) (mdAdd curStr markdown) "`"
    -- finish parsing InlineCode
    | (Prelude.head md == '`' && Prelude.head curStr == '`' && (curStr !! 1) /= '`') = preProcessor (Prelude.tail md) (mdAdd (curStr Prelude.++ "`") markdown) ""
    -- start parsing code
    | (Prelude.head md == '`' && md !! 1 == '`' && Prelude.length curStr <= 3) = preProcessor (Prelude.drop 3 md) (mdAdd curStr markdown) "```"
    -- finish parsing code
    | (Prelude.head md == '`' && md !! 1 == '`' && Prelude.length curStr > 3) = preProcessor (Prelude.drop 3 md) (mdAdd (curStr Prelude.++ "```") markdown) ""
    -- start parsing hyperlink
    | (Prelude.head md == '[') = preProcessor (Prelude.tail md) (mdAdd curStr markdown) "["
    -- finish parsing hyperlink
    | (Prelude.head md == ')' && Prelude.head curStr == '[') = preProcessor (Prelude.tail md) (mdAdd (curStr Prelude.++ ")") markdown) ""
    -- start parsing DotList
    | (Prelude.head md == '-' && md !! 1 == ' ') = preProcessor (Prelude.drop 2 md) (mdAdd curStr markdown) "- "
    -- finish parsing DotList
    | (Prelude.head md == '\n' && Prelude.head curStr == '-') = preProcessor (Prelude.tail md) (mdAdd (curStr Prelude.++ "\n") markdown) ""
    -- start parsing quote
    | (Prelude.head md == '>') = preProcessor (Prelude.tail md) (mdAdd curStr markdown) ">"
    -- finish parsing quote
    | (Prelude.head md == '\n' && Prelude.head curStr == '>') = preProcessor (Prelude.tail md) (mdAdd (curStr Prelude.++ "\n") markdown) ""
    | otherwise = preProcessor (Prelude.tail md) markdown (curStr Prelude.++ [Prelude.head md])

{-
    trim: returns a copy of the string, with leading and trailing whitespace omitted
    e.g. "    string  abc  f \n   " => "string  abc  f"
-}
trim :: String -> String
trim = f . f where f = Prelude.reverse . Prelude.dropWhile DC.isSpace

{-
    parseText: parse simple text from markdown text into Block
    e.g. " simple text\ntest  \n" => Text " simple text\ntest  \n"
-}
parseText :: String -> Block
parseText md = Text md

{-
    parseHeading: parse Heading from markdown text into Block
    e.g. "##  title  \n" => Heading 2 Text "title"
-}
parseHeading :: Int -> String -> Block
parseHeading ordNum md
    -- smallest heading size is 6
    | (ordNum == 6 || Prelude.head md /= '#') = Heading ordNum $ parseText $ trim md
    | Prelude.head md == '#' = parseHeading (ordNum + 1) $ Prelude.tail md
    | otherwise = parseHeading ordNum $ Prelude.tail md

{-
    parseItalic: parse italic text from markdown text into Block
    e.g. "* italic text *" => Italic Text "italic text"
-}
parseItalic :: String -> Block
parseItalic md = Italic $ parseText $ trim $ Prelude.tail $ Prelude.init md

{-
    parseEmphasis: parse emphasis text from markdown text into Block
    e.g. "** empha **" => Emphasis Text "empha"
-}
parseEmphasis :: String -> Block
parseEmphasis md = Emphasis $ parseText $ trim $ Prelude.tail $ Prelude.init $ Prelude.tail $ Prelude.init md

{-
    parseInlineCode: parse inline code from markdown text into Block
    e.g. "`int c = 1;`" => InlineCode Text "int c = 1;"
-}
parseInlineCode :: String -> Block
parseInlineCode md = InlineCode $ parseText $ Prelude.tail $ Prelude.init md

{-
    parseCode: parse code from markdown text into Block
    e.g. "```\nint c = 1;\nwhile True:\n    sleep(3)\nreturn ()\n```" => Code Text "\nint c = 1;\nwhile True:\n    sleep(3)\nreturn ()\n"
-}
parseCode :: String -> Block
parseCode md = Code $ parseText $ Prelude.tail $ Prelude.init $ Prelude.tail $ Prelude.init $ Prelude.tail $ Prelude.init md

{-
    parseLink: parse link from markdown text into Block
    e.g. "[Google](www.google.com)" => Link Text "[Google](www.google.com)"
-}
parseHyperlink :: String -> Block
parseHyperlink md = Hyperlink $ parseText md

{-
    parseDotList: parse dot list from markdown text into Block
    e.g. "- first\n" => DotList Text "first\n"
-}
parseDotList :: String -> Block
parseDotList md = DotList $ parseText $ trim $ Prelude.tail $ Prelude.tail md

{-
    parseQuote: parse quote from markdown text into Block
    e.g. ">quote\n" => Quote Text "quote\n"
-}
parseQuote :: String -> Block
parseQuote md = Quote $ parseText $ Prelude.tail md

{-
    parser: transform any type String to Block
-}
parser :: String -> Block
parser md
    -- Heading
    | Prelude.head md == '#' = parseHeading 0 md
    -- Italic
    | Prelude.head md == '*' && (md !! 1) /= '*' = parseItalic md
    -- Emphasis
    | Prelude.head md == '*' &&  (md !! 1) == '*' = parseEmphasis md
    -- InlineCode
    | Prelude.head md == '`' && (md !! 1) /= '`' = parseInlineCode md
    -- Code
    | Prelude.head md == '`' && (md !! 1) == '`' = parseCode md
    -- Hyperlink
    | Prelude.head md == '[' = parseHyperlink md
    -- DotList
    | Prelude.head md == '-' = parseDotList md
    -- Quote
    | Prelude.head md == '>' = parseQuote md
    | otherwise = parseText md

{-
    translator: translate Markdown String into Markdown Block
-}
translator :: Markdown String -> Markdown Block
translator md = fmap parser md

{-
    mdProcessor: transform markdown text into Markdown Block
-}
mdProcessor :: String -> Markdown Block
mdProcessor md = translator mdStr where mdStr = preProcessor md (Markdown (DV.empty)) ""