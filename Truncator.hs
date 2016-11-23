module Truncator (
    truncateHtml,
    extractHtml,
    extractText,
    cleanTags,
    closeTags,
    annotateEnding
) where

import Data.List
import Text.HTML.TagSoup

-- if there the number of chars is below the max, just return everything
truncateHtml :: Int -> String -> String
truncateHtml n html
    | textLength html <= n = html
    | otherwise = renderTags . closeTags . annotateEnding . cleanTags $ extractHtml (n - 1) (parseTags html) 0

textLength :: String -> Int
textLength html = foldl (\acc t -> acc + length (fromTagText t)) 0 $ filter isTagText $ parseTags html

-- extract text up to a max of n characters,
-- using words as the atomic unit
extractText :: Int -> String -> Bool -> String
extractText n _ _ | n == 0 = ""
extractText _ [] _ = ""
extractText n text isBold
    | len > n = ""
    | otherwise = word ++ extractText (n - len) remaining isBold
    where
      -- tail text to account for possible first space
      (endWord, remaining) = span (/= ' ') $ tail text
      weight = if isBold then 1.25 else 1.0
      word = head text : endWord
      len = ceiling $ weight * fromIntegral (length word)

-- recursively extract tags until up to n characters of text
extractHtml :: Int -> [Tag String] -> Int -> [Tag String]
extractHtml n _ _ | n == 0 = []
extractHtml _ [] _ = []
extractHtml n ((TagText t):rest) isBold
    | null text = []
    | otherwise = TagText text:(extractHtml (n - length text) rest isBold)
    where
        text = extractText n t (isBold > 0)
extractHtml n ((t@(TagOpen name attr)):rest) isBold = t:(extractHtml n rest isBold')
    where
        isBold' = if name == "strong" || name == "b" then isBold + 1 else isBold
extractHtml n ((t@(TagClose name)):rest) isBold = t:(extractHtml n rest isBold')
    where
        isBold' = if name == "strong" || name == "b" then isBold - 1 else isBold

-- strips trailing whitespace
stripWhitespace :: String -> String
stripWhitespace = dropWhileEnd (== ' ')

-- preps the last inner text with ellipsis and cleans trailing whitespace
annotateEnding :: [Tag String] -> [Tag String]
annotateEnding tags
    | isTagText $ last tags = init tags ++ [TagText ((stripWhitespace $ fromTagText $ last tags) ++ "…")]
    | otherwise = tags

-- drop empty dangling tags
cleanTags :: [Tag String] -> [Tag String]
cleanTags tags = dropWhileEnd (\x -> not $ isTagText x) $ tags

-- closes dangling tags
-- dangling tags are outer-most tags which do not align with any close tags
closeTags :: [Tag String] -> [Tag String]
closeTags tags = tags ++ map closeTag unclosed
    where
        (closed, opened) = partition isTagClose $ filter (not . isTagText) tags
        unclosed = reverse $ take (length opened - length closed) opened

-- create matching close tag for an open tag
closeTag :: Tag String -> Tag String
closeTag (TagOpen name attr) = TagClose name
