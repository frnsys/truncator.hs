import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Truncator
import Text.HTML.TagSoup

main = hspec $ do
    describe "Truncator" $ do
        it "extract words" $ do
            extractText 5 "Hello there!" False `shouldBe` "Hello"

        it "extract words no truncating" $ do
            extractText 5 "Hi there!" False `shouldBe` "Hi"

        it "extract words less than required" $ do
            extractText 5 "Hi" False `shouldBe` "Hi"

        it "extracts safely truncated html" $ do
            extractHtml 5 (parseTags "<em>Hello World</em>") 0 `shouldBe` [TagOpen "em" [], TagText "Hello"]

        it "leaves no empty dangling tags" $ do
            cleanTags (extractHtml 8 (parseTags "Hello <strong>World</strong>") 0) `shouldBe` [TagText "Hello "]

        it "closes dangling html tags" $ do
            closeTags [TagOpen "strong" [], TagText "Hello"] `shouldBe` [TagOpen "strong" [], TagText "Hello", TagClose "strong"]

        it "closes dangling html tags with some valid tags" $ do
            closeTags [TagOpen "strong" [], TagOpen "em" [], TagText "Hello", TagClose "em"] `shouldBe` [TagOpen "strong" [], TagOpen "em" [], TagText "Hello", TagClose "em", TagClose "strong"]

        it "preserves word and adds ellipsis" $ do
            truncateHtml 7 "Hello there!" `shouldBe` "Hello…"

        it "produces valid html" $ do
            truncateHtml 7 "<em>Hello there!</em>" `shouldBe` "<em>Hello…</em>"

        it "weights bold text more than non-bold text" $ do
            truncateHtml 10 "<strong>Hello there!</strong>" `shouldBe` "<strong>Hello…</strong>"

        it "handles nested bold text" $ do
            truncateHtml 15 "<strong>Hello <strong>there friend!</strong></strong>" `shouldBe` "<strong>Hello <strong>there…</strong></strong>"

        it "does not count html without inner text" $ do
            truncateHtml 7 "Hello <strong>there!</strong>" `shouldBe` "Hello…"

        it "should return full text if n is large enough" $ do
            truncateHtml (length "Hello there!") "Hello <strong>there!</strong>" `shouldBe` "Hello <strong>there!</strong>"
