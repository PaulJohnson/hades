{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

-- |
module Test.Report.TestDocument where

import qualified Data.ByteString as B
import System.IO
import Model.Report.Document
import Model.Report.Docx
import Model.Report.Html
import qualified Text.Blaze.Html.Renderer.Utf8 as H
import System.Directory
import System.FilePath


runDocumentTest :: IO Bool
runDocumentTest = do
   temp <- getTemporaryDirectory
   putStrLn $ "Writing test documents in " <> temp
   makeDocx Nothing "Test Document" (temp </> "testWordDoc.docx") testDoc mempty
   let html = mkHtmlDocument mempty mempty testDoc
   withFile (temp </> "testHtml.html") WriteMode $ \h ->
      H.renderHtmlToByteStringIO (B.hPut h) html
   return True



-- | A document that exercises the "Block" and "Inline" combinators for nested lists.
testDoc :: [Block]
testDoc = [
      Heading 1 noAttr [Str "First Heading with ", Emph [Str "Italic Text"]],
      Plain [Str "Plain text block. Lorem ipsum dolor sit amet, consectetur adipiscing elit, \
            \sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
            \veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo \
            \consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum \
            \dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, \
            \sunt in culpa qui officia deserunt mollit anim id est laborum."],
      Para [Str "Paragraph block. Lorem ipsum dolor sit amet, consectetur adipiscing elit, \
            \sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
            \veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo \
            \consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum \
            \dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, \
            \sunt in culpa qui officia deserunt mollit anim id est laborum."],
      OrderedList ListRomanLower [
            [Para [Str "First item in an ordered list which is indexed with lower case roman \
                  \numerals. Note that the number type is ignored by Word."],
             Para [Str "Second paragraph of the first item in the ordered list. Ut enim ad minim \
                  \veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo \
                  \consequat."]],
            [Para [Str "Second item in the ordered list. This one has a sub-list of bullets:"],
             UnorderedList [
                  [Para [Str "Duis aute irure dolor in reprehenderit in voluptate velit esse \
                        \cillum."],
                   Para [Str "Faucibus nisl tincidunt eget nullam non nisi est. Nunc sed blandit \
                        \libero volutpat sed cras ornare."]
               ], [Para [Str "Rhoncus est pellentesque elit ullamcorper dignissim cras tincidunt \
                        \lobortis."]]
            ]],
            [Para [Str "Third item in the ordered list. This one has an ordered sub-list:"],
             OrderedList ListDecimal [
                   [Para [Str "Duis aute irure dolor in reprehenderit in voluptate velit esse \
                         \cillum."],
                    Para [Str "Faucibus nisl tincidunt eget nullam non nisi est. Nunc sed blandit \
                         \libero volutpat sed cras ornare."]

               ], [Para [Str "Rhoncus est pellentesque elit ullamcorper dignissim cras tincidunt \
                        \lobortis."]]
               ]]
         ],
      Heading 1 noAttr [Str "Definition List Section"],
      DefinitionList [
            (
               [Str "Def 1"],
               [Para [Str "Neque gravida in fermentum et sollicitudin ac orci phasellus egestas. \
                     \Volutpat maecenas volutpat blandit aliquam etiam. Mattis vulputate enim \
                     \nulla aliquet porttitor lacus luctus. "]]
            ), (
               [Str "Def 2"],
               [Para [Str "Sed adipiscing diam donec adipiscing tristique risus. Neque volutpat ac \
                     \tincidunt vitae. Velit ut tortor pretium viverra suspendisse potenti nullam \
                     \ac tortor."]]
            )
         ]]
