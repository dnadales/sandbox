module Traversals.FileManip where

import           Data.List            (isPrefixOf)
import           System.FilePath.Find (always, extension, fileName, find, (&&?),
                                       (/~?), (==?))

findMdSources :: FilePath -> IO [FilePath]
findMdSources fp = find isVisible (isMdFile &&? isVisible) fp
    where
      isMdFile = extension ==? ".md"
      isVisible = fileName /~? ".?*"
