module Traversals.SOQuestion where

import qualified System.FilePath.Find as SFF

ffNotHidden :: SFF.FindClause Bool
ffNotHidden = SFF.fileName SFF./~? ".?*"

ffIsMD :: SFF.FindClause Bool
ffIsMD = SFF.extension SFF.==? ".md" SFF.&&? SFF.fileName SFF./~? ".?*"

findMarkdownSources :: FilePath -> IO [FilePath]
findMarkdownSources filePath = do
    paths <- SFF.find ffNotHidden ffIsMD filePath
    return paths
