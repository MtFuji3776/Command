module PandopcTool where

import Text.Pandoc.Readers.CommonMark
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc.Class
import Data.Text (Text)
import Text.Pandoc.PDF


test :: PandocIO Pandoc
test = readCommonMark def "# test"