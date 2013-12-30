module Site.Filters (
  sassCompiler
) where

import Hakyll

-- TODO: preferably use patterns/rules here
sassCompiler :: Compiler (Item String)
sassCompiler = loadBody (fromFilePath "scss/screen.scss")
                 >>= makeItem
                 >>= withItemBody (unixFilter "sass" args)
  where args = ["-s", "--scss", "-I", "provider/scss/", "--cache-location", "generated/scss"]
