module Site.Filters (
  sassCompiler
) where

import Hakyll
import Site.Util (procArgs)

-- TODO: preferably use patterns/rules here
sassCompiler :: Compiler (Item String)
sassCompiler = loadBody (fromFilePath "scss/screen.scss")
                 >>= makeItem
                 >>= withItemBody (unixFilter cmd args)
  where (cmd, args) = procArgs "sass"
                        ["-s", "--scss", "-I", "provider/scss/",
                         "--cache-location", "generated/scss"]
