module Site.Filters (
  sassCompiler
) where

import Hakyll

import Site.ShellFilter

-- TODO: preferably use patterns/rules here
sassCompiler :: Compiler (Item String)
sassCompiler = loadBody (fromFilePath "scss/screen.scss")
                 >>= makeItem
                 >>= withItemBody (shellFilter "sass -s --scss -I provider/scss/ --cache-location generated/scss")
