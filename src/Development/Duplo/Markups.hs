module Development.Duplo.Markups (
    build
  ) where

import Development.Duplo.Files (File)

build :: FilePath -> FilePath -> FilePath -> Action a
build cwd bin out = \ out -> do
  -- Only the main Jade files in all components
  let inputJadePaths = getDirectoryFiles cwd
                         [ "app/index.jade"
                         , "components/*/app/index.jade"
                         ]
  let compiler = combine bin "jade"
  inputJade    <- jadeFullPaths
  putNormal "-----*"
  putNormal $ show inputJade
  -- Run through the Jade compiler. Trailing newline is significant in case
  -- of empty Jade
  jadeContents <- mapM ((++ "\n") . readFile') inputJade
  -- Construct file records
  let records = [ Files.expandFile path content
                | path    <- inputJade
                , content <- jadeContents
                ]
  let params = [ "--pretty"
               -- Jade needs a file and take its dirname for includes
               , "--path", combine cwd "app/modules/"
               ]
  Stdout htmlContents <- command [Stdin jade] compiler params
  writeFileChanged out htmlContents
