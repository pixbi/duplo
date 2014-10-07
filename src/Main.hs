{-import Development.Shake.Command-}
import Development.Shake.FilePath
{-import Development.Shake.Util-}
import Development.Shake
import Development.Duplo.Files

main :: IO ()
main = shakeArgs shakeOptions $ do
  ----------
  -- Output paths

  let target     = "public/"
  let targetJs   = combine target "index.js"
  {-let targetCss  = combine target "index.css"-}
  {-let targetHtml = combine target "index.html"-}

  ----------
  -- Input paths

  let inputJsP   = getDirectoryFiles "" ["app/index.js", "app/modules//*.js", "components/*/app/modules//*.js"]
  {-let inputStylP = getDirectoryFiles "" ["app//*.styl", "components//*.styl"]-}
  -- Only the main Jade files in all components
  {-let inputJadeP = getDirectoryFiles "" ["app/index.jade", "components/*/app/index.jade"]-}

  ----------
  -- Compiler paths

  {-let source = takeDirectory $__FILE__-}
  {-let nodeModules  = combine source "../../node_modules/.bin/"-}
  {-let stylCompiler = combine nodeModules "stylus"-}
  {-let jadeCompiler = combine nodeModules "jade"-}

  ----------
  -- Dependencies

  want [targetJs]
  {-want [targetJs, targetCss, targetHtml]-}

  ----------
  -- Do a build clean-up
  "clean" ~> do
    putNormal "Cleaning build files"
    cmd "rm" ["-r", "public/"]

  ----------
  -- Scripts
  targetJs *> \out -> do
    alwaysRerun
    inputJs  <- inputJsP
    contents <- mapM expandFile inputJs
    mapM_ putNormal $ map getPath contents
    writeFileChanged out $ concat $ map getContent contents

  {------------}
  {--- Stylesheet-}
  {-targetCss *> \out -> do-}
  {-  alwaysRerun-}
  {-  inputStyl    <- inputStylP-}
  {-  stylContents <- mapM readFile' inputStyl-}
  {-  -- Trailing newline is significant in case of empty Stylus-}
  {-  let styl = concat stylContents ++ "\n"-}
  {-  Stdout cssContents <- command [Stdin styl] stylCompiler []-}
  {-  writeFileChanged out cssContents-}

  {------------}
  {--- Markup-}
  {-targetHtml *> \out -> do-}
  {-  alwaysRerun-}
  {-  inputJade    <- inputJadeP-}
  {-  jadeContents <- mapM readFile' inputJade-}
  {-  -- Trailing newline is significant in case of empty Jade-}
  {-  let jade = concat jadeContents ++ "\n"-}
  {-  Stdout htmlContents <- command [Stdin jade] jadeCompiler []-}
  {-  writeFileChanged out htmlContents-}
