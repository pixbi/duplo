import Development.Shake
{-import Development.Shake.Command-}
{-import Development.Shake.FilePath-}
{-import Development.Shake.Util-}

main :: IO ()
main = shakeArgs shakeOptions $ do
  ----------
  -- Output paths

  let targetDir  = "public/"
  let targetJs   = targetDir ++ "index.js"
  {-let targetCss  = targetDir ++ "index.css"-}
  {-let targetHtml = targetDir ++ "index.html"-}

  ----------
  -- Input paths

  -- JavaScript files only within app directories
  let inputJsP   = getDirectoryFiles "" ["app//*.js", "components/*/app//*.js"]
  {--- Any Stylus files-}
  {-let inputStylP = getDirectoryFiles "" ["//*.styl"]-}
  {--- Only the main Jade files in all components-}
  {-let inputJadeP = getDirectoryFiles "" ["//app/index.jade"]-}

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
  -- JavaScript
  targetJs *> \out -> do
    alwaysRerun
    -- Get all JS files
    inputJs    <- inputJsP
    -- Read all files
    jsContents <- mapM readFile' inputJs
    writeFileChanged out $ concat jsContents
