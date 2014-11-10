import Language.JavaScript.Parser (readJs, renderToString)
import Development.Duplo.JSCompiler (compile)

main = do
  content <- getContents
  putStr $ show $ compile $ readJs content
