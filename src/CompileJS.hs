import Language.JavaScript.Parser (readJs, renderToString)
import Development.Duplo.JSCompiler (compile)

main = do
  content <- getContents
  putStr $ renderToString $ compile $ readJs content
