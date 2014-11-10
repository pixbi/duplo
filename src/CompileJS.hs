import Language.JavaScript.Parser (readJs, renderToString)
import Development.Duplo.JSCompiler (compile)

main = do
  content <- getContents
  let node = readJs content
  putStr $ show $ compile node
