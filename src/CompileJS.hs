import Language.JavaScript.Parser (readJs, renderToString)
import Development.Duplo.JavaScript.Order (order)

main = do
  content <- getContents
  let precompiled = readJs content
  let ordered = order precompiled
  let compiled = ordered
  putStr $ renderToString compiled
