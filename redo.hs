
import System.Process

main =
  (createProcess . shell) "sh redo.do" >>
  return ()

