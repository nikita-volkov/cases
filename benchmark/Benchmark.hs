
import Prelude
import Cases
import qualified CriterionPlus as C
import qualified Data.Text.Lazy as Text

main = do
  C.benchmark $ do

    C.standoff "" $ do
      let !text = Text.replicate 100 "Abc 123 / dsf asdf ;lkj. "
      C.subject "camelize" $ do
        C.nfIO $ return $ camelize text