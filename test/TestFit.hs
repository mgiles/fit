import Test.Fit.Internal.Numbers as Numbers
import Test.Fit.Internal.FitParser as FitParser
import Test.Fit.Internal.FitFile as FitFile

import Test.Hspec

main :: IO ()
main = hspec $ do
  Numbers.specs
  FitParser.specs
  FitFile.specs
