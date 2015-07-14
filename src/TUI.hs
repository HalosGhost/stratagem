module TUI where

import           Graphics.Vty
import           Graphics.Vty.Widgets.All
import qualified Data.Text                as T
import           Board
import           Piece

newBoard :: Board -> IO (Widget Board)
newBoard is = do newWidget is $ \w -> w
            { render_ = \this size ctx -> do
                        (Board b) <- getState this
                        let s     = T.pack $ pretty None b
                            width = (fromEnum $ regionWidth size) -
                                    (fromEnum $ textWidth s)
                        return    $ string (getNormalAttr ctx) $ show b
            }
