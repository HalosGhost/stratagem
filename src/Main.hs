module Main where

import           Graphics.Vty             hiding (pad)
import           Graphics.Vty.Widgets.All
import           System.Exit
import qualified Data.Text                as     T
import           Control.Monad
import           Data.Maybe
import           Data.List

fg = white
bg = black

main :: IO ()
main = do -- The title font is ANSI Shadow by Patrick Gillespie
          -- Find more info at <https://github.com/patorjk/figlet.js>
          let title = T.pack . intercalate "\n" $
                    [ "███████╗████████╗██████╗  █████╗ ████████╗ █████╗  ██████╗ ███████╗███╗   ███╗"
                    , "██╔════╝╚══██╔══╝██╔══██╗██╔══██╗╚══██╔══╝██╔══██╗██╔════╝ ██╔════╝████╗ ████║"
                    , "███████╗   ██║   ██████╔╝███████║   ██║   ███████║██║  ███╗█████╗  ██╔████╔██║"
                    , "╚════██║   ██║   ██╔══██╗██╔══██║   ██║   ██╔══██║██║   ██║██╔══╝  ██║╚██╔╝██║"
                    , "███████║   ██║   ██║  ██║██║  ██║   ██║   ██║  ██║╚██████╔╝███████╗██║ ╚═╝ ██║"
                    , "╚══════╝   ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═╝ ╚═════╝ ╚══════╝╚═╝     ╚═╝"
                    ]

          let items = [ ("Local PvP",  "Play against a local human"  )
                      , ("Local PvC",  "Play against your machine"   )
                      , ("Online PvP", "Play against a human online" )
                      , ("Exit",       "Exit the game"               )
                      ]

          lst <- newList 1
          sel <- vLimit (length items) lst

          forM_ (map fst items)
                $ \s -> addToList lst s =<< (plainText $ T.pack s)

          listComment <- plainText . T.pack . snd $ items !! 0
          lst `onSelectionChange` \ev ->
              case ev of
                   SelectionOn _ k _ -> setText listComment    $ T.pack label
                                        where label = fromJust $ lookup k items
                   SelectionOff      -> return ()

          lst `onItemActivated` \(ActivateItemEvent _ s _) ->
              case s of
                   "Exit"       -> exitSuccess
                   "Local PvP"  -> return ()
                   "Local PvC"  -> return ()
                   "Online PvP" -> return ()

          tw      <- hCentered listComment
          table   <- newTable [column ColAuto] BorderFull
          setDefaultCellPadding table $ (padLeft 1) `pad` (padRight 1)

          ttext   <- plainText title
          mt      <- plainText T.empty
          mainBox <- (vBox mt ttext) <--> (vBox table tw) >>= withBoxSpacing 1

          addRow table sel

          fg <- newFocusGroup

          addToFocusGroup fg sel

          ui <- centered =<< hLimit 78 mainBox
          c  <- newCollection
          addToCollection c ui fg

          runUi c $ defaultContext
