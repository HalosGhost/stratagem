{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Graphics.Vty             hiding (pad)
import           Graphics.Vty.Widgets.All
import           System.Exit
import qualified Data.Text                as     T
import           Control.Monad
import           Data.Maybe

fg = white
bg = black

main :: IO ()
main = do lst <- newList 1
          setSelectedUnfocusedAttr lst $ Just (fgColor brightGreen)
          sel <- vLimit 4 lst

          let items = [ ("Local PvP",  "Play against a local human"  )
                      , ("Local PvC",  "Play against your machine"   )
                      , ("Online PvP", "Play against a human online" )
                      , ("Exit",       "Exit the game"               )
                      ]

          listComment <- plainText T.empty

          forM_ (map fst items)
                $ \s -> addToList lst s =<< (plainText $ T.pack s)

          lst `onSelectionChange` \ev ->
              case ev of
                   SelectionOn _ k _ -> setText listComment label
                                        where label = fromJust $ lookup k items
                   SelectionOff      -> return ()

          lst `onItemActivated` \(ActivateItemEvent _ s _) ->
              case s of
                   "Exit" -> exitSuccess
                   _      -> return ()

          fg <- newFocusGroup

          addToFocusGroup fg sel
          addToFocusGroup fg listComment

          c <- newCollection
          addToCollection c sel fg
          addToCollection c listComment fg

          runUi c $ defaultContext { normalAttr = white `on` black }
