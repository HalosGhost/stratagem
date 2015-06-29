{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Graphics.Vty             hiding (pad)
import           Graphics.Vty.Widgets.All
import           System.Exit
import qualified Data.Text                as     T

fg = white
bg = black

main :: IO ()
main = do lst <- newList 1
          setSelectedUnfocusedAttr lst $ Just (fgColor brightGreen)
          sel <- vLimit 5 lst

          let i1 = "Local PvP"
              i2 = "Local PvC"
              i3 = "Online PvP"

          listComment <- plainText T.empty

          addToList lst i1 =<< plainText i1
          addToList lst i2 =<< plainText i2
          addToList lst i3 =<< plainText i3

          lst `onSelectionChange` \ev ->
              case ev of
                   SelectionOn _ k _ -> setText listComment k
                   SelectionOff      -> return ()

          fg <- newFocusGroup
          fg `onKeyPressed` \_ k _ ->
             case k of
                  KChar 'Q' -> exitSuccess
                  _         -> return False

          addToFocusGroup fg sel
          addToFocusGroup fg listComment
          c <- newCollection
          addToCollection c sel fg
          addToCollection c listComment fg
          runUi c $ defaultContext { normalAttr = white `on` black }
