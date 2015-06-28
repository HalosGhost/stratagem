{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Vty hiding (pad)
import Graphics.Vty.Widgets.All
import System.Exit

fg = white
bg = black

main :: IO ()
main = do pt <- plainText "Hello, World!" >>= withNormalAttribute (fgColor fg)
          ui <- return pt

          fg <- newFocusGroup
          fg `onKeyPressed` \_ k _ -> do
             case k of
                  KEsc -> exitSuccess
                  _    -> return False

          addToFocusGroup fg ui
          c <- newCollection
          addToCollection c ui fg
          runUi c $ defaultContext { focusAttr = white `on` black }
