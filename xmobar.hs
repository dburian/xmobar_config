------------------------------------------------------------------------------
-- |
-- Copyright: (c) 2018, 2019, 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sat Nov 24, 2018 21:03
--
--
-- An example of a Haskell-based xmobar. Compile it with
--   ghc --make -- xmobar.hs
-- with the xmobar library installed or simply call:
--   xmobar /path/to/xmobar.hs
-- and xmobar will compile and launch it for you and
------------------------------------------------------------------------------

import Xmobar

-- Example user-defined plugin

data HelloWorld = HelloWorld
    deriving (Read, Show)

instance Exec HelloWorld where
    alias HelloWorld = "hw"
    run   HelloWorld = return "<fc=red>Hello World!!</fc>"


-- data MyDynamicInfo = 

-- Configuration, using predefined monitors as well as our HelloWorld
-- plugin:

config :: Config
config = defaultConfig {
  font = "JetBrainsMono Nerd Font Bold 9"
  , additionalFonts = []
  , bgColor  = "#020202"
  , fgColor  = "#ffffff"
  , position = TopW C 100
  , lowerOnStart = True
  , hideOnStart = False
  , textOffset = 0
  , pickBroadest = True
  , persistent = False
  , allDesktops = False
  , commands = [ Run $ Cpu ["-t", "cpu: <total>%"] 10
              , Run $ Memory ["--template", "mem: <usedratio>%"] 10
              , Run $ Swap ["-t", "[<usedratio>%]"] 60
              , Run $ Date "%H:%M %Y-%m-%d" "date" 30
              , Run XMonadLog
              , Run $ Brightness ["-t", "brigh: <percent>%", "--", "-D", "intel_backlight" ] 10
              , Run $ Volume "default" "Master" ["-t", "vol: <volume>%"] 10
              , Run $ BatteryP ["BAT1"] [ "-t"
                , "batt: <left>%"
                , "--"
                , "-O", "charging" ] 120
              , Run $ Kbd [("us", "us"), ("cz(qwerty)", "cs")]
              , Run $ Wireless "wlp3s0" ["-t", "<ssid> <quality>%"] 10
      ]
  , sepChar  = "%"
  , alignSep = "}{"
  , template = "%XMonadLog% }{ %kbd% | %default:Master% %bright% | %wlp3s0wi% | %battery% | %cpu% %memory%%swap% | %date% "
  }

main :: IO ()
main = configFromArgs config >>= xmobar
