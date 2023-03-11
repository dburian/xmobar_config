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
  , commands = [
              Run XMonadLog
              , Run $ Kbd [("us", "\xF11C  us"), ("cz(qwerty)", "\xF11C  cs")]
              , Run $ Volume "default" "Master" [
                  "-t", "<volumestatus>",
                  "-p", "3",
                  "--",
                  "-O", "\xF028 ",
                  "-o", "\xF026 ",
                  "-c", "#ffffff",
                  "-C", "#ffffff"
                  ] 10
              , Run $ Brightness [
                  "-t", "\xE30D <percent>%",
                  "-p", "2",
                  "--",
                  "-D", "intel_backlight"
                  ] 10
              , Run $ Wireless "wlp3s0" ["-t", "\xF1EB <ssid> <quality>%"] 10
              , Run $ BatteryP ["BAT1"] [ "-t"
                , "\xF578 <left>% (<timeleft>)"
                , "-p", "3"
                , "--"
                , "-O", "\xFBA3"
                , "-i", "\xFBA3"
                ] 60
              , Run $ Cpu ["-t", "\xE266 <total>%", "-p", "2" ] 10
              , Run $ Memory ["--template", "\xF1C0 <usedratio>%", "-p", "2"] 10
              , Run $ Swap ["-t", "[<usedratio>%]"] 60
              , Run $ Date "%H:%M %m.%d. %Y" "date" 30
      ]
  , sepChar  = "%"
  , alignSep = "}{"
  , template = "%XMonadLog% }{ %kbd%  %default:Master% %bright%  %wlp3s0wi%  %battery%  %cpu% %memory%%swap%  %date% "
  }

main :: IO ()
main = configFromArgs config >>= xmobar
