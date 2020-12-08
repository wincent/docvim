import Distribution.Simple
import System.Environment

-- Hacking this to work based on:
--
--    https://stackoverflow.com/a/39019781
--
-- Relies on `happy` existing at /usr/bin/happy; eg. on Arch Linux:
--
--    sudo pacman -S stack happy

main = do
  args <- getArgs
  let args' = if elem "configure" args
    then args ++ [ "--with-happy=/usr/bin/happy" ]
    else args
  defaultMainWithArgs args'
