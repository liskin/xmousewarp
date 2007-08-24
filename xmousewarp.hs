-- X Mouse warp
-- Copyright (C) 2007  Tomas Janousek <tomi@nomi.cz>
--
-- This program is intended to workaround the Intel driver's screen size
-- limitation:
--   * use vertical xinerama layout (two 1280x1024 screens, for example)
--   * $ xmousewarp H <left screen num> <right screen num>
--   * use your mouse as if the screens were aligned horizontally
--
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--


import System
import System.Exit (exitWith, ExitCode(..))
import Data.Bits
import Data.Maybe
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama

usage = [ "Usage: xmousewarp H/V <s1> <s2>" ]
printUsage = do
    mapM_ putStrLn usage
    exitWith $ ExitFailure 1

main = do
    args <- getArgs
    if (length args /= 3) then printUsage else return ()

    (or, s1, s2) <- case parseArgs args of
			 Nothing -> do
			     putStrLn "Couldn't parse arguments."
			     printUsage
			     -- ugly hack :)
			     return (undefined, undefined, undefined)
			 Just x -> return x

    dpy <- openDisplay ""
    screens <- getScreenInfo dpy
    let dflt = defaultScreen dpy
	scr = defaultScreenOfDisplay dpy
    rootw <- rootWindow dpy dflt

    -- prepare the window rectangles
    let w1' = s1op or $ screens!!s1
	w2' = s2op or $ screens!!s2
	(w1, w2) = sameDim (w1', w2')

    -- create windows
    win1 <- mkUnmanagedWindow dpy scr w1
    win2 <- mkUnmanagedWindow dpy scr w2
    sync dpy False

    -- go!
    eventLoop dpy (theOtherOne win1 win2) or

    where -- | Return the other window.
	  theOtherOne win1 win2 w = case w of
	      x | x == win1 -> win2
	      x | x == win2 -> win1

-- | The event loop.
eventLoop dpy too or = do
    allocaXEvent $ \e -> do
	nextEvent dpy e
	ev <- getEvent e
	case ev_event_type ev of
	     z | z == enterNotify -> do
		 myWarpPointer dpy (too $ ev_window ev)
		    (x $ ev_x ev) (y $ ev_y ev)
	     _ -> return ()
	eventLoop dpy too or
    where (x, y) = case or of
			Horizontal -> (const 0, id)
			Vertical   -> (id, const 0)

-- | Parse the arguments, returning Nothing if any of them is unparseable.
parseArgs :: [ String ] -> Maybe (Orientation, Int, Int)
parseArgs [a1, a2, a3] =
    let or = parseOrientation a1
	s1 = maybeRead a2
	s2 = maybeRead a3
    in do or' <- or
	  s1' <- s1
	  s2' <- s2
	  return (or', s1', s2')

-- | Make unmanaged, InputOnly window, select CrossingEvents and map it.
mkUnmanagedWindow dpy scr rect = do
    let x = rect_x rect
	y = rect_y rect
	w = rect_width rect
	h = rect_height rect
    let visual = defaultVisualOfScreen scr
	attrmask = cWOverrideRedirect
    rw <- rootWindow dpy (defaultScreen dpy)
    win <- allocaSetWindowAttributes $ \attributes -> do
	set_override_redirect attributes True
	createWindow dpy rw x y w h 0 0 inputOnly visual attrmask attributes
    setTextProperty dpy win "xmousewarp" wM_NAME
    selectInput dpy win (enterWindowMask .|. leaveWindowMask)
    mapWindow dpy win
    return win

-- | Warp the pointer and wait (discarding events) until it arrives to the
-- destination.
myWarpPointer dpy w x y = do
    warpPointer dpy none w 0 0 0 0 (fromIntegral x) (fromIntegral y)
    wait
    where wait = do
	      allocaXEvent $ \e -> do
		  nextEvent dpy e
		  ev <- getEvent e
		  if ev_window ev == w
		     then return ()
		     else wait

-- | Orientation datatype.
data Orientation = Horizontal | Vertical deriving (Eq, Read, Show)

-- | Parse the orientation specifier.
parseOrientation "H" = Just Horizontal
parseOrientation "h" = Just Horizontal
parseOrientation "V" = Just Vertical
parseOrientation "v" = Just Vertical
parseOrientation _ = Nothing

safeSize = 200

-- Screen rectangle => Edge rectangle convertors:
leftEdge rect = rect { rect_width = safeSize,
    rect_x = (rect_x rect) - fromIntegral safeSize + 1 }
topEdge rect = rect { rect_height = safeSize,
    rect_y = (rect_y rect) - fromIntegral safeSize + 1}
rightEdge rect = rect {
    rect_x = (rect_x rect + fromIntegral (rect_width rect) - 1),
    rect_width = safeSize }
bottomEdge rect = rect {
    rect_y = (rect_y rect + fromIntegral (rect_height rect) - 1),
    rect_height = safeSize }

-- Operations for screens and a particular orientation:
s1op Horizontal = rightEdge
s1op Vertical = bottomEdge
s2op Horizontal = leftEdge
s2op Vertical = topEdge

-- | Make the two rectangles the same size.
sameDim (Rectangle a b c d, Rectangle e f g h) =
    (Rectangle a b i j, Rectangle e f i j)
    where i = c `min` g
	  j = d `min` h

-- | Maybeised read.
maybeRead :: (Read a) => String -> Maybe a
maybeRead s = case reads s of
		   [(x, "")] -> Just x
		   _ -> Nothing
