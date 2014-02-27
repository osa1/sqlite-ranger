{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-------------------------------------------------------------------------------
module Main where


-------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Database.SQLite3
import           Safe
import           UI.NCurses
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
data Table = Table
    { tblName :: Text
    , tblSql  :: Text
    , tblDump :: ([Text], [[Text]])
    } deriving (Show)


readTables :: Database -> IO [Table]
readTables db = do
    retRef <- newIORef []
    execWithCallback db query (callback retRef)
    tbls <- fmap reverse $ readIORef retRef
    mapM (addDumpField db) tbls
  where
    query = "SELECT * FROM sqlite_master WHERE type='table';"
    tblNameIdx = 2
    tblSqlIdx = 4

    callback :: IORef [Table] -> ColumnCount -> [Text] -> [Maybe Text] -> IO ()
    -- TODO: handle errors better
    callback ref _ _ colVals = do
      case join (atMay colVals tblNameIdx) of
        Nothing -> error "table name is missing"
        Just n ->
          case join (atMay colVals tblSqlIdx) of
            Nothing -> error $ "table sql is missing: " ++ T.unpack n
            Just sql -> modifyIORef ref (Table n sql ([], []) :)

    addDumpField db tbl = do
      dump <- dumpTable db tbl
      return tbl{tblDump=dump}


dumpTable :: Database -> Table -> IO ([Text], [[Text]])
dumpTable db Table{tblName} = do
    retRef <- newIORef ([], [])
    execWithCallback db query (callback retRef)
    readIORef retRef
  where
    query = "SELECT * FROM " <> tblName <> ";"

    callback ::
      IORef ([Text], [[Text]]) -> ColumnCount -> [Text] -> [Maybe Text] -> IO ()
    callback ref _ cols colVals =
      modifyIORef ref (updateRef cols (map (maybe "-" id) colVals))

    updateRef :: [Text] -> [Text] -> ([Text], [[Text]]) -> ([Text], [[Text]])
    updateRef cols colVals ([], colVals') = (cols, colVals : colVals')
    updateRef _ colVals (cols, colVals')  = (cols, colVals : colVals')


colw :: Integer -> Table -> Int
colw totalW tbl =
  let cols = fromIntegral $ length $ fst $ tblDump tbl
  in floor $ (fromIntegral $ totalW - cols + 1) / fromIntegral cols


-------------------------------------------------------------------------------
data App = App
    { screenx       :: Integer
    , screeny       :: Integer
    , window        :: Window
    , tables        :: [Table]
    , selected      :: Int
    , updateSize    :: Bool
    , redrawSidebar :: Bool
    , redrawMain    :: Bool
    }


initialApp :: Integer -> Integer -> Window -> App
initialApp x y win = App x y win [] (-1) True True True


-------------------------------------------------------------------------------
drawLine :: Integer -> Update ()
drawLine = drawLineH (Just $ Glyph ' ' [])


sidebarWidth :: Integer -> Integer
sidebarWidth screenx = floor $ (fromIntegral screenx / 100) * 20


drawSidebar :: App -> Curses ()
drawSidebar App{..} = do
    titleColorID <- newColorID ColorWhite ColorGreen 1
    selColor <- newColorID ColorWhite ColorMagenta 2

    updateWindow window $ do
      moveCursor 0 0
      setColor titleColorID
      drawLine sw
      drawText "Tables"

      setColor defaultColorID
      drawTables tables selected selColor (0, 0)

    render
  where
    sw = sidebarWidth screenx

    drawTables [] _ _ _ = return ()
    drawTables (t : ts) selected selColor (cy, cx) = do
      moveCursor (cy+1) cx

      if selected == 0 then do
        setColor selColor
        drawLine sw
        drawText $ tblName t
      else do
        setColor defaultColorID
        drawLine sw
        drawText $ tblName t

      drawTables ts (selected-1) selColor (cy+1, cx)


handleEvents :: App -> Curses App
handleEvents app@App{..} = do
    ev <- getEvent window (Just 100)
    case ev of
      Just (EventCharacter char) -> return app
      Just (EventSpecialKey key) -> return $ handleKey key
      Just EventResized -> return $ app{updateSize=True}
      _ -> return app
  where
    handleKey KeyDownArrow
      | selected < length tables - 1 = app{selected=selected+1, redrawSidebar=True, redrawMain=True}
      | otherwise = app{selected=0, redrawSidebar=True, redrawMain=True}
    handleKey KeyUpArrow
      | selected > 0 = app{selected=selected-1, redrawSidebar=True, redrawMain=True}
      | otherwise = app{selected=length tables-1, redrawSidebar=True, redrawMain=True}
    handleKey _ = app


drawMain :: App -> Curses ()
drawMain app@App{..} = do
    titleColorID <- newColorID ColorWhite ColorGreen 1
    updateWindow window $ do
      clearBox

      moveCursor 0 (sw+1)
      setColor titleColorID
      drawLine mw
      drawText "SQL"

      moveCursor 1 (sw+1)
      setColor defaultColorID

      drawTexts 1 (sw+1) dumpTexts
  where
    clearBox :: Update ()
    clearBox = do
      setColor defaultColorID
      forM_ [0..screeny-1] $ \y -> do
        moveCursor y (sw+1)
        drawLine screenx

    sw = sidebarWidth screenx

    mw = screenx - (sw + 1)

    colw' = colw mw (tables !! selected)

    dump :: ([Text], [[Text]])
    dump = tblDump $ tables !! selected

    dumpTexts :: [[Text]]
    dumpTexts = fst dump : snd dump

    drawTexts _ _ [] = return ()
    drawTexts y x (t : ts) = do
      drawCols y x t
      drawTexts (y+1) x ts

    drawCols _ _ [] = return ()
    drawCols y x (t : ts) = do
      moveCursor y x
      drawText t
      drawCols y (x + fromIntegral colw') ts


loop :: App -> Curses ()
loop app@App{..} = do

    -- TODO: refactor this using a state monad

    app' <- if updateSize then do
              (screeny', screenx') <- screenSize
              return app{screeny=screeny', screenx=screenx', updateSize=False}
            else
              return app

    app'' <- if redrawSidebar then do
               drawSidebar app'
               return $ app{redrawSidebar=False}
             else
               return app'

    app''' <- if redrawMain then do
                drawMain app''
                return app''{redrawMain=False}
              else
                return app''

    app'''' <- handleEvents app'''
    loop app''''


runApp :: App -> Curses ()
runApp app@App{..} = do
    db <- liftIO $ open "sqlitedb"
    tables <- liftIO $ readTables db
    loop app{tables=tables, selected=1}


main :: IO ()
main = runCurses $ do
    setEcho False
    window <- defaultWindow
    (screeny, screenx) <- screenSize
    _ <- setCursorMode CursorInvisible
    runApp $ initialApp screenx screeny window
