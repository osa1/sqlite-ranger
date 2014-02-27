{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-------------------------------------------------------------------------------
module Main where


-------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.IORef
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
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


tblCols :: Table -> [Text]
tblCols = fst . tblDump


tblRows :: Table -> [[Text]]
tblRows = snd . tblDump


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
    callback ref _ _ colVals =
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
      modifyIORef ref (updateRef cols (map (fromMaybe "-") colVals))

    updateRef :: [Text] -> [Text] -> ([Text], [[Text]]) -> ([Text], [[Text]])
    updateRef cols colVals ([], colVals') = (cols, colVals : colVals')
    updateRef _ colVals (cols, colVals')  = (cols, colVals : colVals')


colw :: Integer -> Table -> Int
colw totalW tbl =
  let cols = fromIntegral $ length $ fst $ tblDump tbl
  in floor (fromIntegral (totalW - cols + 1) / fromIntegral cols :: Double)



-------------------------------------------------------------------------------
data App = App
    { -- program settings
      screenx       :: Integer
    , screeny       :: Integer
    , window        :: Window

    -- tables in db
    , tables        :: [Table]

    -- event handling flags
    , updateSize    :: Bool
    , redrawSidebar :: Bool
    , redrawMain    :: Bool
    , exit          :: Bool

    -- sidebar view state
    , selectedTbl   :: Int

    -- main view state
    , selectedRow   :: Int
    , selectedCol   :: Int

    -- views
    , sidebarView   :: View
    , mainView      :: View
    , focus         :: View
    }


initialApp :: Integer -> Integer -> Window -> View -> View -> App
initialApp x y win sidebar main =
    App x y win [] True True True False (-1) (-1) (-1) sidebar main sidebar


-------------------------------------------------------------------------------
data View = View
    { handleKey  :: Key -> App -> App
    , handleChar :: Char -> App -> App
    }


mkSidebarView :: View
mkSidebarView = View
    { handleKey = flip keyHandler
    , handleChar = flip charHandler
    }
  where
    keyHandler app KeyDownArrow = moveDown app
    keyHandler app KeyUpArrow = moveUp app
    keyHandler app _ = app

    charHandler app '\n' =
      app{ focus=mainView app
         , selectedRow=if selectedRow app < 0 then 0 else selectedRow app
         , selectedCol=if selectedCol app < 0 then 0 else selectedCol app
         }
    charHandler app 'q' = app{exit=True}
    charHandler app 'k' = moveUp app
    charHandler app 'j' = moveDown app
    charHandler app _   = app

    moveUp app@App{..}
      | selectedTbl > 0 =
          app{selectedTbl=selectedTbl - 1, redrawSidebar=True, redrawMain=True}
      | otherwise =
          app{selectedTbl=length tables - 1, redrawSidebar=True, redrawMain=True}

    moveDown app@App{..}
      | selectedTbl < length tables - 1 =
          app{selectedTbl=selectedTbl + 1, redrawSidebar=True, redrawMain=True}
      | otherwise =
          app{selectedTbl=0, redrawSidebar=True, redrawMain=True}


mkMainView :: View
mkMainView = View
    { handleKey = flip keyHandler
    , handleChar = flip charHandler
    }
  where
    keyHandler app KeyDownArrow = moveDown app
    keyHandler app KeyUpArrow = moveUp app
    keyHandler app KeyLeftArrow = moveLeft app
    keyHandler app KeyRightArrow = moveRight app
    keyHandler app KeyEnter = app -- TODO
    keyHandler app _ = app

    charHandler app 'q' = app{exit=True}
    charHandler app '\27' = app{focus=sidebarView app}
    charHandler app 'j' = moveDown app
    charHandler app 'k' = moveUp app
    charHandler app 'h' = moveLeft app
    charHandler app 'l' = moveRight app
    charHandler app _ = app

    moveDown app@App{selectedTbl, tables, selectedRow} =
      let rows = tblRows (tables !! selectedTbl)
          lenRows = length rows
      in if selectedRow > lenRows
           then app{selectedRow=0, redrawMain=True}
           else app{selectedRow=selectedRow + 1, redrawMain=True}

    moveUp app@App{selectedTbl, tables, selectedRow} =
      let rows = tblRows (tables !! selectedTbl)
          lenRows = length rows
      in if selectedRow > 0
           then app{selectedRow=selectedRow - 1, redrawMain=True}
           else app{selectedRow=lenRows, redrawMain=True}

    moveLeft app@App{selectedTbl, tables, selectedCol} =
      let cols = tblCols (tables !! selectedTbl)
          lenCols = length cols
      in if selectedCol > 0
           then app{selectedCol=selectedCol - 1, redrawMain=True}
           else app{selectedCol=lenCols - 1, redrawMain=True}

    moveRight app@App{selectedTbl, tables, selectedCol} =
      let cols = tblCols (tables !! selectedTbl)
          lenCols = length cols
      in if selectedCol < lenCols - 1
           then app{selectedCol=selectedCol + 1, redrawMain=True}
           else app{selectedCol=0, redrawMain=True}


-------------------------------------------------------------------------------
drawLine :: Integer -> Update ()
drawLine = drawLineH (Just $ Glyph ' ' [])


sidebarWidth :: Integer -> Integer
sidebarWidth screenx = floor ((fromIntegral screenx / 100) * 20 :: Double)


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
      drawTables tables selectedTbl selColor (0, 0)

    render
  where
    sw = sidebarWidth screenx

    drawTables [] _ _ _ = return ()
    drawTables (t : ts) selectedTbl selColor (cy, cx) = do
      moveCursor (cy+1) cx

      if selectedTbl == 0 then do
        setColor selColor
        drawLine sw
        drawText $ tblName t
      else do
        setColor defaultColorID
        drawLine sw
        drawText $ tblName t

      drawTables ts (selectedTbl-1) selColor (cy+1, cx)


handleEvents :: App -> Curses App
handleEvents app@App{..} = do
    ev <- getEvent window (Just 10)
    return $ case ev of
               Just (EventCharacter char) -> handleChar focus char app
               Just (EventSpecialKey key) -> handleKey focus key app
               Just EventResized -> app{updateSize=True}
               _ -> app


drawMain :: App -> Curses ()
drawMain App{..} = do
    titleColorID <- newColorID ColorWhite ColorGreen 1
    selColor <- newColorID ColorWhite ColorMagenta 2

    updateWindow window $ do
      clearBox

      moveCursor 0 (sw+1)
      setColor titleColorID
      drawLine mw
      drawText "Table contents"

      moveCursor 1 (sw+1)
      setColor defaultColorID

      drawTexts 1 (sw+1) (take (fromIntegral screeny - 1) dumpTexts) 0 selColor
  where
    clearBox :: Update ()
    clearBox = do
      setColor defaultColorID
      forM_ [0..screeny-1] $ \y -> do
        moveCursor y (sw+1)
        drawLine screenx

    sw = sidebarWidth screenx

    mw = screenx - (sw + 1)

    colw' = colw mw (tables !! selectedTbl)

    dump :: ([Text], [[Text]])
    dump = tblDump $ tables !! selectedTbl

    dumpTexts :: [[Text]]
    dumpTexts = fst dump : snd dump

    drawTexts _ _ [] _ _ = return ()
    drawTexts y x (t : ts) curLine selColor = do
      drawCols y x t curLine 0 selColor
      drawTexts (y+1) x ts (curLine + 1) selColor

    drawCols _ _ [] _ _ _ = return ()
    drawCols y x (t : ts) curLine curCol selColor = do
      moveCursor y x
      when (curLine == selectedRow && curCol == selectedCol) $ setColor selColor
      drawText $ T.take (fromIntegral colw' - 1) t
      when (curLine == selectedRow && curCol == selectedCol) $ setColor defaultColorID
      drawCols y (x + fromIntegral colw') ts curLine (curCol + 1) selColor


updateScreenSize :: StateT App Curses ()
updateScreenSize = do
    (screeny', screenx') <- lift screenSize
    modify $ \app -> app{screeny=screeny', screenx=screenx', updateSize=False}


loop :: StateT App Curses ()
loop = do
    us <- gets updateSize
    when us updateScreenSize

    rs <- gets redrawSidebar
    get >>= when rs . lift . drawSidebar

    rm <- gets redrawMain
    get >>= when rm . lift . drawMain

    get >>= lift . handleEvents >>= put

    exitp <- gets exit
    unless exitp loop


runApp :: App -> Curses ()
runApp app@App{..} = do
    db <- liftIO $ open "sqlitedb"
    tables <- liftIO $ readTables db
    evalStateT loop app{tables=tables, selectedTbl=0}


main :: IO ()
main = runCurses $ do
    setEcho False
    window <- defaultWindow
    (screeny, screenx) <- screenSize
    _ <- setCursorMode CursorInvisible
    runApp $ initialApp screenx screeny window mkSidebarView mkMainView
