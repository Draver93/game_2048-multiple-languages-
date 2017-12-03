import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT
import System.Random
import Data.IORef
import Data.Ratio
import Control.Monad
import System.IO.Unsafe
import Data.Time.Clock.POSIX
import Unsafe.Coerce(unsafeCoerce)
import GHC.Float(double2Float)

doubleToGF = unsafeCoerce . double2Float :: Double -> GLfloat

timeInMicros :: IO Integer
timeInMicros = numerator . toRational . (* 1000000) <$> getPOSIXTime

timeInMillis :: IO Integer
timeInMillis = (`div` 1000) <$> timeInMicros

timeInSeconds :: IO Integer
timeInSeconds = (`div` 1000) <$> timeInMillis

timeInSeconds' :: IO Double
timeInSeconds' = (/ 1000000) . fromIntegral <$> timeInMicros

data PointMyD =
    PointMyD {
        x_d :: Double,
        y_d :: Double}
    deriving (Eq, Show, Read)

data PointMyI =
    PointMyI {
        x_i :: Int,
        y_i :: Int}
    deriving (Eq, Show, Read)

data Tile =
    Tile {
        name :: String,
        pos :: PointMyD,
        size :: PointMyD,
        idt :: PointMyI,
        count :: Int}
    deriving (Eq, Show, Read)

data GameSpace =
    GameSpace {
        nameGS :: String,
        posGS :: PointMyD,
        sizeGS :: PointMyD}
    deriving (Eq, Show, Read)

tiles :: IORef [Tile]
tiles = unsafePerformIO $ newIORef []

listen :: IORef Bool
listen = unsafePerformIO $ newIORef True

addTile :: IORef Bool
addTile = unsafePerformIO $ newIORef False

w_h :: IORef Double
w_h = unsafePerformIO $ newIORef 600
w_w :: IORef Double
w_w = unsafePerformIO $ newIORef 800

moveSpeed :: IORef Double
moveSpeed = unsafePerformIO $ newIORef 2

gS :: IORef GameSpace
gS = unsafePerformIO $ newIORef (GameSpace ("GameSpace") (PointMyD 0.0 0.0) (PointMyD 0.0 0.0))

main = do
    getArgsAndInitialize
    createAWindow "Game 2048 (R - to restart, Esc - to exit)"
    mainLoop

createAWindow windowName = do    
    startGame
    initialWindowSize $= Size 800 600
    initialWindowPosition $= Position 0 0
    createWindow windowName

    displayCallback $= display
    idleCallback $= Just idle      
    reshapeCallback $= Just reshape 
    keyboardMouseCallback $= Just keyboard

display = do
    clear [ColorBuffer]
    draw
    flush

idle = do
    postRedisplay Nothing

reshape size@(Size w h) = do 
    matrixMode $= Projection
    loadIdentity
    let wf = fromIntegral w
        hf = fromIntegral h
    writeIORef w_h hf
    writeIORef w_w wf
    ortho 0.0 wf hf 0.0 (-10) 10
    matrixMode $= Modelview 0
    loadIdentity
    viewport $= (Position 0 0, size)
    postRedisplay Nothing


leftShift x = do
    pt <- readIORef tiles
    loopCount <- newIORef 1
    let tl = pt
    let tSize = length tl
    let tile = tl !! x -- 1 элемент списка

    if x < tSize
        then do
            if (x_i (idt tile)) > 0
                then do 
                    pt <- getObjectByPos (x_i (idt tile) - 1) (y_i (idt tile))
                    let id_obj = pt
                    if id_obj == -1
                        then do 
                            relocateTileTypeTwo (x_i (idt tile)) 
                                                (y_i (idt tile)) 
                                                (x_i (idt tile) - 1) 
                                                (y_i (idt tile))
                            val <- leftShift 0
                            pt <- readIORef loopCount
                            let valLC = pt
                            writeIORef loopCount (val + valLC)
                            return ()
                        else do
                            let tileNxt = tl !! id_obj
                            if ((count tile) == (count tileNxt))
                                then do 
                                    deleteFromGList id_obj                      --Удалить элемент на пути
                                    relocateTileTypeTwo (x_i (idt tile))        --Переместить 
                                                        (y_i (idt tile))    
                                                        (x_i (idt tile) - 1) 
                                                        (y_i (idt tile)) 
                                    
                                    pt <- readIORef tiles
                                    let tl = pt

                                    pt <- getObjectByPos (x_i (idt tile) - 1) (y_i (idt tile))
                                    let id_obj = pt

                                    let tileOld = tl !! id_obj
                                    deleteFromGList id_obj                      --Удалить элемент на пути
                                    let newTile = Tile      (name tileOld) 
                                                            (pos tileOld) 
                                                            (size tileOld) 
                                                            (idt tileOld) 
                                                            ((count tileOld) * 2)
                                    
                                    pt <- readIORef tiles
                                    let tl = pt
                                    let newTiles = tl ++ [newTile]
                                    writeIORef tiles newTiles
                                    val <- leftShift 0
                                    pt <- readIORef loopCount
                                    let valLC = pt
                                    writeIORef loopCount (val + valLC)
                                    return ()
                                else do
                                    pt <- readIORef tiles
                                    let listTl = pt
                                    let size = length listTl
                                    if (x + 1) < size
                                        then do
                                            val <- leftShift (x + 1)
                                            pt <- readIORef loopCount
                                            let valLC = pt
                                            writeIORef loopCount (val + valLC)
                                            return ()
                                        else return()
                                    return ()
                            return ()
                    return ()
                else do
                    pt <- readIORef tiles
                    let listTl = pt
                    let size = length listTl
                    if (x + 1) < size
                        then do
                            val <- leftShift (x + 1)
                            pt <- readIORef loopCount
                            let valLC = pt
                            writeIORef loopCount (val + valLC)
                            return ()
                        else return()
                    return ()
            return()
        else return()
    pt <- readIORef loopCount
    let lc = pt
    return(lc)


rightShift x = do 
    pt <- readIORef tiles
    loopCount <- newIORef 1
    let tl = pt
    let tSize = length tl
    let tile = tl !! x -- 1 элемент списка

    if x < tSize
        then do
            if (x_i (idt tile)) < 3
                then do 
                    pt <- getObjectByPos (x_i (idt tile) + 1) (y_i (idt tile))
                    let id_obj = pt
                    if id_obj == -1
                        then do 
                            relocateTileTypeTwo (x_i (idt tile)) 
                                                (y_i (idt tile)) 
                                                (x_i (idt tile) + 1) 
                                                (y_i (idt tile))
                            val <- rightShift 0
                            pt <- readIORef loopCount
                            let valLC = pt
                            writeIORef loopCount (val + valLC)
                            return ()
                        else do
                            let tileNxt = tl !! id_obj
                            if ((count tile) == (count tileNxt))
                                then do 
                                    deleteFromGList id_obj                      --Удалить элемент на пути
                                    relocateTileTypeTwo (x_i (idt tile))        --Переместить 
                                                        (y_i (idt tile))    
                                                        (x_i (idt tile) + 1) 
                                                        (y_i (idt tile)) 
                                    
                                    pt <- readIORef tiles
                                    let tl = pt

                                    pt <- getObjectByPos (x_i (idt tile) + 1) (y_i (idt tile))
                                    let id_obj = pt

                                    let tileOld = tl !! id_obj
                                    deleteFromGList id_obj                      --Удалить элемент на пути
                                    let newTile = Tile      (name tileOld) 
                                                            (pos tileOld) 
                                                            (size tileOld) 
                                                            (idt tileOld) 
                                                            ((count tileOld) * 2)
                                    
                                    pt <- readIORef tiles
                                    let tl = pt
                                    let newTiles = tl ++ [newTile]
                                    writeIORef tiles newTiles
                                    val <- rightShift 0
                                    pt <- readIORef loopCount
                                    let valLC = pt
                                    writeIORef loopCount (val + valLC)
                                    return ()
                                else do
                                    pt <- readIORef tiles
                                    let listTl = pt
                                    let size = length listTl
                                    if (x + 1) < size
                                        then do
                                            val <- rightShift (x + 1)
                                            pt <- readIORef loopCount
                                            let valLC = pt
                                            writeIORef loopCount (val + valLC)
                                            return ()
                                        else return()
                                    return ()
                            return ()
                    return ()
                else do
                    pt <- readIORef tiles
                    let listTl = pt
                    let size = length listTl
                    if (x + 1) < size
                        then do
                            val <- rightShift (x + 1)
                            pt <- readIORef loopCount
                            let valLC = pt
                            writeIORef loopCount (val + valLC)
                            return ()
                        else return()
                    return ()
            return()
        else return()
    pt <- readIORef loopCount
    let lc = pt
    return(lc)

upShift x = do
    pt <- readIORef tiles
    loopCount <- newIORef 1
    let tl = pt
    let tSize = length tl
    let tile = tl !! x -- 1 элемент списка

    if x < tSize
        then do
            if (y_i (idt tile)) > 0
                then do 
                    pt <- getObjectByPos (x_i (idt tile)) (y_i (idt tile) - 1)
                    let id_obj = pt
                    if id_obj == -1
                        then do 
                            relocateTileTypeTwo (x_i (idt tile)) 
                                                (y_i (idt tile)) 
                                                (x_i (idt tile)) 
                                                (y_i (idt tile) - 1)
                            val <- upShift 0
                            pt <- readIORef loopCount
                            let valLC = pt
                            writeIORef loopCount (val + valLC)
                            return ()
                        else do
                            let tileNxt = tl !! id_obj
                            if ((count tile) == (count tileNxt))
                                then do 
                                    deleteFromGList id_obj                      --Удалить элемент на пути
                                    relocateTileTypeTwo (x_i (idt tile))        --Переместить 
                                                        (y_i (idt tile))    
                                                        (x_i (idt tile)) 
                                                        (y_i (idt tile) - 1) 
                                    
                                    pt <- readIORef tiles
                                    let tl = pt

                                    pt <- getObjectByPos (x_i (idt tile)) (y_i (idt tile) - 1)
                                    let id_obj = pt

                                    let tileOld = tl !! id_obj
                                    deleteFromGList id_obj                      --Удалить элемент на пути
                                    let newTile = Tile      (name tileOld) 
                                                            (pos tileOld) 
                                                            (size tileOld) 
                                                            (idt tileOld) 
                                                            ((count tileOld) * 2)
                                    
                                    pt <- readIORef tiles
                                    let tl = pt
                                    let newTiles = tl ++ [newTile]
                                    writeIORef tiles newTiles
                                    val <- upShift 0
                                    pt <- readIORef loopCount
                                    let valLC = pt
                                    writeIORef loopCount (val + valLC)
                                    return ()
                                else do
                                    pt <- readIORef tiles
                                    let listTl = pt
                                    let size = length listTl
                                    if (x + 1) < size
                                        then do
                                            val <- upShift (x + 1)
                                            pt <- readIORef loopCount
                                            let valLC = pt
                                            writeIORef loopCount (val + valLC)
                                            return ()
                                        else return()
                                    return ()
                            return ()
                    return ()
                else do
                    pt <- readIORef tiles
                    let listTl = pt
                    let size = length listTl
                    if (x + 1) < size
                        then do
                            val <- upShift (x + 1)
                            pt <- readIORef loopCount
                            let valLC = pt
                            writeIORef loopCount (val + valLC)
                            return ()
                        else return()
                    return ()
            return()
        else return()
    pt <- readIORef loopCount
    let lc = pt
    return(lc)

downShift x = do
    pt <- readIORef tiles
    loopCount <- newIORef 1
    let tl = pt
    let tSize = length tl
    let tile = tl !! x -- 1 элемент списка

    if x < tSize
        then do
            if (y_i (idt tile)) < 3
                then do 
                    pt <- getObjectByPos (x_i (idt tile)) (y_i (idt tile) + 1)
                    let id_obj = pt
                    if id_obj == -1
                        then do 
                            relocateTileTypeTwo (x_i (idt tile)) 
                                                (y_i (idt tile)) 
                                                (x_i (idt tile)) 
                                                (y_i (idt tile) + 1)
                            val <- downShift 0
                            pt <- readIORef loopCount
                            let valLC = pt
                            writeIORef loopCount (val + valLC)
                            return ()
                        else do
                            let tileNxt = tl !! id_obj
                            if ((count tile) == (count tileNxt))
                                then do 
                                    deleteFromGList id_obj                      --Удалить элемент на пути
                                    relocateTileTypeTwo (x_i (idt tile))        --Переместить 
                                                        (y_i (idt tile))    
                                                        (x_i (idt tile)) 
                                                        (y_i (idt tile) + 1) 
                                    
                                    pt <- readIORef tiles
                                    let tl = pt

                                    pt <- getObjectByPos (x_i (idt tile)) (y_i (idt tile) + 1)
                                    let id_obj = pt

                                    let tileOld = tl !! id_obj
                                    deleteFromGList id_obj                      --Удалить элемент на пути
                                    let newTile = Tile      (name tileOld) 
                                                            (pos tileOld) 
                                                            (size tileOld) 
                                                            (idt tileOld) 
                                                            ((count tileOld) * 2)
                                    
                                    pt <- readIORef tiles
                                    let tl = pt
                                    let newTiles = tl ++ [newTile]
                                    writeIORef tiles newTiles
                                    val <- downShift 0
                                    pt <- readIORef loopCount
                                    let valLC = pt
                                    writeIORef loopCount (val + valLC)
                                    return ()
                                else do
                                    pt <- readIORef tiles
                                    let listTl = pt
                                    let size = length listTl
                                    if (x + 1) < size
                                        then do
                                            val <- downShift (x + 1)
                                            pt <- readIORef loopCount
                                            let valLC = pt
                                            writeIORef loopCount (val + valLC)
                                            return ()
                                        else return()
                                    return ()
                            return ()
                    return ()
                else do
                    pt <- readIORef tiles
                    let listTl = pt
                    let size = length listTl
                    if (x + 1) < size
                        then do
                            val <- downShift (x + 1)
                            pt <- readIORef loopCount
                            let valLC = pt
                            writeIORef loopCount (val + valLC)
                            return ()
                        else return()
                    return ()
            return()
        else return()
    pt <- readIORef loopCount
    let lc = pt
    return(lc)

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard (SpecialKey KeyLeft) Down _ _ = do
    locListen <- readIORef listen
    let ll = locListen
    if ll
        then do
            pt <- readIORef tiles
            let tl = pt
            let size = length tl
            num <- leftShift 0 -- Возвращаемое значение говорит о количестве циклов
            if num > size
                then do
                    writeIORef addTile True
                    return ()
                else return ()
            return()
        else return()
    return()

keyboard (SpecialKey KeyUp) Down _ _ = do
    locListen <- readIORef listen
    let ll = locListen
    if ll
        then do
            pt <- readIORef tiles
            let tl = pt
            let size = length tl
            num <- upShift 0 -- Возвращаемое значение говорит о количестве циклов
            if num > size
                then do
                    writeIORef addTile True
                    return ()
                else return ()
            return()
        else return()
    return()


keyboard (SpecialKey KeyRight) Down _ _ = do
    locListen <- readIORef listen
    let ll = locListen
    if ll
        then do
            pt <- readIORef tiles
            let tl = pt
            let size = length tl
            num <- rightShift 0 -- Возвращаемое значение говорит о количестве циклов
            if num > size
                then do
                    writeIORef addTile True
                    return ()
                else return ()
            return()
        else return()
    return()

keyboard (SpecialKey KeyDown) Down _ _ = do
    locListen <- readIORef listen
    let ll = locListen
    if ll
        then do
            pt <- readIORef tiles
            let tl = pt
            let size = length tl
            num <- downShift 0 -- Возвращаемое значение говорит о количестве циклов
            if num > size
                then do
                    writeIORef addTile True
                    return ()
                else return ()
            return()
        else return()
    return()

keyboard (Char 'r') Down _ _ = do
    startGame
    return()
keyboard _            _    _ _ = return ()

getObjectByPos x y = do
    pt <- readIORef tiles
    let tl = pt -- Список элементов
    let tLn = length tl
    finalVal <- newIORef (-1)
    forM_ [0..(tLn - 1)] $ \i -> do
        let tile = tl !! i -- 1 элемент списка
        if (((x_i (idt tile)) == (x :: Int)) && ((y_i (idt tile)) == (y :: Int)))
            then do 
                writeIORef finalVal i
                return ()
            else return ()
    pt <- readIORef finalVal
    let result = pt
    return (pt)

getObjectByPosLoc x y tl = do
    let tLn = length tl
    finalVal <- newIORef (-1)
    forM_ [0..(tLn - 1)] $ \i -> do
        let tile = tl !! i -- 1 элемент списка
        if (((x_i (idt tile)) == (x :: Int)) && ((y_i (idt tile)) == (y :: Int)))
            then do 
                writeIORef finalVal i
                return ()
            else return ()
    pt <- readIORef finalVal
    let result = pt
    return (pt)

deleteFromList :: Int -> [Tile] -> [Tile]
deleteFromList idt lst = do
    let hdList = take idt lst
    let tlList = drop (idt + 1) lst
    let finalList = hdList ++ tlList
    finalList

deleteFromGList idt = do
    pt <- readIORef tiles
    let lst = pt
    let size = length lst
    let hdList = if idt > 0
        then take idt lst
        else []
    
    let tlList = if idt < size
        then drop (idt + 1) lst
        else []
    let finalList = hdList ++ tlList
    writeIORef tiles finalList
    return()

startGame = do
    writeIORef tiles []

    pt <- readIORef w_h
    let h = pt :: Double

    pt <- readIORef w_w
    let w = pt :: Double
    
    let size = PointMyD (h/1.2)  (h/1.2)
    let pos = PointMyD (w/2.0 - (x_d size)/2.0)  (h/2.0 - (y_d size)/2.0)

    let newGS = GameSpace "BackGround" pos size

    writeIORef gS newGS

    forM_ [0..1] $ \i -> do
        createRndTile

    return ()

createRndTile = do 

    pt <- readIORef tiles
    let tl = pt 
    let countTiles = 16 - (length tl)
    pt <- randomRIO (0, (countTiles - 1) :: Int)
    let loc_pos = pt
    lp <- newIORef loc_pos 
    forM_ [0..3] $ \i -> do
        forM_ [0..3] $ \j -> do
            pt <- getObjectByPos i j
            let num = pt
            if num == (-1)
                then do 
                    pt <- readIORef lp
                    let loc_p = pt
                    if loc_p == 0
                        then do
                            createTile i j
                            return()
                        else return()
                    writeIORef lp (loc_p - 1)
                    return()
                else return()
    return ()

createTile i j = do

    pt <- readIORef gS
    let gs = pt 
    let fSize   = PointMyD (x_d (sizeGS gs))  (y_d (sizeGS gs))
    let fPos    = PointMyD (x_d (posGS gs))   (y_d (posGS gs))
    -----------------------------------------------------------------------------------
    let size = PointMyD ((x_d fSize)/4.0 - 12.5)  
                        ((x_d fSize)/4.0 - 12.5) 

    let pos = PointMyD  ((x_d fPos) + (((x_d size) + 10.0) * (fromIntegral i)) + 10)
                        ((y_d fPos) + (((y_d size) + 10.0) * (fromIntegral j)) + 10)

    let idt = PointMyI i j

    pt <- randomRIO (0, 100 :: Int)
    let rnd = pt
    
    let count = if rnd > 90
        then 4
        else 2
    -----------------------------------------------------------------------------------

    let tile = Tile "TILE" pos size idt count

    pt <- readIORef tiles
    let tl = pt 
    let newTiles = tl ++ [tile]

    writeIORef tiles newTiles

    return ()

relocateTile idt i j = do

    pt <- readIORef tiles
    let tl = pt -- Список элементов
    let tile = tl !! idt
    let newIdt = PointMyI i j
    let newTile = Tile (name tile) (pos tile) (size tile) newIdt (count tile)
    let newTiles = deleteFromList idt tl 

    let finalList = newTiles ++ [newTile]
    writeIORef tiles finalList

    return()

relocateTileTypeTwo i_1 j_1 i_2 j_2 = do 
    tmpFirstTile <- newIORef (Tile ("NULL") (PointMyD 0.0 0.0) (PointMyD 0.0 0.0) (PointMyI 0 0) 0)
    numTile <- newIORef (-1)
    ---
    pTiles <- readIORef tiles
    let tl = pTiles -- Список элементов
    let tLn = length tl -- Длинна списка
    ---

    forM_ [0..(tLn - 1)] $ \i -> do
        let tmp_tile =  tl !! i
        let tmp_idt = (idt tmp_tile)
        if (((x_i tmp_idt) == i_1) && ((y_i tmp_idt) == j_1))
            then do
                writeIORef tmpFirstTile tmp_tile
                writeIORef numTile i
                return()
            else return()

    
    pt <- readIORef tmpFirstTile
    let tFT = pt

    if (count tFT) > 0
        then do
            let newIdt = PointMyI i_2 j_2
            let newTile = Tile (name tFT) (pos tFT) (size tFT) newIdt (count tFT)

            pt <- readIORef numTile
            let num = pt
            let newTiles = deleteFromList num tl 

            let finalList = newTiles ++ [newTile]
            writeIORef tiles finalList
            return()
        else return ()

    return()

move ident dt = do
    move_acitve <- newIORef False

    pt <- readIORef tiles
    let tl = pt

    pt <- readIORef gS
    let gs = pt

    pt <- readIORef moveSpeed
    let mS = pt

    let tile    = tl !! ident
    let tSize   = (size tile)
    let tId     = (idt tile)
    let tPos    = (pos tile)
    let fPos    = (posGS gs)
        
    let pt_x = ((x_d fPos) + (((x_d tSize) + 10.0) * (fromIntegral (x_i tId))) + 10)
    let pt_y = ((y_d fPos) + (((y_d tSize) + 10.0) * (fromIntegral (y_i tId))) + 10)
    tmpTile <- newIORef tile
    --Пока что события не учитывают действия других
    if ((x_d tPos) == pt_x)
        then return ()
        else do
            writeIORef move_acitve True
            if pt_x - (x_d tPos) > 0
                then do 
                    pt <- readIORef tmpTile
                    let oldTile = pt
                    let oldPos = (pos oldTile)
                    writeIORef tmpTile (Tile    (name oldTile) 
                                                (PointMyD ((x_d oldPos) + mS * dt) (y_d oldPos))   --Изменяем
                                                (size oldTile) 
                                                (idt oldTile) 
                                                (count oldTile))
                    return ()
                else do 
                    pt <- readIORef tmpTile
                    let oldTile = pt
                    let oldPos = (pos oldTile)
                    writeIORef tmpTile (Tile    (name oldTile) 
                                                (PointMyD ((x_d oldPos) - mS * dt) (y_d oldPos))  --Изменяем
                                                (size oldTile) 
                                                (idt oldTile) 
                                                (count oldTile))
                    return ()
            return ()

    if ((y_d tPos) == pt_y)
        then return ()
        else do
            writeIORef move_acitve True
            if pt_y - (y_d tPos) > 0
                then do 
                    pt <- readIORef tmpTile
                    let oldTile = pt
                    let oldPos = (pos oldTile)
                    writeIORef tmpTile (Tile    (name oldTile) 
                                                (PointMyD (x_d oldPos) ((y_d oldPos) + mS * dt))   --Изменяем
                                                (size oldTile) 
                                                (idt oldTile) 
                                                (count oldTile))
                    return ()
                else do 
                    pt <- readIORef tmpTile
                    let oldTile = pt
                    let oldPos = (pos oldTile)
                    writeIORef tmpTile (Tile    (name oldTile) 
                                                (PointMyD (x_d oldPos) ((y_d oldPos) - mS * dt))   --Изменяем
                                                (size oldTile) 
                                                (idt oldTile) 
                                                (count oldTile))
                    return ()
            return ()    
    
    if (abs ((y_d tPos) - pt_y)) <= (mS * dt)
        then do 
            pt <- readIORef tmpTile
            let oldTile = pt
            let oldPos = (pos oldTile)
            writeIORef tmpTile (Tile    (name oldTile) 
                                        (PointMyD (x_d oldPos) (pt_y))   --Изменяем
                                        (size oldTile) 
                                        (idt oldTile) 
                                        (count oldTile))
            return()
        else return()

    if (abs ((x_d tPos) - pt_x)) <= (mS * dt)
        then do 
            pt <- readIORef tmpTile
            let oldTile = pt
            let oldPos = (pos oldTile)
            writeIORef tmpTile (Tile    (name oldTile) 
                                        (PointMyD (pt_x) (y_d oldPos))   --Изменяем
                                        (size oldTile) 
                                        (idt oldTile) 
                                        (count oldTile))
            return()
        else return()
    pt <- readIORef move_acitve
    let mActive = pt
    if mActive
        then do 
            deleteFromGList ident
            
            pt <- readIORef tmpTile
            let newTile = pt

            pt <- readIORef tiles
            let tl = pt

            let newTiles = tl ++ [newTile]
            writeIORef tiles newTiles
            return()
        else return()
    pt <- readIORef move_acitve
    let ma = pt
    return(ma)

draw = do  
    pt <- readIORef tiles
    let lst = pt

    pt <- timeInSeconds'
    let intTime = pt
    let dt = intTime / 10000000000.0
 
    pt <- readIORef gS
    let gs = pt
    drawQuadF (posGS gs) (sizeGS gs)

    let tLn = length lst
    mActive <- newIORef False

    forM_ [0..tLn - 1] $ \i -> do
        let tile = lst !! i
        moveActive <- move i dt
        if moveActive == True
            then do 
                writeIORef mActive True
                return()
            else return()

        drawQuad (pos tile) (size tile) (count tile)

    pt <- readIORef mActive
    let mA = pt
    if mA 
        then do 
            writeIORef listen False
            return()
        else do
            writeIORef listen True
            return()
    pt <- readIORef addTile
    let aT = pt
    if (aT && not (mA))
        then do   
            createRndTile
            writeIORef addTile False
            return()
        else return()

    return ()

drawQuad pos size count = do
    let color3f r g b = color $ Color3 r g (b :: GLfloat)
        vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
    currentColor $= Color4 0 0.3 1 1

    let pX = (doubleToGF (x_d pos)) 
    let pY = (doubleToGF (y_d pos)) 

    let sX = (doubleToGF (x_d size)) 
    let sY = (doubleToGF (y_d size)) 
    let clr = 1.0 - ((logBase 2 (fromIntegral count)) - 1) / 10
    renderPrimitive Quads $ do
        color3f clr clr clr
        vertex3f pX pY 0
        vertex3f pX (pY + sY) 0
        vertex3f (pX + sX) (pY + sY) 0
        vertex3f (pX + sX) pY 0
    return ()
drawQuadF pos size = do
    let color3f r g b = color $ Color3 r g (b :: GLfloat)
        vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
    currentColor $= Color4 0 0.3 1 1

    let pX = (doubleToGF (x_d pos)) 
    let pY = (doubleToGF (y_d pos)) 

    let sX = (doubleToGF (x_d size)) 
    let sY = (doubleToGF (y_d size)) 

    renderPrimitive Quads $ do
        color3f 0.2 0.2 0.6
        vertex3f pX pY 0
        vertex3f pX (pY + sY) 0
        vertex3f (pX + sX) (pY + sY) 0
        vertex3f (pX + sX) pY 0
    return ()