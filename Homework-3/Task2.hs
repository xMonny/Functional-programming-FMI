module Task2 where
    import Data.Word
    import Data.List
    import System.IO
    import System.FilePath
    import Data.Char

    data Rgb = Rgb { red   :: Word8
                   , green :: Word8
                   , blue  :: Word8 } deriving (Show, Read, Eq)

    data Image = Image { width   :: Int
                       , height  :: Int
                       , content :: [[Rgb]] } deriving (Show, Read, Eq)

    baseImage :: Image
    baseImage = Image 0 0 []

    numberRows :: [[a]] -> Int
    numberRows [] = 0
    numberRows matrix = length matrix

    numberColumns :: [[a]] -> Int
    numberColumns [] = 0
    numberColumns matrix
        | not (validRowLength matrix (length (head matrix))) = error "Input content is not valid: Different number of columns"
        | otherwise = length (head matrix)

    validRowLength :: [[a]] -> Int -> Bool
    validRowLength [] _ = False
    validRowLength matrix width
        | length rowsWithWidth /= length matrix = False
        | otherwise = True
        where rowsWithWidth = [row | row <- matrix, length row == width]


    validImage :: Image -> Bool
    validImage (Image width height content)
        | Image width height content == baseImage = True
        | numberRows content /= height = False
        | validRowLength content width = True
        | otherwise                    = False

    --A) grayscale
    grayscaleValue :: Rgb -> Word8
    grayscaleValue (Rgb red green blue) = round (fromInteger 
                                                    (30 * toInteger red 
                                                    + 59 * toInteger green 
                                                    + 11 * toInteger blue) / 100) :: Word8

    createGrayscale :: Word8 -> Rgb
    createGrayscale value = Rgb value value value

    convertToGrayscale :: [Rgb] -> [Rgb]
    convertToGrayscale = map (createGrayscale . grayscaleValue)

    makeGrayscale :: [[Rgb]] -> [[Rgb]]
    makeGrayscale = map convertToGrayscale

    grayscale :: Image -> Image
    grayscale (Image width height content)
        | not $ validImage $ Image width height content = error "grayscale: Input image is not valid"
        | otherwise = Image width height $ makeGrayscale content

    --Б) edgeDetect
    getElement :: Int -> Int -> [Rgb] -> Rgb
    getElement pos curr (elem : arr)
        | pos == curr = elem
        | otherwise = getElement pos (curr + 1) arr

    getRgb :: Int -> Int -> Int -> [[Rgb]] -> Rgb
    getRgb row column currRow (pixels : content)
        | currRow == row = getElement column 1 pixels
        | otherwise = getRgb row column (currRow + 1) content

    getPixel :: Int -> Int -> [[Rgb]] -> Rgb
    getPixel row column content
        | row < 1 || row > numberRows content = createGrayscale 0
        | column < 1 || column > numberColumns content = createGrayscale 0
        | otherwise = getRgb row column 1 content

    isGrayscalePixel :: Rgb -> Bool
    isGrayscalePixel (Rgb red green blue)
        | red == green && green == blue = True 
        | otherwise = False

    isGrayscaleRow :: [Rgb] -> Bool
    isGrayscaleRow [] = True
    isGrayscaleRow (pixel : row)
        | isGrayscalePixel pixel = isGrayscaleRow row
        | otherwise = False

    isGrayscaleContent :: [[Rgb]] -> Bool
    isGrayscaleContent [] = True
    isGrayscaleContent (row : content)
        | isGrayscaleRow row = isGrayscaleContent content
        | otherwise = False

    isGrayscaleImage :: Image -> Bool
    isGrayscaleImage (Image width height content)
        | not (validImage (Image width height content)) = False
        | otherwise = isGrayscaleContent content

    getGrayscaleValue :: Rgb -> Word8
    getGrayscaleValue (Rgb red green blue)
        | isGrayscalePixel (Rgb red green blue) = red
        | otherwise = 0

    getValueFloat :: Int -> Int -> [[Rgb]] -> Float
    getValueFloat row col content = fromIntegral (getGrayscaleValue (getPixel row col content)) :: Float

    right :: Int -> Int -> [[Rgb]] -> Float
    right row col = getValueFloat row (col+1)

    left :: Int -> Int -> [[Rgb]] -> Float
    left row col = getValueFloat row (col-1)

    up :: Int -> Int -> [[Rgb]] -> Float
    up row = getValueFloat (row-1)

    down :: Int -> Int -> [[Rgb]] -> Float
    down row = getValueFloat (row+1)

    upRight :: Int -> Int -> [[Rgb]] -> Float
    upRight row col = getValueFloat (row-1) (col+1)

    upLeft :: Int -> Int -> [[Rgb]] -> Float
    upLeft row col = getValueFloat (row-1) (col-1)

    downRight :: Int -> Int -> [[Rgb]] -> Float
    downRight row col = getValueFloat (row+1) (col+1)

    downLeft :: Int -> Int -> [[Rgb]] -> Float
    downLeft row col = getValueFloat (row+1) (col-1)

    upLeftCornerMatrix :: [[Rgb]] -> [[Float]]
    upLeftCornerMatrix content = [[corner,     vertical, corner    ]
                                , [horizontal, current,  horizontal]
                                , [corner,     vertical, corner   ]]
                                where row = 1
                                      col = 1
                                      current = getValueFloat row col content
                                      corner = downRight row col content
                                      horizontal = right row col content
                                      vertical = down row col content

    upRightCornerMatrix :: [[Rgb]] -> [[Float]]
    upRightCornerMatrix content = [[corner,     vertical, corner   ]
                                , [horizontal, current,  horizontal]
                                , [corner,     vertical, corner   ]]
                                where row = 1
                                      col = numberColumns content
                                      current = getValueFloat row col content
                                      corner = downLeft row col content
                                      horizontal = left row col content
                                      vertical = down row col content

    downLeftCornerMatrix :: [[Rgb]] -> [[Float]]
    downLeftCornerMatrix content = [[corner,   vertical, corner    ]
                                , [horizontal, current,  horizontal]
                                , [corner,     vertical, corner   ]]
                                where row = numberRows content
                                      col = 1
                                      current = getValueFloat row col content
                                      corner = upRight row col content
                                      horizontal = right row col content
                                      vertical = up row col content

    downRightCornerMatrix :: [[Rgb]] -> [[Float]]
    downRightCornerMatrix content = [[corner,     vertical, corner    ]
                                   , [horizontal, current,  horizontal]
                                   , [corner,     vertical, corner   ]]
                                  where row = numberRows content
                                        col = numberColumns content
                                        current = getValueFloat row col content
                                        corner = upLeft row col content
                                        horizontal = left row col content
                                        vertical = up row col content

    fillUpMatrix :: Int -> [[Rgb]] -> [[Float]]
    fillUpMatrix column content =  [[downLeftCorner, downPixel, downRightCorner ]
                                  , [leftPixel,      current,   rightPixel      ]
                                  , [downLeftCorner, downPixel, downRightCorner]]
                                  where row = 1
                                        col = column
                                        current = getValueFloat row col content
                                        downPixel = down row col content
                                        downLeftCorner = downLeft row col content
                                        downRightCorner = downRight row col content
                                        leftPixel = left row col content
                                        rightPixel = right row col content

    fillDownMatrix :: Int -> [[Rgb]] -> [[Float]]
    fillDownMatrix column content =  [[upLeftCorner, upPixel, upRightCorner ]
                                    , [leftPixel,    current,   rightPixel  ]
                                    , [upLeftCorner, upPixel, upRightCorner]]
                                    where row = numberRows content
                                          col = column
                                          current = getValueFloat row col content
                                          upPixel = up row col content
                                          upLeftCorner = upLeft row col content
                                          upRightCorner = upRight row col content
                                          leftPixel = left row col content
                                          rightPixel = right row col content

    fillLeftMatrix :: Int -> [[Rgb]] -> [[Float]]
    fillLeftMatrix r content =  [[upRightCorner,   upPixel,   upRightCorner   ]
                               , [rightPixel,      current,   rightPixel      ]
                               , [downRightCorner, downPixel, downRightCorner]]
                               where row = r
                                     col = 1
                                     current = getValueFloat row col content
                                     upPixel = up row col content
                                     downPixel = down row col content
                                     upRightCorner = upRight row col content
                                     downRightCorner = downRight row col content
                                     rightPixel = right row col content

    fillRightMatrix :: Int -> [[Rgb]] -> [[Float]]
    fillRightMatrix r content =  [[upLeftCorner,  upPixel,   upLeftCorner    ]
                                , [leftPixel,      current,   leftPixel      ]
                                , [downLeftCorner, downPixel, downLeftCorner]]
                                where row = r
                                      col = numberColumns content
                                      current = getValueFloat row col content
                                      upPixel = up row col content
                                      downPixel = down row col content
                                      upLeftCorner = upLeft row col content
                                      downLeftCorner = downLeft row col content
                                      leftPixel = left row col content

    usualMatrix :: Int -> Int -> [[Rgb]] -> [[Float]]
    usualMatrix r col content = [[upLeftCorner,   upPixel,     upRightCorner ]
                               , [leftPixel,      current,      rightPixel   ]
                               , [downLeftCorner, downPixel, downRightCorner]]
                                where row = r
                                      column = col
                                      current = getValueFloat row col content
                                      upPixel = up row col content
                                      downPixel = down row col content
                                      upLeftCorner = upLeft row col content
                                      downLeftCorner = downLeft row col content
                                      upRightCorner = upRight row col content
                                      downRightCorner = downRight row col content
                                      leftPixel = left row col content
                                      rightPixel = right row col content

    --create 3x3 matrix for every pixel
    pixelMatrix :: Int -> Int -> [[Rgb]] -> [[Float]]
    pixelMatrix row col content
        | row == 1 && col == 1                = upLeftCornerMatrix content
        | row == 1 && col == maxColumns       = upRightCornerMatrix content
        | row == maxRows && col == 1          = downLeftCornerMatrix content
        | row == maxRows && col == maxColumns = downRightCornerMatrix content
        | row == 1 && col < maxColumns        = fillUpMatrix col content
        | row == maxRows && col < maxColumns  = fillDownMatrix col content
        | row < maxRows && col == 1           = fillLeftMatrix row content
        | row < maxRows && col == maxColumns  = fillRightMatrix row content
        | otherwise                           = usualMatrix row col content
        where maxRows    = numberRows content
              maxColumns = numberColumns content

    gX :: [[Float]]
    gX = [[1, 0, -1]
        , [2, 0, -2]
        , [1, 0, -1]]

    gY :: [[Float]]
    gY = [[1, 2, 1]
        , [0, 0, 0]
        , [-1, -2, -1]]

    multiplyScalarRows :: [Float] -> [Float] -> Float
    multiplyScalarRows [] [] = 0.0
    multiplyScalarRows (a : rowA) (b : rowB) = a*b + multiplyScalarRows rowA rowB

    multiplyScalarMatrix :: [[Float]] -> [[Float]] -> Float
    multiplyScalarMatrix [] [] = 0.0
    multiplyScalarMatrix (rowA : matrixA) (rowB : matrixB) 
        | numberRows matrixA /= numberRows matrixB = error "edgeDetect: Error in calculating (different number of content rows)"
        | length rowA /= length rowB = error "edgeDetect: Error in calculating (different number of content columns)"
        | otherwise = multiplyScalarRows rowA rowB 
                    + multiplyScalarMatrix matrixA matrixB

    newGrayscaleValue :: Float -> Float -> Float
    newGrayscaleValue first second 
        | value > 255 = 255
        | otherwise = value
        where value = sqrt(first*first + second*second)

    convertRowToNewGrayscale :: [[Rgb]] -> [Rgb] -> Int -> Int -> [Rgb]
    convertRowToNewGrayscale _ [] _ _ = []
    convertRowToNewGrayscale content (pixel : row) r col = createGrayscale newValue : convertRowToNewGrayscale content row r (col+1)
                                                    where currentMatrix = pixelMatrix r col content
                                                          first = multiplyScalarMatrix gX currentMatrix
                                                          second = multiplyScalarMatrix gY currentMatrix
                                                          newValue = round (newGrayscaleValue first second) :: Word8
                                                                  

    convertContentToNewGrayscale :: [[Rgb]] -> [[Rgb]] -> Int -> [[Rgb]]
    convertContentToNewGrayscale [] _ _ = []
    convertContentToNewGrayscale (row : content) unmodifiedContent r = convertRowToNewGrayscale unmodifiedContent row r 1 :
                                                                    convertContentToNewGrayscale content unmodifiedContent (r+1)

    edgeDetect :: Image -> Image
    edgeDetect (Image width height content)
        | not $ validImage $ Image width height content = error "edgeDetect: Input image is not valid"
        | not $ isGrayscaleContent content = error "edgeDetect: Input image is not grayscale"
        | otherwise = Image width height $ convertContentToNewGrayscale content content 1

    --В) flood fill

    --collect coordinates of pixels, which have to be flood-filled
    getCoordinatesToChange :: Rgb -> Rgb -> Int -> Int -> [[Rgb]] -> [[Int]] -> [[Int]]
    getCoordinatesToChange newColor prevColor row col content visited
        | [row,col] `elem` visited = []
        | row < 1 || row > numberRows content = []
        | col < 1 || col > numberColumns content = []
        | getPixel row col content /= prevColor = []
        | getPixel row col content == newColor = []
        | otherwise = [[row,col]] 
                  ++ getCoordinatesToChange newColor prevColor (row-1) col content (visited ++ [[row,col]])
                  ++ getCoordinatesToChange newColor prevColor (row+1) col content (visited ++ [[row,col]])
                  ++ getCoordinatesToChange newColor prevColor row (col+1) content (visited ++ [[row,col]])
                  ++ getCoordinatesToChange newColor prevColor row (col-1) content (visited ++ [[row,col]])

    unique :: [[Int]] -> [[Int]]
    unique = reverse . nub . reverse

    changePixel :: Int -> Int -> Rgb -> [[Rgb]] -> [[Rgb]]
    changePixel row col newColor content = firstPart ++ changed ++ otherPart
        where firstPart = take (row-1) content
              changed = [take (col-1) rowToChange
                        ++ [newColor]
                        ++ drop col rowToChange]
              rowToChange = last $ take row content
              otherPart = drop row content

    makeChange :: [[Int]] -> Rgb -> [[Rgb]] -> [[Rgb]]
    makeChange [] newColor content = content
    makeChange (pair : coordinates) newColor content = makeChange coordinates newColor changedContent
        where row = head pair
              col = last pair
              changedContent = changePixel row col newColor content

    makeFloodFill :: Rgb -> Int -> Int -> [[Rgb]] -> [[Rgb]]
    makeFloodFill newColor row col content = makeChange coordinatesToChange newColor content
        where prevColor = getPixel row col content
              visited = []
              coordinatesToChange = unique $ getCoordinatesToChange newColor prevColor row col content visited
                  
    floodFill :: Rgb -> Int -> Int -> Image -> Image
    floodFill color x y (Image width height content)
        | not (validImage (Image width height content)) = error "floodFill: Input image is not valid"
        | otherwise = Image width height $ makeFloodFill color x y content

    --Г) save image
    writeRow :: FilePath -> [Rgb] -> IO()
    writeRow _ [] = return ()
    writeRow path ((Rgb red green blue) : row) = do
                            appendFile path (show red ++ (' ' : show green) ++ (' ' : show blue) ++ "\n")
                            writeRow path row

    writeContent :: FilePath -> [[Rgb]] -> IO()
    writeContent _ [] = return ()
    writeContent path (row : content) = do
                                    writeRow path row
                                    writeContent path content

    writeInFile :: FilePath -> Image -> IO()
    writeInFile path (Image width height content) = do
                                            writeFile path ("P3" ++ "\n")
                                            appendFile path (show width ++ " " ++ show height ++ "\n")
                                            appendFile path ("255" ++ "\n")
                                            writeContent path content

    saveImage :: FilePath -> Image -> IO()
    saveImage path (Image width height content)
        | not $ validImage $ Image width height content = error "saveImage: Input image is not valid"
        | takeExtension path /= ".ppm" = error "saveImage: Input image doesn't have extension .ppm"
        | otherwise = writeInFile path (Image width height content)

    --Д) load image
    groupOf :: Int -> [a] -> [[a]]
    groupOf _ [] = []
    groupOf n xs
        | n > 0 = take n xs : groupOf n (drop n xs)

    isCorrectForWord8 :: Int -> Int -> Int -> Bool
    isCorrectForWord8 redInt greenInt blueInt
        = redInt >= 0 && redInt <= 255 && greenInt >= 0 && greenInt <= 255 && blueInt >= 0 && blueInt <= 255

    createContent :: [String] -> [Rgb]
    createContent [] = []
    createContent (element : pixels) = do
                                    let values = words element
                                    let redInt = read $ head values :: Int
                                    let greenInt = read $ last $ take 2 values :: Int
                                    let blueInt = read $ last values :: Int
                                    if isCorrectForWord8 redInt greenInt blueInt
                                        then do
                                        let red = fromIntegral redInt :: Word8
                                        let green = fromIntegral greenInt :: Word8
                                        let blue = fromIntegral blueInt :: Word8
                                        let pixel = Rgb red green blue
                                        pixel : createContent pixels
                                    else error "loadImage: Found incorrect format of pixel (color is out of bounds)"

    getImageFrom :: String -> IO Image
    getImageFrom path = do
                    file <- readFile path
                    let allLines = filter (/= "") (lines file)
                    let header = head allLines
                    if header == "P3"
                        then do
                            let sizes = words $ last $ take 2 allLines
                            let maximumValue = last $ take 3 allLines
                            if maximumValue == "255"
                                then do
                                    let pixels = drop 3 allLines
                                    let width = read $ head sizes :: Int
                                    let height = read $ last sizes :: Int
                                    if width > 0 && height > 0
                                        then do
                                            let content = createContent pixels
                                            return (Image width height (groupOf width content))
                                        else error "loadImage: Invalid size of image (width or height is negative)"
                                else error "loadImage: Expected 255 for maximum value of each pixel"
                        else error "loadImage: Expected P3 image"

    loadImage :: String -> IO Image
    loadImage path
        | takeExtension path /= ".ppm" = error "loadImage: Input image doesn't have extension .ppm"
        | otherwise = getImageFrom path