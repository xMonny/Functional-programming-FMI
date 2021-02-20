module Task2_Test where
    import Test.HUnit
    import Task2

    simpleMatrix :: [[Int]]
    simpleMatrix = [[1, 2, 3]
                  , [4, 5, 6]
                  , [7, 8, 9]
                  , [10, 11, 12]]

    wrongMatrix :: [[Int]]
    wrongMatrix = [[1, 2, 3]
                ,  [3, 4, 5, 6]]

    wrongRowsImage :: Image
    wrongRowsImage = Image 1 2 [[Rgb 100 140 210, Rgb 250 11 25]
                              , [Rgb 50 98 35, Rgb 129 219 33]]

    wrongColumnsImage :: Image
    wrongColumnsImage = Image 1 2 [[Rgb 100 140 210, Rgb 250 11 25, Rgb 50 98 35, Rgb 129 219 33]]

    invalidContentImage :: Image
    invalidContentImage = Image 2 2 [[Rgb 100 140 210, Rgb 250 11 25]
                                   , [Rgb 50 98 35]]

    validImage0x1 :: Image
    validImage0x1 = Image 0 1 [[]]

    validImage1x1 :: Image
    validImage1x1 = Image 1 1 [[Rgb 22 5 10]]

    grayscaleValidImage1x1 :: Image
    grayscaleValidImage1x1 = Image 1 1 [[Rgb 11 11 11]]

    validImage3x3 :: Image
    validImage3x3 = Image 3 3 [[Rgb 100 0 0, Rgb 15 11 0, Rgb 20 30 50]
                             , [Rgb 100 2 3, Rgb 10 15 10, Rgb 11 20 3]
                             , [Rgb 22 5 10, Rgb 16 17 18, Rgb 20 30 25]]

    grayscaleValidImage3x3 :: Image
    grayscaleValidImage3x3 = Image 3 3 [[Rgb 30 30 30, Rgb 11 11 11, Rgb 29 29 29]
                                      , [Rgb 32 32 32, Rgb 13 13 13, Rgb 15 15 15]
                                      , [Rgb 11 11 11, Rgb 17 17 17, Rgb 26 26 26]]

    validImage3x2 :: Image
    validImage3x2 = Image 3 2 [[Rgb 255 0 0, Rgb 128 255 128, Rgb 128 255 128]
                             , [Rgb 0 255 0, Rgb 255 255 255, Rgb 128 255 128]]

    grayscaleValidImage3x2 :: Image
    grayscaleValidImage3x2 = Image 3 2 [[Rgb 76 76 76, Rgb 203 203 203, Rgb 203 203 203]
                                      , [Rgb 150 150 150, Rgb 255 255 255, Rgb 203 203 203]]

    validImage4x5 :: Image
    validImage4x5 = Image 4 5 [[Rgb 199 17 14, Rgb 113 10 14, Rgb 20 15 10, Rgb 222 111 55]
                             , [Rgb 32 18 50, Rgb 99 15 14, Rgb 222 111 55, Rgb 153 19 140]
                             , [Rgb 187 215 13, Rgb 99 15 14, Rgb 99 15 14, Rgb 100 15 14]
                             , [Rgb 99 15 14, Rgb 99 15 14, Rgb 170 220 11, Rgb 15 16 19]
                             , [Rgb 100 14 10, Rgb 98 77 56, Rgb 18 250 44, Rgb 11 15 30]]

    grayscaleValidImage4x5 :: Image
    grayscaleValidImage4x5 = Image 4 5 [[Rgb 71 71 71, Rgb 41 41 41, Rgb 16 16 16, Rgb 138 138 138]
                                      , [Rgb 26 26 26, Rgb 40 40 40, Rgb 138 138 138, Rgb 73 73 73]
                                      , [Rgb 184 184 184, Rgb 40 40 40, Rgb 40 40 40, Rgb 40 40 40]
                                      , [Rgb 40 40 40, Rgb 40 40 40, Rgb 182 182 182, Rgb 16 16 16]
                                      , [Rgb 39 39 39, Rgb 81 81 81, Rgb 158 158 158, Rgb 15 15 15]]

    edgeDetectGrayscaledValidImage1x1 :: Image
    edgeDetectGrayscaledValidImage1x1 = Image 1 1 [[Rgb 0 0 0]]

    edgeDetectGrayscaledValidImage3x2 :: Image
    edgeDetectGrayscaledValidImage3x2 = Image 3 2 [[Rgb 0 0 0, Rgb 255 255 255, Rgb 0 0 0]
                                                 , [Rgb 0 0 0, Rgb 255 255 255, Rgb 0 0 0]]

    edgeDetectGrayscaledValidImage4x5 :: Image
    edgeDetectGrayscaledValidImage4x5 = Image 4 5 [[Rgb 0 0 0, Rgb 114 114 114, Rgb 255 255 255, Rgb 0 0 0]
                                                 , [Rgb 224 224 224, Rgb 137 137 137, Rgb 171 171 171, Rgb 148 148 148]
                                                 , [Rgb 28 28 28, Rgb 67 67 67, Rgb 32 32 32, Rgb 26 26 26]
                                                 , [Rgb 208 208 208, Rgb 255 255 255, Rgb 255 255 255, Rgb 186 186 186]
                                                 , [Rgb 0 0 0, Rgb 255 255 255, Rgb 180 180 180, Rgb 0 0 0]]

    floodFillValidImage1x1 :: Image
    floodFillValidImage1x1 = Image 1 1 [[Rgb 1 1 1]]

    floodFillValidImage3x2 :: Image
    floodFillValidImage3x2 = Image 3 2 [[Rgb 255 0 0, Rgb 1 1 1, Rgb 1 1 1]
                                      , [Rgb 0 255 0, Rgb 255 255 255, Rgb 1 1 1]]

    floodFillValidImage4x5 :: Image
    floodFillValidImage4x5 = Image 4 5 [[Rgb 199 17 14, Rgb 113 10 14, Rgb 20 15 10, Rgb 222 111 55]
                                      , [Rgb 32 18 50, Rgb 32 18 50, Rgb 222 111 55, Rgb 153 19 140]
                                      , [Rgb 187 215 13, Rgb 32 18 50, Rgb 32 18 50, Rgb 100 15 14]
                                      , [Rgb 32 18 50, Rgb 32 18 50, Rgb 170 220 11, Rgb 15 16 19]
                                      , [Rgb 100 14 10, Rgb 98 77 56, Rgb 18 250 44, Rgb 11 15 30]]

    --Validation test
    testInvalidColumnsMatrix :: Test
    testInvalidColumnsMatrix = "Matrix ((1, 2, 3), (3, 4, 5, 6)) should be invalid: Different number of columns" ~:
                                                        False ~=? validRowLength wrongMatrix 3

    testNumberRowsSimpleMatrix :: Test
    testNumberRowsSimpleMatrix = "Matrix ((1, 2, 3), (4, 5, 6), (7, 8, 9), (10, 11, 12)) should have 4 rows" ~: 
                                    4 ~=? numberRows simpleMatrix
    testNumberColumnsSimpleMatrix :: Test
    testNumberColumnsSimpleMatrix = "Matrix ((1, 2, 3), (4, 5, 6), (7, 8, 9)) should have 3 columns" ~: 
                                        3 ~=? numberColumns simpleMatrix

    testValidImageWrongRows :: Test
    testValidImageWrongRows = "Image 1 2 [[Rgb 100 140 210, Rgb 250 11 25], [Rgb 50 98 35, Rgb 129 219 33]] has wrong number of rows" ~: 
                                False ~=? validImage wrongRowsImage

    testValidImageWrongColumns :: Test
    testValidImageWrongColumns = "Image 1 2 [[Rgb 100 140 210, Rgb 250 11 25, Rgb 50 98 35, Rgb 129 219 33]] has wrong number of columns" ~: 
                                False ~=? validImage wrongColumnsImage

    testValidImageInvalidContent :: Test
    testValidImageInvalidContent = "Image 2 2 [[Rgb 100 140 210, Rgb 250 11 25], [Rgb 50 98 35]] has different columns in rows" ~:
                                False ~=? validImage invalidContentImage

    --Grayscale test
    testGrayscaleBaseImage :: Test
    testGrayscaleBaseImage = "Base image should be base image after grayscale" ~:
                                baseImage ~=? grayscale baseImage

    testGrayscaleValidImage0x1 :: Test
    testGrayscaleValidImage0x1 = "Image 0x1 should be the same after grayscale" ~:
                                    validImage0x1 ~=? grayscale validImage0x1

    testGrayscaleValidImage1x1 :: Test
    testGrayscaleValidImage1x1 = "Image 1x1 should be correctly grayscaled" ~:
                                    grayscaleValidImage1x1 ~=? grayscale validImage1x1

    testGrayscaleValidImage3x3 :: Test
    testGrayscaleValidImage3x3 = "Image 3x3 should be correctly grayscaled" ~:
                                    grayscaleValidImage3x3 ~=? grayscale validImage3x3

    testGrayscaleValidImage3x2 :: Test
    testGrayscaleValidImage3x2 = "Image 3x2 should be correctly grayscaled" ~:
                                    grayscaleValidImage3x2 ~=? grayscale validImage3x2

    testGrayscaleValidImage4x5 :: Test
    testGrayscaleValidImage4x5 = "Image 4x5 should be correctly grayscaled" ~:
                                    grayscaleValidImage4x5 ~=? grayscale validImage4x5

    --Test if image is grayscaled
    testIsGrayscaledBaseImage :: Test
    testIsGrayscaledBaseImage = "Base image should be grayscaled" ~:
                                    True ~=? isGrayscaleImage baseImage

    testIsGrayscaledValidImage0x1 :: Test
    testIsGrayscaledValidImage0x1 = "Image 0x1 should be grayscaled" ~:
                                    True ~=? isGrayscaleImage validImage0x1

    testIsGrayscaledValidImage3x3 :: Test
    testIsGrayscaledValidImage3x3 = "Image 3x3 should not be grayscaled" ~:
                                        False ~=? isGrayscaleImage validImage3x3

    testIsGrayscaledValidImage4x5 :: Test
    testIsGrayscaledValidImage4x5 = "Image 4x5 should not be grayscaled" ~:
                                        False ~=? isGrayscaleImage validImage4x5

    testIsGrayscaledValidGrayscaleImage4x5 :: Test
    testIsGrayscaledValidGrayscaleImage4x5 = "Grayscaled image 4x5 should be grayscaled" ~: 
                                                True ~=? isGrayscaleImage grayscaleValidImage4x5

    testIsGrayscaledValidGrayscaleImage3x2 :: Test
    testIsGrayscaledValidGrayscaleImage3x2 = "Grayscaled image 3x2 should be grayscaled" ~:
                                                True ~=? isGrayscaleImage grayscaleValidImage3x2

    --Edge detect test
    testEdgeDetectBaseImage :: Test
    testEdgeDetectBaseImage = "Base image should be correctly edge-detected" ~:
                                baseImage ~?= edgeDetect baseImage

    testEdgeDetectGrayscaledValidImage1x1 :: Test
    testEdgeDetectGrayscaledValidImage1x1 = "Grayscaled image 1x1 should be correctly edge-detected" ~:
                                                edgeDetectGrayscaledValidImage1x1 ~=? edgeDetect grayscaleValidImage1x1

    testEdgeDetectValidImage0x1 :: Test
    testEdgeDetectValidImage0x1 = "Image 0x1 should be the same after edge detect" ~:
                                                validImage0x1 ~=? edgeDetect validImage0x1

    testEdgeDetectGrayscaledValidImage3x2 :: Test
    testEdgeDetectGrayscaledValidImage3x2 = "Grayscaled image 3x3 should be correctly edge-detected" ~:
                                                edgeDetectGrayscaledValidImage3x2 ~=? edgeDetect grayscaleValidImage3x2

    testEdgeDetectGrayscaledValidImage4x5 :: Test
    testEdgeDetectGrayscaledValidImage4x5 = "Grayscaled image 4x5 should be correctly edge-detected" ~:
                                                edgeDetectGrayscaledValidImage4x5 ~=? edgeDetect grayscaleValidImage4x5

    --Flood fill test
    testFloodFillBaseImage :: Test
    testFloodFillBaseImage = "Base image should be the same after flood fill" ~: baseImage ~=? floodFill (Rgb 1 1 1) 1 1 baseImage

    testFloodFillValidImage0x1 :: Test
    testFloodFillValidImage0x1 = "Image 0x1 should be the samoe after flood fill" ~: 
                                    validImage0x1 ~=? floodFill (Rgb 1 1 1) 1 1 validImage0x1

    testFloodFillValidImage1x1 :: Test
    testFloodFillValidImage1x1 = "Image 1x1 should be correctly flood-filled with color (Rgb 1 1 1), row = 1, col = 1" ~:
                                    floodFillValidImage1x1 ~=? floodFill (Rgb 1 1 1) 1 1 validImage1x1

    testFloodFillValidImage3x2 :: Test
    testFloodFillValidImage3x2 = "Image 3x2 should be correctly flood-filled with color (Rgb 1 1 1), row = 2, col = 3" ~:
                                    floodFillValidImage3x2 ~=? floodFill (Rgb 1 1 1) 2 3 validImage3x2
    
    testFloodFillValidImage4x5 :: Test
    testFloodFillValidImage4x5 = "Image 4x5 should be correctly flood-filled with color (Rgb 32 18 50), row = 3, col = 2" ~:
                                    floodFillValidImage4x5 ~=? floodFill (Rgb 32 18 50) 3 2 validImage4x5

    testFloodFillValidImage4x5OutOfBounds :: Test
    testFloodFillValidImage4x5OutOfBounds = "Image 4x5 should be the same after flood fill when out of bounds" ~:
                                                validImage4x5 ~=? floodFill (Rgb 1 1 1) 10 2 validImage4x5

    --Test calculation of grayscale value and creating grayscale content with grayscale value
    rgb5_5_5 :: Rgb
    rgb5_5_5 = Rgb 5 5 5

    rgb132_132_132 :: Rgb
    rgb132_132_132 = Rgb 132 132 132

    rgb100_40_90 :: Rgb
    rgb100_40_90 = Rgb 100 40 90

    rgb255_132_190 :: Rgb
    rgb255_132_190 = Rgb 255 132 190

    testGrayscaleValueRgb5_5_5 :: Test
    testGrayscaleValueRgb5_5_5 = "(Rgb 5 5 5) should have grayscale value 5" ~:
                                    5 ~=? grayscaleValue rgb5_5_5

    testGrayscaleValueRgb100_40_90 :: Test
    testGrayscaleValueRgb100_40_90 = "(Rgb 100 40 90) should have grayscale value 64" ~:
                                    64 ~=? grayscaleValue rgb100_40_90

    testGrayscaleValueRgb255_132_190 :: Test
    testGrayscaleValueRgb255_132_190 = "(Rgb 255 132 190) should have grayscale value 175" ~:
                                    175 ~=? grayscaleValue rgb255_132_190

    testCreateGrayscaleWithValue5 :: Test
    testCreateGrayscaleWithValue5 = "Create grayscale with value 5 should return (Rgb 5 5 5)" ~:
                                        rgb5_5_5 ~=? createGrayscale 5

    testCreateGrayscaleWithValue132 :: Test
    testCreateGrayscaleWithValue132 = "Create grayscale with value 132 should return (Rgb 132 132 132)" ~:
                                        rgb132_132_132 ~=? createGrayscale 132

    --Test answer from scalar multiplication of matrixes 3x3
    matrixA_3x3 :: [[Float]]
    matrixA_3x3 = [[1, 2, 3]
                 , [4, 5, 6]
                 , [-10, -5, 9]]

    matrixB_3x3 :: [[Float]]
    matrixB_3x3 = [[-1, 2, -3]
                 , [4, 5, 6]
                 , [-1, 2, 0]]

    answerMultiplyScalarMatrix_3x3 :: Float
    answerMultiplyScalarMatrix_3x3 = 71

    testMultiplyScalarMatrix_3x3 :: Test
    testMultiplyScalarMatrix_3x3 = "Scalar matrix multiplication should be completed by indexes" ~:
                                    answerMultiplyScalarMatrix_3x3 ~=? multiplyScalarMatrix matrixA_3x3 matrixB_3x3

    --Test changePixel on position
    content_3x3 :: [[Rgb]]
    content_3x3 = [[Rgb 1 1 1, Rgb 2 2 2, Rgb 3 3 3]
                 , [Rgb 4 5 6, Rgb 7 8 9, Rgb 10 11 12]
                 , [Rgb 13 13 13, Rgb 14 14 14, Rgb 15 15 15]]

    changedContentWithRgb5_5_5 :: [[Rgb]]
    changedContentWithRgb5_5_5 = [[Rgb 1 1 1, Rgb 2 2 2, Rgb 3 3 3]
                                 , [Rgb 4 5 6, Rgb 5 5 5, Rgb 10 11 12]
                                 , [Rgb 13 13 13, Rgb 14 14 14, Rgb 15 15 15]]

    testChangePixelWithRgb5_5_5 :: Test
    testChangePixelWithRgb5_5_5 = "Pixel on row 2, column 2 should be replaced with (Rgb 5 5 5)" ~:
                                    changedContentWithRgb5_5_5 ~=? changePixel 2 2 rgb5_5_5 content_3x3

    --Test getPixel from content
    testGetPixelFromContent_3x3_0_0 :: Test
    testGetPixelFromContent_3x3_0_0 = "Pixel on row 0, column 0 from content 3x3 should be (Rgb 0 0 0)" ~:
                                    Rgb 0 0 0 ~=? getPixel 0 0 content_3x3

    testGetPixelFromContent_3x3_Invalid_Bounds :: Test
    testGetPixelFromContent_3x3_Invalid_Bounds = "Pixel on row -1, column -2 from content 3x3 should be (Rgb 0 0 0)" ~:
                                    Rgb 0 0 0 ~=? getPixel (-1) (-2) content_3x3

    testGetPixelFromContent_3x3_Out_Of_Bounds :: Test
    testGetPixelFromContent_3x3_Out_Of_Bounds = "Pixel on row 10, column 2 from content 3x3 should be (Rgb 0 0 0)" ~:
                                    Rgb 0 0 0 ~=? getPixel 10 2 content_3x3

    testGetPixelFromContent_3x3_3_1 :: Test
    testGetPixelFromContent_3x3_3_1 = "Pixel on row 3, column 1 from content 3x3 should be (Rgb 13 13 13)" ~:
                                    Rgb 13 13 13 ~=? getPixel 3 1 content_3x3

    testGetPixelFromContent_3x3_2_3 :: Test
    testGetPixelFromContent_3x3_2_3 = "Pixel on row 2, column 3 from content 3x3 should be (Rgb 10 11 12)" ~:
                                    Rgb 10 11 12 ~=? getPixel 2 3 content_3x3