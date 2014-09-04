'Copyright (c) 1996-2014 Randall Britten
'Licensed under "The MIT License (MIT)"

'Permission is hereby granted, free of charge, to any person obtaining a copy
'of this software and associated documentation files (the "Software"), to deal
'in the Software without restriction, including without limitation the rights
'to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
'copies of the Software, and to permit persons to whom the Software is
'furnished to do so, subject to the following conditions:

'The above copyright notice and this permission notice shall be included in all
'copies or substantial portions of the Software.

'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
'IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
'FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
'AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
'LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
'OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
'SOFTWARE.

DECLARE SUB FileSolution ()
DECLARE SUB RemovePiece (Image AS INTEGER, LowestSquare AS ANY)
DECLARE SUB Setup4 ()
DECLARE SUB ShowSolution ()
DECLARE SUB PlacePiece (Image AS INTEGER, LowestSquare AS ANY)
DECLARE SUB NextSquare (LowestSquare AS ANY)
DECLARE FUNCTION TryPiece (Image AS INTEGER, LowestSquare AS ANY)
DECLARE SUB PlayPentomino (LowestSquare AS ANY)
DECLARE SUB SetupBoard ()
DECLARE SUB DrawPiece2 (PieceNumber AS INTEGER)
DECLARE SUB StorePieceImage (ImageIndex AS INTEGER)
DECLARE SUB Setup3 ()
DECLARE SUB Setup2 ()
DECLARE SUB DrawPiece1 (PieceNumber AS INTEGER)
DECLARE SUB Setup1 ()
DECLARE SUB RotateClockWise (ImageIndex AS INTEGER)
DECLARE SUB FlipPiece (ImageIndex AS INTEGER)

CONST PieceCount = 13

DIM SHARED UsedPiece(1 TO PieceCount) AS INTEGER
DIM SHARED PieceMarker(1 TO PieceCount) AS STRING

CONST RenderOffsetX = 20
CONST RenderOffsetY = 3


CONST BoardWidth = 8+1
CONST BoardHeight = 8+1
DIM SHARED Board(0 TO BoardWidth + 1, 0 TO BoardHeight + 1) AS INTEGER

TYPE BlockCoord
  x AS INTEGER
  y AS INTEGER
END TYPE

CONST PieceImageCount = 57

TYPE PieceSpecific
  PieceNumber AS INTEGER
  Rotate      AS INTEGER
  Flip        AS INTEGER
END TYPE

DIM SHARED PieceStructure(1 TO PieceCount, 1 TO 4)  AS BlockCoord   'Twelve Pentominos. Note that relative coord (0,0) is occupied by default.
DIM SHARED PieceSpecifics(1 TO PieceImageCount)  AS PieceSpecific   'How they can be flipped and rotated
DIM SHARED PieceImage(1 TO PieceImageCount, 0 TO 4)  AS BlockCoord  'The resulting images 0.x is for the piece number
DIM SHARED NumberUsed AS INTEGER                                    'Number of pieces places so far.

TYPE SolutionStep
  Image AS INTEGER
  x     AS INTEGER
  y     AS INTEGER
END TYPE


DIM SHARED Solution(1 TO PieceCount)  AS SolutionStep

DIM SHARED GlobalLowestSquare AS BlockCoord
DIM LowestSquare AS BlockCoord
DIM SHARED NumberSolutions AS INTEGER

CALL Setup1  'Assigns the piece structures, and Piece markers
CALL Setup2  'Assigns the piece image specifics.
CALL Setup3  'Places images in array.
CALL Setup4  'Set PieceUsed to 0
CLS
CALL SetupBoard


NumberUsed = 0
NumberSolutions = 0
LowestSquare.x = 1
LowestSquare.y = 1

'OPEN "Sol_Spec.DAT" FOR OUTPUT AS #1
OPEN "Sol_Look.DAT" FOR OUTPUT AS #2

CALL PlayPentomino(LowestSquare)

CLOSE

SUB DrawAllImages
  DIM i AS INTEGER

  CLS
  FOR i = 1 TO PieceImageCount
    COLOR 15, 15
    CALL DrawPiece2(i)
    'SLEEP
    NumberUsed = 0
    COLOR 15, 0
    CLS
  NEXT
  COLOR 15, 0

END SUB

SUB DrawPiece1 (PieceNumber AS INTEGER)
  xBase = PieceNumber * 6
  yBase = 7
  LOCATE yBase, xBase
  PRINT " "
  FOR i = 1 TO 4
    x = xBase + PieceStructure(PieceNumber, i).x
    y = yBase + PieceStructure(PieceNumber, i).y
    LOCATE y, x
    PRINT " "
  NEXT
END SUB

SUB DrawPiece2 (PieceNumber AS INTEGER)
  xBase = 20
  yBase = 10
  LOCATE yBase, xBase
  PRINT "x"
  FOR i = 1 TO 4
    x = xBase + PieceImage(PieceNumber, i).x
    y = yBase + PieceImage(PieceNumber, i).y
    LOCATE y, x
    PRINT " "
  NEXT

END SUB

SUB FileSolution
  DIM CaptureScreen AS STRING
'  FOR i = 1 TO PieceCount
'    PRINT #1, NumberSolutions, i, Solution(i).Image, Solution(i).x, Solution(i).y
'  NEXT


  FOR j = 0 TO BoardHeight
    CaptureScreen = ""
    FOR i = 0 TO BoardWidth
      CaptureScreen = CaptureScreen + CHR$(SCREEN(RenderOffsetY + j, RenderOffsetX + i))
    NEXT
    PRINT #2, CaptureScreen
  NEXT
  PRINT #2, "   "
      
END SUB

SUB FlipPiece (ImageIndex AS INTEGER)

  FOR i = 1 TO 4
    PieceImage(ImageIndex, i).x = -PieceImage(ImageIndex, i).x
  NEXT

END SUB

SUB NextSquare (LowestSquare AS BlockCoord)

  GlobalLowestSquare.x = LowestSquare.x
  GlobalLowestSquare.y = LowestSquare.y
 
  WHILE Board(GlobalLowestSquare.x, GlobalLowestSquare.y) = 1
    GlobalLowestSquare.x = GlobalLowestSquare.x + 1
    IF GlobalLowestSquare.x > BoardWidth THEN
      GlobalLowestSquare.x = 0
      GlobalLowestSquare.y = GlobalLowestSquare.y + 1
    END IF
    IF GlobalLowestSquare.y > BoardHeight THEN
      PRINT "error in lowest square"
      'SLEEP
    END IF
  WEND
END SUB

SUB PlacePiece (Image AS INTEGER, LowestSquare AS BlockCoord)
  NumberUsed = NumberUsed + 1
  LOCATE 1, 1
  PRINT "    "
  LOCATE 1, 1
  PRINT NumberUsed

 
  'Remeber this step of the solution
  Solution(NumberUsed).Image = Image
  Solution(NumberUsed).x = LowestSquare.x
  Solution(NumberUsed).y = LowestSquare.y

  'Mark this piece as used.
  UsedPiece(PieceImage(Image, 0).x) = 1

  'Fill the squares on the board
  Board(LowestSquare.x, LowestSquare.y) = 1

  LOCATE LowestSquare.y + RenderOffsetY, LowestSquare.x + RenderOffsetX
  PRINT PieceMarker(PieceImage(Image, 0).x)
 
 
  FOR i = 1 TO 4
    x = PieceImage(Image, i).x + LowestSquare.x
    y = PieceImage(Image, i).y + LowestSquare.y
    Board(x, y) = 1

    LOCATE y + RenderOffsetY, x + RenderOffsetX
    PRINT PieceMarker(PieceImage(Image, 0).x)
   
  NEXT
  i = 0
END SUB

SUB PlayPentomino (LowestSquare AS BlockCoord)
 DIM NextLowestSquare AS BlockCoord
 DIM Image AS INTEGER
 FOR Image = 1 TO PieceImageCount
   IF UsedPiece(PieceImage(Image, 0).x) = 0 THEN  'If Piece not used yet.
      UseThis = TryPiece(Image, LowestSquare)     'Try image of piece.
      IF UseThis = 1 THEN                         'If Piece fits.
         CALL PlacePiece(Image, LowestSquare)     'Place the image.
         IF NumberUsed = PieceCount THEN
           LOCATE 1, 1
           PRINT "Found a solution"
           NumberSolutions = NumberSolutions + 1
           LOCATE 2, 1
           PRINT "        "
           LOCATE 2, 1
           PRINT NumberSolutions
           CALL FileSolution
           'CALL ShowSolution
           'BEEP
           'SLEEP 1
           LOCATE 1, 1
           PRINT "                "

         ELSE
           CALL NextSquare(LowestSquare)               'Find next vacant square.
           NextLowestSquare.x = GlobalLowestSquare.x   'Can't pass by reference, so have to return values by Global variable.
           NextLowestSquare.y = GlobalLowestSquare.y   'What a pain.

           CALL PlayPentomino(NextLowestSquare)        'Continue filling rest of board
           
         END IF
         CALL RemovePiece(Image, LowestSquare)            'BackTrack (even if recur was succesful, there may be more.
      END IF
   END IF
 NEXT

END SUB

SUB RemovePiece (Image AS INTEGER, LowestSquare AS BlockCoord)
'Basically undo everything that was done in place piece.

  NumberUsed = NumberUsed - 1

  'Mark this piece as un-used.
  UsedPiece(PieceImage(Image, 0).x) = 0

  'Empty the squares on the board
  Board(LowestSquare.x, LowestSquare.y) = 0
 
  LOCATE LowestSquare.y + RenderOffsetY, LowestSquare.x + RenderOffsetX
  PRINT " "


  FOR i = 1 TO 4
    x = PieceImage(Image, i).x + LowestSquare.x
    y = PieceImage(Image, i).y + LowestSquare.y
    Board(x, y) = 0

    LOCATE y + RenderOffsetY, x + RenderOffsetX
    PRINT " "

  NEXT

  
END SUB

SUB RotateClockWise (ImageIndex AS INTEGER)
  DIM WorkingImage(1 TO 4)  AS BlockCoord
  FOR i = 1 TO 4
    WorkingImage(i).x = PieceImage(ImageIndex, i).y
    WorkingImage(i).y = -PieceImage(ImageIndex, i).x
  NEXT

  FOR i = 1 TO 4
    PieceImage(ImageIndex, i).x = WorkingImage(i).x
    PieceImage(ImageIndex, i).y = WorkingImage(i).y
  NEXT

END SUB

SUB Setup1

'Straight
PieceStructure(1, 1).x = 0
PieceStructure(1, 1).y = 1

PieceStructure(1, 2).x = 0
PieceStructure(1, 2).y = 2

PieceStructure(1, 3).x = 0
PieceStructure(1, 3).y = 3

PieceStructure(1, 4).x = 0
PieceStructure(1, 4).y = 4

'Hook
PieceStructure(2, 1).x = 0
PieceStructure(2, 1).y = 1

PieceStructure(2, 2).x = 0
PieceStructure(2, 2).y = 2

PieceStructure(2, 3).x = 0
PieceStructure(2, 3).y = 3

PieceStructure(2, 4).x = 1
PieceStructure(2, 4).y = 3

'Kink
PieceStructure(3, 1).x = 0
PieceStructure(3, 1).y = 1

PieceStructure(3, 2).x = 0
PieceStructure(3, 2).y = 2

PieceStructure(3, 3).x = 1
PieceStructure(3, 3).y = 2

PieceStructure(3, 4).x = 1
PieceStructure(3, 4).y = 3

'Crutch
PieceStructure(4, 1).x = 0
PieceStructure(4, 1).y = 1

PieceStructure(4, 2).x = 0
PieceStructure(4, 2).y = 2

PieceStructure(4, 3).x = 0
PieceStructure(4, 3).y = 3

PieceStructure(4, 4).x = 1
PieceStructure(4, 4).y = 2

'Tee
PieceStructure(5, 1).x = 0
PieceStructure(5, 1).y = 1

PieceStructure(5, 2).x = 0
PieceStructure(5, 2).y = 2

PieceStructure(5, 3).x = 1
PieceStructure(5, 3).y = 2

PieceStructure(5, 4).x = -1
PieceStructure(5, 4).y = 2

'Cee
PieceStructure(6, 1).x = 0
PieceStructure(6, 1).y = 1

PieceStructure(6, 2).x = 0
PieceStructure(6, 2).y = 2

PieceStructure(6, 3).x = 1
PieceStructure(6, 3).y = 2

PieceStructure(6, 4).x = 1
PieceStructure(6, 4).y = 0

'Plus
PieceStructure(7, 1).x = 0
PieceStructure(7, 1).y = 1

PieceStructure(7, 2).x = 0
PieceStructure(7, 2).y = 2

PieceStructure(7, 3).x = 1
PieceStructure(7, 3).y = 1

PieceStructure(7, 4).x = -1
PieceStructure(7, 4).y = 1

'Y
PieceStructure(8, 1).x = 0
PieceStructure(8, 1).y = 1

PieceStructure(8, 2).x = 0
PieceStructure(8, 2).y = 2

PieceStructure(8, 3).x = -1
PieceStructure(8, 3).y = 1

PieceStructure(8, 4).x = 1
PieceStructure(8, 4).y = 2

'Two
PieceStructure(9, 1).x = 0
PieceStructure(9, 1).y = 1

PieceStructure(9, 2).x = 0
PieceStructure(9, 2).y = 2

PieceStructure(9, 3).x = -1
PieceStructure(9, 3).y = 0

PieceStructure(9, 4).x = 1
PieceStructure(9, 4).y = 2

'Lambda
PieceStructure(10, 1).x = 0
PieceStructure(10, 1).y = 1

PieceStructure(10, 2).x = 0
PieceStructure(10, 2).y = 2

PieceStructure(10, 3).x = 1
PieceStructure(10, 3).y = 1

PieceStructure(10, 4).x = 1
PieceStructure(10, 4).y = 2

'W
PieceStructure(11, 1).x = 0
PieceStructure(11, 1).y = 1

PieceStructure(11, 2).x = 1
PieceStructure(11, 2).y = 1

PieceStructure(11, 3).x = 1
PieceStructure(11, 3).y = 2

PieceStructure(11, 4).x = 2
PieceStructure(11, 4).y = 2

'V
PieceStructure(12, 1).x = 0
PieceStructure(12, 1).y = 1

PieceStructure(12, 2).x = 0
PieceStructure(12, 2).y = 2

PieceStructure(12, 3).x = 1
PieceStructure(12, 3).y = 0

PieceStructure(12, 4).x = 2
PieceStructure(12, 4).y = 0

'Square (2x2), 
'Note: Hack: duplication hack since code assumes pentomino, not tetromino.
'            i.e. by default there is already a (0,0), but one is explicitly added here again.
PieceStructure(13, 1).x = 0
PieceStructure(13, 1).y = 0

PieceStructure(13, 2).x = 0
PieceStructure(13, 2).y = 1

PieceStructure(13, 3).x = 1
PieceStructure(13, 3).y = 0

PieceStructure(13, 4).x = 1
PieceStructure(13, 4).y = 1



PieceMarker(1) = "1"
PieceMarker(2) = "2"
PieceMarker(3) = "3"
PieceMarker(4) = "4"
PieceMarker(5) = "5"
PieceMarker(6) = "6"
PieceMarker(7) = "7"
PieceMarker(8) = "8"
PieceMarker(9) = "9"
PieceMarker(10) = "a"
PieceMarker(11) = "b"
PieceMarker(12) = "c"
PieceMarker(13) = "d"

END SUB

SUB Setup2

  'Straight
  ImageNumber = 1
  PieceSpecifics(ImageNumber).PieceNumber = 1
  PieceSpecifics(ImageNumber).Rotate = 0
  PieceSpecifics(ImageNumber).Flip = 0
 
  ImageNumber = ImageNumber + 1
  PieceSpecifics(ImageNumber).PieceNumber = 1
  PieceSpecifics(ImageNumber).Rotate = 1
  PieceSpecifics(ImageNumber).Flip = 0
 
  'Hook
  FOR i = 0 TO 3
    ImageNumber = ImageNumber + 1
    PieceSpecifics(ImageNumber).PieceNumber = 2
    PieceSpecifics(ImageNumber).Rotate = i
    PieceSpecifics(ImageNumber).Flip = 0
  NEXT

  FOR i = 0 TO 3
    ImageNumber = ImageNumber + 1
    PieceSpecifics(ImageNumber).PieceNumber = 2
    PieceSpecifics(ImageNumber).Rotate = i
    PieceSpecifics(ImageNumber).Flip = 1
  NEXT


  'Kink
  FOR i = 0 TO 3
    ImageNumber = ImageNumber + 1
    PieceSpecifics(ImageNumber).PieceNumber = 3
    PieceSpecifics(ImageNumber).Rotate = i
    PieceSpecifics(ImageNumber).Flip = 0
  NEXT

  FOR i = 0 TO 3
    ImageNumber = ImageNumber + 1
    PieceSpecifics(ImageNumber).PieceNumber = 3
    PieceSpecifics(ImageNumber).Rotate = i
    PieceSpecifics(ImageNumber).Flip = 1
  NEXT

  'Crutch
  FOR i = 0 TO 3
    ImageNumber = ImageNumber + 1
    PieceSpecifics(ImageNumber).PieceNumber = 4
    PieceSpecifics(ImageNumber).Rotate = i
    PieceSpecifics(ImageNumber).Flip = 0
  NEXT

  FOR i = 0 TO 3
    ImageNumber = ImageNumber + 1
    PieceSpecifics(ImageNumber).PieceNumber = 4
    PieceSpecifics(ImageNumber).Rotate = i
    PieceSpecifics(ImageNumber).Flip = 1
  NEXT

  'Tee
  FOR i = 0 TO 3
    ImageNumber = ImageNumber + 1
    PieceSpecifics(ImageNumber).PieceNumber = 5
    PieceSpecifics(ImageNumber).Rotate = i
    PieceSpecifics(ImageNumber).Flip = 0
  NEXT

  'Cee
  FOR i = 0 TO 3
    ImageNumber = ImageNumber + 1
    PieceSpecifics(ImageNumber).PieceNumber = 6
    PieceSpecifics(ImageNumber).Rotate = i
    PieceSpecifics(ImageNumber).Flip = 0
  NEXT

  'Plus
  ImageNumber = ImageNumber + 1
  PieceSpecifics(ImageNumber).PieceNumber = 7
  PieceSpecifics(ImageNumber).Rotate = 0
  PieceSpecifics(ImageNumber).Flip = 0


  'Y
  FOR i = 0 TO 3
    ImageNumber = ImageNumber + 1
    PieceSpecifics(ImageNumber).PieceNumber = 8
    PieceSpecifics(ImageNumber).Rotate = i
    PieceSpecifics(ImageNumber).Flip = 0
  NEXT

  FOR i = 0 TO 3
    ImageNumber = ImageNumber + 1
    PieceSpecifics(ImageNumber).PieceNumber = 8
    PieceSpecifics(ImageNumber).Rotate = i
    PieceSpecifics(ImageNumber).Flip = 1
  NEXT

  'Two
  FOR i = 0 TO 1
    ImageNumber = ImageNumber + 1
    PieceSpecifics(ImageNumber).PieceNumber = 9
    PieceSpecifics(ImageNumber).Rotate = i
    PieceSpecifics(ImageNumber).Flip = 0
  NEXT

  FOR i = 0 TO 1
    ImageNumber = ImageNumber + 1
    PieceSpecifics(ImageNumber).PieceNumber = 9
    PieceSpecifics(ImageNumber).Rotate = i
    PieceSpecifics(ImageNumber).Flip = 1
  NEXT

  'Lambda
  'Clever hack: By not rotating or flipping this piece, we avoid symmetric solutions.
  'In this case board has all 8 symmetries.  In the case where board has less symmetry, add back whichever are the required transformations of this piece.
  ImageNumber = ImageNumber + 1
  PieceSpecifics(ImageNumber).PieceNumber = 10
  PieceSpecifics(ImageNumber).Rotate = 0
  PieceSpecifics(ImageNumber).Flip = 0

  'W
  FOR i = 0 TO 3
    ImageNumber = ImageNumber + 1
    PieceSpecifics(ImageNumber).PieceNumber = 11
    PieceSpecifics(ImageNumber).Rotate = i
    PieceSpecifics(ImageNumber).Flip = 0
  NEXT

  'V
  FOR i = 0 TO 3
    ImageNumber = ImageNumber + 1
    PieceSpecifics(ImageNumber).PieceNumber = 12
    PieceSpecifics(ImageNumber).Rotate = i
    PieceSpecifics(ImageNumber).Flip = 0
  NEXT

  'Square 2x2
  ImageNumber = ImageNumber + 1
  PieceSpecifics(ImageNumber).PieceNumber = 13
  PieceSpecifics(ImageNumber).Rotate = 0
  PieceSpecifics(ImageNumber).Flip = 0


PRINT ImageNumber

END SUB

SUB Setup3
  FOR i = 1 TO PieceImageCount
    StorePieceImage (i)
  NEXT
END SUB

SUB Setup4
  FOR i = 1 TO PieceCount
    UsedPiece(i) = 0
  NEXT
END SUB

SUB SetupBoard
  FOR i = 0 TO BoardWidth
    FOR j = 0 TO BoardHeight
      Board(i, j) = 0
    NEXT
  NEXT

  FOR i = 0 TO BoardWidth
    Board(i, 0) = 1
    LOCATE RenderOffsetY, i + RenderOffsetX
    PRINT "+"

    Board(i, BoardHeight) = 1
    LOCATE RenderOffsetY + BoardHeight, i + RenderOffsetX
    PRINT "+"

    'CONST BarrierY = 6
    'Barrier through middle
    'Board(i, BarrierY) = 1
    'LOCATE RenderOffsetY + BarrierY, i + RenderOffsetX
    'PRINT "+"

  NEXT

  FOR j = 0 TO BoardHeight
    Board(0, j) = 1
    LOCATE j + RenderOffsetY, RenderOffsetX
    PRINT "+"

    Board(BoardWidth, j) = 1
    LOCATE RenderOffsetY + j, BoardWidth + RenderOffsetX
    PRINT "+"

  NEXT

END SUB

SUB ShowAllImages
'Draw Pieces
CLS
COLOR 15, 15
DIM i AS INTEGER
FOR i = 1 TO PieceImageCount
  CALL DrawPiece2(i)
  'SLEEP
  COLOR 15, 0
  CLS
  COLOR 15, 15
NEXT
COLOR 15, 0
'SLEEP
END SUB

SUB ShowSolution
  FOR i = 1 TO PieceCount
    PRINT i, Solution(i).Image, Solution(i).x, Solution(i).y
  NEXT

END SUB


SUB StorePieceImage (ImageIndex AS INTEGER)
  DIM WorkingImage(0 TO 4)  AS BlockCoord


    PieceImage(ImageIndex, 0).x = PieceSpecifics(ImageIndex).PieceNumber

    'Store the piece image
    FOR i = 1 TO 4
      PieceImage(ImageIndex, i).x = PieceStructure(PieceSpecifics(ImageIndex).PieceNumber, i).x
      PieceImage(ImageIndex, i).y = PieceStructure(PieceSpecifics(ImageIndex).PieceNumber, i).y
    NEXT

    'Rotate Piece if necessary
    FOR i = 1 TO PieceSpecifics(ImageIndex).Rotate
      CALL RotateClockWise(ImageIndex)
    NEXT

    'Flip Piece if necessary
    IF PieceSpecifics(ImageIndex).Flip = 1 THEN
      CALL FlipPiece(ImageIndex)
    END IF

    'Make image be on 'vacant' side of 0,0
    WorkingImage(0).x = 0
    WorkingImage(0).y = 0
    'Copy into Working Image, and find lowest y
    yMin = 0
    FOR i = 1 TO 4
      x = PieceImage(ImageIndex, i).x
      y = PieceImage(ImageIndex, i).y
      WorkingImage(i).x = x
      WorkingImage(i).y = y
      IF y < yMin THEN yMin = y
    NEXT
   
    'Shift so that all y are non negative.
    FOR i = 0 TO 4
      WorkingImage(i).y = WorkingImage(i).y - yMin
    NEXT

    'Find lowest x in row where y=0
    xMin = 100
    FOR i = 0 TO 4
      x = WorkingImage(i).x
      y = WorkingImage(i).y
      IF y = 0 THEN
        IF (x < xMin) THEN xMin = x
      END IF
    NEXT

    'Shift so that x are positive where y=0
    FOR i = 0 TO 4
      WorkingImage(i).x = WorkingImage(i).x - xMin
    NEXT

    'Copy back into PieceImage
    FOR i = 1 TO 4
      x = WorkingImage(i).x
      y = WorkingImage(i).y
      PieceImage(ImageIndex, i).x = x
      PieceImage(ImageIndex, i).y = y
      IF (x = 0) AND (y = 0) THEN 'The 0,0 bit is implied as the first piece of the pentomino, so it isn't explicitly stored.
        PieceImage(ImageIndex, i).x = WorkingImage(0).x
        PieceImage(ImageIndex, i).y = WorkingImage(0).y
      END IF
    NEXT

END SUB

FUNCTION TryPiece (Image AS INTEGER, LowestSquare AS BlockCoord)
  TempTryPiece = 1
  FOR i = 1 TO 4
    x = LowestSquare.x + PieceImage(Image, i).x
    IF x < 0 OR x > BoardWidth THEN x = 0
    y = LowestSquare.y + PieceImage(Image, i).y
    IF y < 0 OR y > BoardHeight THEN y = 0

   
    IF Board(x, y) = 1 THEN
      TempTryPiece = 0
    END IF



  NEXT
  TryPiece = TempTryPiece

  IF TempTryPiece = 1 THEN
    FOR i = 1 TO 4
      x = LowestSquare.x + PieceImage(Image, i).x
      y = LowestSquare.y + PieceImage(Image, i).y
    NEXT
    
  END IF
END FUNCTION

