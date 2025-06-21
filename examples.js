var examples = {};

examples.snake = `; ╭─────────────────────  ╶ |
; \\  ╭─┌╭──╮╭───╮┌─┐─┐╭───╮ |
; ╭─╮ \\│ ╷ ││ . ││   ╮│ '╶╯ |
; ╰───╯└─┴─┘└─┴─┘└─╯─┘╰───╯ |
;===========================/
;
; instructions:
; 
; w - up
; a - left
; s - down
; d - right
;
; apples:
;
; color    chance    %        effect
; red      3 in 4    75%      +1 length
; pink     3 in 32   9.375%   +3 length
; blue     1 in 16   6.25%    +5 length
; yellow   1 in 16   6.25%    remove body temporarily
; cyan     1 in 32   3.125%   +1 apple on screen
;
; snake.asm
;
; $0200-$05ff holds pixel data for a 32x32 screen
; this starts at the top left ($0200), to the top
; right ($021f), for each row down to the bottom
; right ($05ff)
;
; $fe should read random values
; $ff should read a code for the last key pressed
;
; the way this works is by using a range of memory
; $2000-$27ff to hold pointers that say where each
; of the snakes segments are. these will be values
; within the screens range ($0200-$05ff). this is
; twice the screens range in case the snake filled
; every pixel, it would hold a 2 byte pointer for
; the entire snake, from head to tail.
;
; to avoid shifting every segment each time the
; snake moves, another pointer is kept that says
; where the head is at in the segment range. then
; when the snake moves, this pointer is decremented
; and the new head is placed in the new location,
; and the tail can be worked out by adding the
; length to that pointer to get its location.
;
; collision is handled by checking the pixel where
; the head is about to be, and testing for each of
; the colors of the snake or one of the apples.
;
; memory map:
;
;   general purpose 16-bit pointers/temps
;
;   $00-$01   arg
;   $02-$03   brg
;   $04-$05   crg
;
;   game state
;
;   $10-$11   head
;   $12       direction
;   $14-$15   length
;   $16-$17   segments pointer
;   $18-$19   tail
;   $1a-$1b   apple
;   $1c       increment temp for apple loop
;   $1f       counter for yellow powerup
;

.palette 1
.cps 5000        ; cycles per second
                 ; 4000 recommended
                 ; game speeds up as you grow
.org $0600

arg         = $00
argLo       = $00
argHi       = $01
brg         = $02
brgLo       = $02
brgHi       = $03
crg         = $04
crgLo       = $04
crgHi       = $05

head        = $10
headLo      = $10
headHi      = $11
direction   = $12
right       = %00000001
down        = %00000010
left        = %00000100
up          = %00001000
length      = $14
lenLo       = $14
lenHi       = $15
segs        = $16
segsLo      = $16
segsHi      = $17
tail        = $18
tailLo      = $18
tailHi      = $19
apple       = $1a
appleLo     = $1a
appleHi     = $1b
appleI      = $1c
powerup     = $1f

random      = $fe
lastKey     = $ff
upKey       = $77
leftKey     = $61
downKey     = $73
rightKey    = $64

mask1b      = %00000001
mask2b      = %00000011
mask3b      = %00000111
mask4b      = %00001111
mask5b      = %00011111
mask6b      = %00111111
mask7b      = %01111111
segsMask    = %00100111 ; $2000-$27ff

black       = %00000000
cyan        = %00011111
pink        = %11100111
yellow      = %11111000
red         = %11100000
blue        = %00000111
green       = %00011000

scrLo       = $00
scrHi       = $02
scrHiM      = $05 ; M - max
scrHiV      = $06 ; V - overflow
segArrLo    = $00
segArrHi    = $20
segArrHiV   = $28

init:
ldx #$ff
txs

lda #scrHi       ; clear screen
sta argHi
lda #scrLo
sta argLo
tay
ldx #scrHiV
clearLoop:
sta (arg),y
iny
bne clearLoop
inc argHi
cpx argHi
bne clearLoop

lda #$10         ; initial position $0410
sta headLo
sta $2000        ; also place in segment array
lda #$04
sta headHi
sta $2001
lda #segArrLo    ; initialize segment pointer
sta segsLo
lda #segArrHi
sta segsHi
lda #$02         ; initial length $0002
sta lenLo
lda #$00
sta lenHi
lda #0           ; initialize powerup counter
sta powerup
jsr genApple     ; generate an apple

loop:
jsr readInput
jsr clearTail
jsr updatePosition
jsr drawHead
jsr wasteTime
jmp loop

wasteTime:
lda lenLo        ; this subroutine should take
sta argLo        ; less time as the snake grows
lda lenHi        ; so prepare the length
sta argHi
lsr argHi        ; divide 4 (max length is 1024)
ror argLo
lsr argHi
ror argLo
lda #$ff         ; and take that from 255
sec              ; which gives values
sbc argLo        ; closer to 255 for short snake
tax              ; closer to 0 for long snake
tloop:           ; which is used as a counter
dex
cpx #0
bne tloop
rts

genApple:
lda random       ; for high byte, we need
and #mask2b      ; within a 2 bit range
ldx #scrHiV
clc
adc #scrHi       ; +2 to get $02-$05
sta appleHi
lda random       ; for low byte just a random
sta appleLo      ; 8 bit value
ldy #0

appleLoop:
lda (apple),y    ; load the (new) apple pixel
cmp #black       ; make sure its empty
beq appleDone    ; if not, start looking
inc appleLo      ; at the next spot,
bne appleLoop
inc appleHi
cpx appleHi      ; x holding the overflow value ($06)
bne appleLoop
lsr appleHi      ; if eq, wrap around
dec appleHi      ; (6 >> 1) - 1 = 2
bne appleLoop    ; 2 != 0 (branch always)

appleDone:       ; here we have a valid spot
lda random       ; so we can pick a color on these
and #mask5b      ; odds, for a random value (5 lsb):
cmp #0           ; 00000 - cyan
bne noCyanApple
lda #cyan
sta (apple),y
rts

noCyanApple:     ; 4 lsb = 1111, blue
and #mask4b      ; 01111 - blue
cmp #mask4b      ; 11111 - blue
bne noBlueApple
lda #blue
sta (apple),y
rts

noBlueApple:     ; 3 lsb = 000, pink
and #mask3b      ; 00000 - cyan (above)
cmp #0           ; 01000 - pink
bne noPinkApple  ; 10000 - pink
lda #pink        ; 11000 - pink
sta (apple),y
rts

noPinkApple:     ; 3 lsb = 000, yellow
cmp #mask3b      ; 00111 - yellow
bne noYellowApple; 01111 - blue (above)
lda #yellow      ; 10111 - yellow
sta (apple),y    ; 11111 - blue (above)
rts

noYellowApple:   ; everything else, red
lda #red
sta (apple),y
rts

updatePosition:
lda direction    ; directions are 8,4,2,1

checkMovingRight:
lsr              ; so we can right shift 
bcc checkMovingDown ; and check carry
inc headLo       ; x direction
lda #mask5b      ; is within a 5 bit range
bit headLo       ; check if they are 0
beq crash        ; if so (wrapped around) crash
rts

checkMovingDown:
lsr
bcc checkMovingLeft
lda headLo       ; y direction
sec
adc #mask5b      ; add that range + 1 (sec)
sta headLo
bcc dontCarry
lda #scrHiM      ; check max value (if carry)
cmp headHi
beq crash        ; if so (max+1) crash
inc headHi
dontCarry:
rts

checkMovingLeft:
lsr
bcc moveUp
lda #mask5b
bit headLo       ; here check 0 first
beq crash        ; if so its about to wrap, crash
dec headLo       ; otherwise, decrement
rts

moveUp:
lda headLo
clc
sbc #mask5b      ; sub 5 bit range - 1 (clc)
sta headLo
bcs dontBorrow
lda #scrHi       ; check min value (if borrow)
cmp headHi
beq crash        ; if so (min-1) crash
dec headHi
dontBorrow:
rts

crash:
jmp init

drawHead:
ldy #0
lda (head),y     ; load the current pixel
cmp #green       ; if snake is already there, crash
beq crash
ldx #0           ; x says how many apples to generate
                 ; or $ff if the powerup is to be set
checkRedApple:   ; otherwise, start checking apples
cmp #red
bne checkCyanApple
inx              ; if red, generate 1 apple
inc lenLo        ; length += 1
bne noApple
inc lenHi
bpl noApple      ; branch always

checkCyanApple:
cmp #cyan
bne checkPinkApple
inx              ; if cyan, just generate 2 apples
inx
bpl noApple

checkPinkApple:
cmp #pink
bne checkYellowApple
inx              ; if pink, generate 1 apple
lda lenLo
clc
adc #3           ; length += 3
sta lenLo
bcc noApple
inc lenHi
bpl noApple

checkYellowApple:
cmp #yellow
bne checkBlueApple
ldx #$ff         ; if yellow, mark x with $ff
bne noApple

checkBlueApple:
cmp #blue
bne noApple
inx              ; if blue, generate 1 apple
lda lenLo
clc
adc #5           ; length += 5
sta lenLo
bcc noApple
inc lenHi
bpl noApple

noApple:
lda headLo       ; save the head pointer
sta (segs),y     ; to segment array
iny
lda headHi
sta (segs),y
lda #green       ; and draw head
dey
sta (head),y
lda (tail),y     ; load tail
cmp #green       ; if its green
bne dontClearTail
lda #black       ; draw black
sta (tail),y
dontClearTail:

cpx #$ff         ; x = $ff
beq clrAndGen    ; powerup

cpx #0           ; x = 0
beq dontGenApple ; no apples
stx appleI       ; otherwise, generate x apples
doGenApple:
jsr genApple
dec appleI
bne doGenApple
dontGenApple:
rts

clrAndGen:
jsr clearSnake  ; powerup clears the snake
jsr genApple    ; and generates an apple
lda #$1f        ; and lasts 31 frames
sta powerup
rts

clearTail:
lda powerup     ; if powerup active,
bne skip        ; skip forward
                ; here we need to decrement the pointer,
                ; update it, and then add the length
                ; to get the tail pointer. everything
                ; * 2 since they are 2 byte pointers.
                ; the tail pointer will end up in 'arg'.
lda segsLo      ; get the segment pointer
clc
adc #$fe        ; subtract 2
sta segsLo
lda segsHi
adc #$07        ; subtract 1 from hi byte if carry set
and #segsMask   ; mask to keep it in range
sta segsHi      ; update pointer
adc lenHi       ; add length
adc lenHi       ; * 2
sta argHi       ; and store hi byte
lda lenLo       ; take length low byte
asl             ; * 2
bcc dontCarry2
inc argHi       ; carry
clc
dontCarry2:
adc segsLo      ; add pointer low byte
sta argLo       ; store lo byte
lda argHi
adc #0          ; carry
and #segsMask   ; mask
sta argHi       ; re-store hi byte
bne dontSkip    ; skip past 'skip'

skip:           ; here we just need to
dec powerup     ; decrement the powerup counter
lda segsLo      ; get the (old) pointer
sta argLo       ; which will be the tail pointer
clc
adc #$fe        ; then subtract 2
sta segsLo      ; to update it
lda segsHi
sta argHi
adc #$07        ; hi byte - 1 if carry
and #segsMask   ; mask
sta segsHi

dontSkip:
ldy #0
lda (arg),y     ; take the tail pointer
sta tailLo      ; and save it for later
iny
lda (arg),y
sta tailHi
rts

clearSnake:     ; clears the snake (except the head)
lda segsLo      ; get segment pointer
sta argLo
lda segsHi
sta argHi
lda lenLo       ; get the length
sta brgLo
lda lenHi
sta brgHi
ldy #0
beq skipHead

clearSnakeLoop:
lda (arg),y     ; arg is where we are at in the seg array
sta crgLo       ; which points to a pointer
iny
lda (arg),y
sta crgHi
dey
tya
sta (crg),y     ; where we need to clear

skipHead:
inc argLo       ; add 2
inc argLo
bne dontCarry4
inc argHi
lda #segsMask   ; mask
and argHi
sta argHi
dontCarry4:
dec brgLo       ; brg is the iteration count
lda #$ff
cmp brgLo
bne clearSnakeLoop
dec brgHi
cmp brgHi
bne clearSnakeLoop
rts

readInput:
ldx lastKey
ldy direction

checkUp:
lda #up        ; up = 8
cpx #upKey
bne checkLeft
cpy #down      ; dont move backwards
bne doneChecking
rts

checkLeft:
lsr            ; left = 4
cpx #leftKey
bne checkDown
cpy #right
bne doneChecking
rts

checkDown:
lsr            ; down = 2
cpx #downKey
bne checkRight
cpy #up
bne doneChecking
rts

checkRight:
lsr            ; right = 1
cpx #rightKey 
bne dontUpdateDir
cpy #left
bne doneChecking
rts

doneChecking:
sta direction
dontUpdateDir:
rts

.org $fffc
.word init`;

examples.tetris = `; -TETRIS-
; W : rotate
; A : move left
; S : move down
; D : move right

; cycles per second 
; 5000-7500 recommended
.cps 5000
.palette 2

random =       $fe
lastKey =      $ff
wKey =         $77
aKey =         $61
sKey =         $73
dKey =         $64

blockData =    $20
blockX =       $20
blockY =       $21
block =        $28
blockLo =      $28
blockHi =      $29
newData =      $10
newX =         $10
newY =         $11
newBlock =     $18
newLo =        $18
newHi =        $19
rightSet =     $32
bottomSet =    $33
leftSet =      $34
topSet =       $35
rotateSet =    $37
leftClear =    $32
topClear =     $33
rightClear =   $34
bottomClear =  $35
rotateClear =  $36
tempDir =      $0f
pieceColor =   $30
tempData =     $40
tempBlock =    $48
tempSet =      $00
tempClear =    $01
minY =         $00
maxY =         $01
curRow =       $02
curRowLo =     $02
curRowHi =     $03
nextY =        $06
nextRow =      $04
nextRowLo =    $04
nextRowHi =    $05
cxpcy =        $00
cymcx =        $01
rect =         $00
rectLo =       $00
rectHi =       $01
rectWidth =    $02
rectColor =    $03

red =          $04
orange =       $0e
yellow =       $18
green =        $14
cyan =         $78
blue =         $64
purple =       $66
black =        $00
gray =         $5d

wallKickEnabled = 1 ; slightly buggy

.org $0600

jsr drawBorder
jsr newPiece
loop:
ldy #$80
readLoop:
jsr readInput
dey
bne readLoop
jsr moveDown
jmp loop

readInput:
ldx #0
lda lastKey
cmp #aKey
bne checkRight
stx lastKey
jmp moveLeft
checkRight:
cmp #dKey
bne checkRotate
stx lastKey
jmp moveRight
checkRotate:
cmp #wKey
bne checkDown
stx lastKey
jmp moveRotate
checkDown:
cmp #sKey
bne doneChecking
stx lastKey
jmp moveDown
doneChecking:
rts

moveRight:
jsr copyPosition
ldx #0
lda rightSet
sta tempDir

moveRightLoop:
inc newX,x
inc newLo,x
lsr tempDir
bcs contMovingRight
lda (newBlock,x)
beq contMovingRight
lda #0
rts
contMovingRight:
inx
inx
cpx #$8
bne moveRightLoop
lda rightSet
sta tempSet
lda rightClear
sta tempClear
jsr redrawPiece
jsr copyPositionBack
lda #1
rts

moveLeft:
jsr copyPosition
ldx #0
lda leftSet
sta tempDir
           
moveLeftLoop:
dec newX,x
dec newLo,x
lsr tempDir
bcs contMovingLeft
lda (newBlock,x)
beq contMovingLeft
lda #0
rts
contMovingLeft:
inx
inx
cpx #$8
bne moveLeftLoop
lda leftSet
sta tempSet
lda leftClear
sta tempClear
jsr redrawPiece
jsr copyPositionBack
lda #1
rts

moveDown:
jsr copyPosition
ldx #0
lda bottomSet
sta tempDir
           
moveDownLoop:
inc newY,x
lda newLo,x
clc
adc #$20
sta newLo,x
bcc dontInc
inc newHi,x
dontInc:
lsr tempDir
bcs contMovingDown
lda (newBlock,x)
beq contMovingDown
jsr clearRows
jsr newPiece
rts
contMovingDown:
inx
inx
cpx #$8
bne moveDownLoop
lda bottomSet
sta tempSet
lda bottomClear
sta tempClear
jsr redrawPiece
jsr copyPositionBack
rts

clearRows:

ldx #2
lda blockY
minLoop:
cmp blockY,x
bmi noMin
lda blockY,x
noMin:
inx
inx
cpx #6
bne minLoop
sta minY

lda $27
maxLoop:
cmp blockY,x
bpl noMax
lda blockY,x
noMax:
dex
dex
bne maxLoop
sta maxY

lda #0
sta nextRowLo

checkRowsLoop:
jsr checkRow
inc minY
ldx maxY
cpx minY
bpl checkRowsLoop
rts

checkRow:
ldy minY
lda yLoByte,y
sta curRowLo
lda yHiByte,y
sta curRowHi
ldy #0

checkRowLoop:
lda (curRow),y
bne contCheckingRow
rts
contCheckingRow:
iny
cpy #10
bne checkRowLoop
ldy minY
dey
sty nextY
bpl skipOne

shiftNextRow:
lda nextRowLo
sta curRowLo
lda nextRowHi
sta curRowHi
dec nextY
ldy nextY
skipOne:
lda yLoByte,y
sta nextRowLo
lda yHiByte,y
sta nextRowHi
ldx #0
ldy #0
shiftRowLoop:

lda (nextRow),y
sta (curRow),y
beq contShiftingRow
inx
contShiftingRow:
iny
cpy #10
bne shiftRowLoop
cpx #0
bne shiftNextRow
rts

moveRotate:
lda pieceColor
cmp #yellow
bne notYellow      ; dont rotate square piece
rts
notYellow:
lda #0
cmp #wallKickEnabled
bne doWallKick
jmp tryRotate
doWallKick:
jsr tryRotate
beq tryLeft
rts

tryLeft:           ; first rotate failed, nothing redrawn
jsr copyRotation
jsr moveLeft
beq tryRight
jsr tryRotate
beq tryLeft2
rts

tryLeft2:
jsr moveLeft
beq tryRight
jsr tryRotate
beq tryRight
rts

tryRight:
jsr redrawRotation
jsr copyRotationBack
jsr moveRight
beq doneTryingRotate
jsr tryRotate
beq tryRight2
rts

tryRight2:
jsr moveRight
beq doneTryingRotate
jsr tryRotate
beq doneTryingRotate
rts

doneTryingRotate:
jsr redrawRotation
jsr copyRotationBack
rts

tryRotate:
lda rotateSet
sta tempDir
                   ; formulas  are
                   ; new x = cx + cy - y
                   ; new y = cy - cx + x
lda blockX
sta newX
clc
adc blockY            ; precompute cx + cy
sta cxpcy
lda blockY
sta newY
sec
sbc blockX            ; precompute cy - cx
sta cymcx
lda blockLo
sta newLo
lda blockHi
sta newHi
ldx #0

moveRotateLoop:
lda cxpcy            ; A = cx + cy
sec
sbc blockY,x          ; A -= y
sta newX,x          ; new x = A
lda cymcx            ; A = cy - cx
clc
adc blockX,x          ; A += x
sta newY,x          ; new y = A
tay
lda yHiByte,y
sta newHi,x
lda yLoByte,y
clc
adc newX,x
sta newLo,x
lsr tempDir
bcs contMovingRotate
lda (newBlock,x)
beq contMovingRotate
lda #0
rts

contMovingRotate:
inx
inx
cpx #$8
bne moveRotateLoop
ldx rightSet
lda bottomSet
stx bottomSet
ldx leftSet
sta leftSet
lda topSet
stx topSet
sta rightSet
lda rotateSet
sta tempSet
lda rotateClear
sta tempClear
jsr redrawPiece
jsr copyPositionBack
lda #1
rts

redrawPiece:
ldx #0
txa

clearLoop:
lsr tempClear
bcs dontClear
sta (block,x)
dontClear:
inx
inx
cpx #8
bne clearLoop
ldx #0
lda pieceColor

setLoop:
lsr tempSet
bcs dontSet
sta (newBlock,x)
dontSet:
inx
inx
cpx #8
bne setLoop
rts

redrawRotation:
ldx #0
txa

clearRotationLoop:
sta (block,x)
inx
inx
cpx #8
bne clearRotationLoop
ldx #0
lda pieceColor

setRotationLoop:
sta (tempBlock,x)
inx
inx
cpx #8
bne setRotationLoop
rts

copyPosition:
ldx #$0f
copyLoop:
lda blockData,x
sta newData,x
dex
bpl copyLoop
rts

copyPositionBack:
ldx #$0f
copyBackLoop:
lda newData,x
sta blockData,x
dex
bpl copyBackLoop
rts

copyRotation:
ldx #$0f
copyRotationLoop:
lda blockData,x
sta tempData,x
dex
bpl copyRotationLoop
rts

copyRotationBack:
ldx #$0f
copyRotationBackLoop:
lda tempData,x
sta blockData,x
dex
bpl copyRotationBackLoop
rts

drawPiece:
lda pieceColor
ldx #0
drawLoop:
sta (block,x)
inx
inx
cpx #8
bne drawLoop
rts

drawBorder:
lda #$aa
sta rectLo
lda #$02
sta rectHi
ldy #12
ldx #22
lda #gray
jsr drawRect

lda #$cb
sta rectLo
lda #$02
sta rectHi
ldy #10
ldx #20
lda #black

drawRect:
sty rectWidth
sta rectColor
ldy #0
beq innerLoop
outerLoop:
ldy #0
lda rectLo
clc
adc #$20
sta rectLo
bcc dontIncRect
inc rectHi
dontIncRect:
lda rectColor
innerLoop:
sta (rect),y
iny
cpy rectWidth
bne innerLoop
dex
bne outerLoop
rts

newPiece:
random7:
lda random
and #%00000111
beq random7
tay
dey
ldx #0

clc
newPieceLoop:
lda initBlocks,y
sta blockData,x

tya
adc #7
tay
inx
cpx #$18
bne newPieceLoop

jsr drawPiece
rts

yHiByte: 
  .db $02, $02
  .db $03, $03, $03, $03, $03, $03, $03, $03
  .db $04, $04, $04, $04, $04, $04, $04, $04
  .db $05, $05

yLoByte:
  .db $cb, $eb
  .db $0b, $2b, $4b, $6b, $8b, $ab, $cb, $eb
  .db $0b, $2b, $4b, $6b, $8b, $ab, $cb, $eb
  .db $0b, $2b

initBlocks:
; block 1 xy
  .db 4, 4, 4, 4, 4, 4, 4
  .db 1, 1, 1, 1, 1, 1, 1

; block 2 xy
  .db 3, 5, 4, 4, 3, 3, 4
  .db 0, 0, 0, 0, 1, 0, 0

; block 3 xy
  .db 4, 3, 5, 5, 5, 3, 3
  .db 0, 1, 0, 0, 1, 1, 1

; block 4 xy
  .db 5, 5, 5, 3, 6, 5, 5
  .db 1, 1, 1, 1, 1, 1, 1

; block 1 lo/hi
  .db $ef, $ef, $ef, $ef, $ef, $ef, $ef
  .db $02, $02, $02, $02, $02, $02, $02

; block 2 lo/hi
  .db $ce, $d0, $cf, $cf, $ee, $ce, $cf
  .db $02, $02, $02, $02, $02, $02, $02

; block 3 lo/hi
  .db $cf, $ee, $d0, $d0, $f0, $ee, $ee
  .db $02, $02, $02, $02, $02, $02, $02

; block 4 lo/hi
  .db $f0, $f0, $f0, $ee, $f1, $f0, $f0
  .db $02, $02, $02, $02, $02, $02, $02

; block color
  .db red, orange, yellow, green, cyan, blue, purple

; redisual
  .db 0,0,0,0,0,0,0

; block dir right
  .db %0011, %0101, %0011, %1010, %0111, %0101, %0101

; block dir bottom
  .db %0100, %0010, %0110, %0010, %0000, %0010, %0010

; block dir left
  .db %1100, %1001, %1100, %0101, %1101, %1001, %1001

; block dir top
  .db %0001, %1000, %1001, %0001, %0000, %0100, %0001

; block dir rotation(clear)
  .db %1001, %0001, %1111, %0011, %0001, %0001, %1011

; block dir rotation(set)
  .db %0101, %0001, %1111, %1001, %0001, %0001, %0111

.org $fffc
.word $0600`;

examples['2048'] = `; 2048
; wasd to play

.palette = 0
.cps = 10000
.org $0600

ldx #0
copyLoop:
lda dirData,x
sta $70,x
inx
inx
cpx #$80
bne copyLoop

lda #1
sta $0f

jsr randomTile
jsr randomTile
jsr draw

loop:
jsr readInput
jmp loop

readInput:
ldy #0
lda $ff
sty $ff
cmp #$77
bne checkLeft
jmp moveUp
checkLeft:
cmp #$61
bne checkDown
jmp moveLeft
checkDown:
cmp #$73
bne checkRight
jmp moveDown
checkRight:
cmp #$64
bne doneChecking
jmp moveRight
doneChecking:
rts

draw:
ldx #0
drawLoop:
lda blockLocsLo,x
sta $00
lda blockLocsHi,x
sta $01
lda $10,x
jsr drawBlock
inx
cpx #$10
bne drawLoop
rts

drawBlock:
ldy #$00
sta ($00),y
iny
sta ($00),y
iny
sta ($00),y
ldy #$20
sta ($00),y
iny
sta ($00),y
iny
sta ($00),y
ldy #$40
sta ($00),y
iny
sta ($00),y
iny
sta ($00),y
rts

moveRight:
ldx #0
stx $0f
ldx #$70
jsr merge
ldx #$78
jsr merge
ldx #$80
jsr merge
ldx #$88
jsr merge
jsr randomTile
jsr draw
rts

moveLeft:
ldx #0
stx $0f
ldx #$90
jsr merge
ldx #$98
jsr merge
ldx #$a0
jsr merge
ldx #$a8
jsr merge
jsr randomTile
jsr draw
rts

moveDown:
ldx #0
stx $0f
ldx #$b0
jsr merge
ldx #$b8
jsr merge
ldx #$c0
jsr merge
ldx #$c8
jsr merge
jsr randomTile
jsr draw
rts

moveUp:
ldx #0
stx $0f
ldx #$d0
jsr merge
ldx #$d8
jsr merge
ldx #$e0
jsr merge
ldx #$e8
jsr merge
jsr randomTile
jsr draw
rts

merge:
stx $28
lda ($00,x)
sta $24
lda ($02,x)
sta $25
lda ($04,x)
sta $26
lda ($06,x)
sta $27

lda #$23
sta $00
lda #$00
sta $01

ldx #0
stx $20
stx $21
stx $22
stx $23
lda $27
beq zero1
sta ($00,x)
dec $00
zero1:
lda $26
beq zero2
sta ($00,x)
dec $00
zero2:
lda $25
beq zero3
sta ($00,x)
dec $00
zero3:
lda $24
sta ($00,x)

lda $22
beq mergeCase0
cmp $23
bne mergeCase2
lda $21
beq mergeCase1
cmp $20
bne mergeCase1
; case 4 1111
inc $23
inc $21
lda $21
sta $22
lda #$0
sta $21
sta $20
beq mergeCopyBack

mergeCase1:
; case 1 0011
inc $23
lda $21
sta $22
lda $20
sta $21
lda #$0
sta $20
beq mergeCopyBack

mergeCase2:
cmp $21
bne mergeCase3
; case 2 0110
inc $22
lda $20
sta $21
lda #$0
sta $20
beq mergeCopyBack

mergeCase3:
lda $20
beq mergeCase0
cmp $21
bne mergeCase0
; case 3 1100
inc $21
lda #$0
sta $20
mergeCase0:

mergeCopyBack:
ldx $28

ldy $0f
bne skipCheck

lda $20
cmp ($00,x)
bne setChangeFlag
lda $21
cmp ($02,x)
bne setChangeFlag
lda $22
cmp ($04,x)
bne setChangeFlag
lda $23
cmp ($06,x)
bne setChangeFlag
rts
setChangeFlag:
inc $0f
skipCheck:

lda $20
sta ($00,x)
lda $21
sta ($02,x)
lda $22
sta ($04,x)
lda $23
sta ($06,x)
rts

randomTile:
ldy $0f
bne changeFlagSet
rts
changeFlagSet:
lda $fe
cmp #25
bpl randomLoop
iny
randomLoop:
lda $fe
and #$0f
tax
lda $10,x
bne randomLoop
tya
sta $10,x
rts

dirData:
  ; right
  .db $10, $00, $11, $00, $12, $00, $13, $00
  .db $14, $00, $15, $00, $16, $00, $17, $00
  .db $18, $00, $19, $00, $1a, $00, $1b, $00
  .db $1c, $00, $1d, $00, $1e, $00, $1f, $00
  ; left
  .db $13, $00, $12, $00, $11, $00, $10, $00
  .db $17, $00, $16, $00, $15, $00, $14, $00
  .db $1b, $00, $1a, $00, $19, $00, $18, $00
  .db $1f, $00, $1e, $00, $1d, $00, $1c, $00
  ; down
  .db $10, $00, $14, $00, $18, $00, $1c, $00
  .db $11, $00, $15, $00, $19, $00, $1d, $00
  .db $12, $00, $16, $00, $1a, $00, $1e, $00
  .db $13, $00, $17, $00, $1b, $00, $1f, $00
  ; up
  .db $1c, $00, $18, $00, $14, $00, $10, $00
  .db $1d, $00, $19, $00, $15, $00, $11, $00
  .db $1e, $00, $1a, $00, $16, $00, $12, $00
  .db $1f, $00, $1b, $00, $17, $00, $13, $00

blockLocsLo:
  .db $e7, $ec, $f1, $f6
  .db $87, $8c, $91, $96
  .db $27, $2c, $31, $36
  .db $c7, $cc, $d1, $d6
blockLocsHi:
  .db $02, $02, $02, $02
  .db $03, $03, $03, $03
  .db $04, $04, $04, $04
  .db $04, $04, $04, $04

.org $fffc
.dw $0600`;

examples.mandelbrot = `.fp 4.12        ; fixed point format
.palette 3

CENTER_X = -f.0.5
CENTER_Y = f.0.0
VIEW_WIDTH = f.2.25
ITERATIONS = 2  ; * 256
BS_MODE = 0


;CENTER_X = -f.0.211
;CENTER_Y = -f.0.818
;VIEW_WIDTH = f.0.01

;CENTER_X = f.0.045
;CENTER_Y = -f.0.638
;VIEW_WIDTH = f.0.0075


MIN_X = CENTER_X-VIEW_WIDTH/2
MAX_X = CENTER_X+VIEW_WIDTH/2
MIN_Y = CENTER_Y-VIEW_WIDTH/2
MAX_Y = CENTER_Y+VIEW_WIDTH/2

op1 = $00
op2 = $01
resLo = $02
resHi = $03
resQ = $02
resR = $03

op1Lo = $00
op1Hi = $01
op2Lo = $02
op2Hi = $03
resLL = $04
resLH = $05
resHL = $06
resHH = $07
resQL = $04
resQH = $05
resRL = $06
resRH = $07
op1LL = $08

xn = $10
yn = $11
iLo = $12
iHi = $13
cxLo = $14
cxHi = $15
cyLo = $16
cyHi = $17
zxLo = $18
zxHi = $19
zyLo = $1a
zyHi = $1b
zx2Lo = $1c
zx2Hi = $1d
zy2Lo = $1e
zy2Hi = $1f
spLo = $20
spHi = $21

.org $0600
.cps 10000000

; for (y = 0...31)
;   for (x = 0...31)
;     zx = cx = x/32 * (max_x-min_x) - min_x
;     zy = cy = y/32 * (max_y-min_y) - min_y
;     i = 0
;     zx2 = zx*zx
;     zy2 = zy*zy
;     while (i < max_i)
;       zy = 2*zx*zy + cy
;       zx = zx2 - zy2 + cx
;       zx2 = zx*zx
;       zy2 = zy*zy
;       if (zx2 + zy2 >= 4) break
;       i++
;     color = i
;     drawPixel(x,y)

start:
  lda #BS_MODE
  bne noBsMode
  lda #$cd      ; cmp absolute
  sta bs        ; replaces the jmp
noBsMode:

  lda #$02
  sta spHi      ; screen pointer = $0200
  lda #0
  sta spLo
  sta yn        ; y = 0
yLoop:
  lda #0
  sta xn        ; x = 0
xLoop:
  ldx xn        ; scaled coords are pre-calculated
  lda xLo,x     ; cx = x/31 * (max_x-min_x) - min_x
  sta cxLo      ; zx = cx
  sta zxLo
  lda xHi,x
  sta cxHi
  sta zxHi

  ldy yn        ; cy = y/31 * (max_y-min_y) - min_y
  lda yLo,y     ; zy = cy
  sta cyLo
  sta zyLo
  lda yHi,y
  sta cyHi
  sta zyHi

  lda #0        ; i = 0
  sta iLo
  sta iHi

  lda zxLo     ; zx2 = zx*zx
  sta $00
  lda zxHi
  sta $01
  jsr ssq16
  lda $05
  sta zx2Lo
  lda $06
  sta zx2Hi

  lda zyLo      ; zy2 = zy*zy
  sta $00
  lda zyHi
  sta $01
  jsr ssq16
  lda $05
  sta zy2Lo
  lda $06
  sta zy2Hi

iLoop:
  lda zxLo      ; zy = 2*zx*zy + cy
  sta $00
  lda zxHi
  sta $01
  lda zyLo
  sta $02
  lda zyHi
  sta $03
  jsr smul16    ; zx * zy
  asl $05       ; * 2
  rol $06
  ldx #$05
bs:
  jsr abs16
  lda $05
  clc
  adc cyLo      ; + cy
  sta zyLo
  lda $06
  adc cyHi
  sta zyHi

  lda zx2Lo     ; zx = zx2 - zy2 + cx
  sec
  sbc zy2Lo
  sta zxLo
  lda zx2Hi
  sbc zy2Hi
  sta zxHi
  lda zxLo
  clc
  adc cxLo
  sta zxLo
  lda zxHi
  adc cxHi
  sta zxHi

  lda zxLo     ; zx2 = zx*zx
  sta $00
  lda zxHi
  sta $01
  jsr ssq16
  lda $05
  sta zx2Lo
  lda $06
  sta zx2Hi

  lda zyLo      ; zy2 = zy*zy
  sta $00
  lda zyHi
  sta $01
  jsr ssq16
  lda $05
  sta zy2Lo
  lda $06
  sta zy2Hi

  lda zx2Hi     ; if (zx2 + zxy >= 4) break
  clc
  adc zy2Hi
  bcs breakILoop
  cmp #127
  bpl breakILoop

  inc iLo       ; i++
  bne iLoop     ; if (i >= maxi) break
  inc iHi
  lda #ITERATIONS
  cmp iHi
  beq breakILoop
  jmp iLoop

breakILoop:
  lda iLo       ; use i low byte as color
  ldy #0        ; draw pixel
  sta (spLo),y

  inc spLo      ; inc screen pointer
  bne dontIncSpHi
  inc spHi
  dontIncSpHi:
  ldx #32       ; x++
  inc xn        ; if (x >= 32) break
  cpx xn
  beq breakXLoop
  jmp xLoop

breakXLoop:
  ldy #32       ; y++
  inc yn        ; if (y >= 32) break
  cpy yn
  beq breakYLoop
  jmp yLoop

breakYLoop:
  hlt

; s16 = u16
abs16:
  lda $01,x
  bpl abs16pos
  eor #$ff
  sta $01,x
  lda $00,x
  eor #$ff
  sec
  adc #0
  sta $00,x
  lda $01,x
  adc #0
  sta $01,x
abs16pos:
  rts

; u16 ** 2 = u32
; u4.12 ** 2 = u4.12 (in resLH and resHL)
usq16:
  lda op1Lo
  sta op2Lo
  lda op1Hi
  sta op2Hi

; u16 * u16 = u32
; u4.12 * u4.12 = u4.12 (in resLH and resHL)
umul16:
  lda #0
  sta resLL
  sta resLH
  sta resHL
  sta resHH
  sta $08
  sta $09
  ldx #16
umul16Loop:
  lda op1Lo
  and #1
  beq umul16skipAdd
  clc
  lda resLL
  adc op2Lo
  sta resLL
  lda resLH
  adc op2Hi
  sta resLH
  lda resHL
  adc $08
  sta resHL
  lda resHH
  adc $09
  sta resHH
umul16skipAdd:
  lsr op1Hi
  ror op1Lo
  asl op2Lo
  rol op2Hi
  rol $08
  rol $09
  dex
  bne umul16Loop
  lsr resHH
  ror resHL
  ror resLH
  ror resLL
  lsr resHH
  ror resHL
  ror resLH
  ror resLL
  lsr resHH
  ror resHL
  ror resLH
  ror resLL
  lsr resHH
  ror resHL
  ror resLH
  ror resLL
  rts

; s4.12 ** 2 = s4.12 (in resLH and resHL)
ssq16:
  lda op1Lo
  sta op2Lo
  lda op1Hi
  sta op2Hi

; s4.12 * s4.12 = s4.12 (in resLH and resHL)
smul16:
  lda op1Hi
  eor op2Hi
  sta $0a
  ldx #0
  jsr abs16
  ldx #2
  jsr abs16
  jsr umul16
  lda $0a
  bmi smul16fixSign
  rts
smul16fixSign:
  lda resHH
  eor #$ff
  sta resHH
  lda resHL
  eor #$ff
  sta resHL
  lda resLH
  eor #$ff
  sta resLH
  lda resLL
  eor #$ff
  sec
  adc #0
  sta resLL
  lda resLH
  adc #0
  sta resLH
  lda resHL
  adc #0
  sta resHL
  lda resHH
  adc #0
  sta resHH
  rts

dx = (MAX_X - MIN_X) / 31
dy = (MAX_X - MIN_X) / 31

xLo:
  .byte <(MIN_X+0*dx), <(MIN_X+1*dx), <(MIN_X+2*dx), <(MIN_X+3*dx)
  .byte <(MIN_X+4*dx), <(MIN_X+5*dx), <(MIN_X+6*dx), <(MIN_X+7*dx)
  .byte <(MIN_X+8*dx), <(MIN_X+9*dx), <(MIN_X+10*dx), <(MIN_X+11*dx)
  .byte <(MIN_X+12*dx), <(MIN_X+13*dx), <(MIN_X+14*dx), <(MIN_X+15*dx)
  .byte <(MIN_X+16*dx), <(MIN_X+17*dx), <(MIN_X+18*dx), <(MIN_X+19*dx)
  .byte <(MIN_X+20*dx), <(MIN_X+21*dx), <(MIN_X+22*dx), <(MIN_X+23*dx)
  .byte <(MIN_X+24*dx), <(MIN_X+25*dx), <(MIN_X+26*dx), <(MIN_X+27*dx)
  .byte <(MIN_X+28*dx), <(MIN_X+29*dx), <(MIN_X+30*dx), <(MIN_X+31*dx)
xHi:
  .byte >(MIN_X+0*dx), >(MIN_X+1*dx), >(MIN_X+2*dx), >(MIN_X+3*dx)
  .byte >(MIN_X+4*dx), >(MIN_X+5*dx), >(MIN_X+6*dx), >(MIN_X+7*dx)
  .byte >(MIN_X+8*dx), >(MIN_X+9*dx), >(MIN_X+10*dx), >(MIN_X+11*dx)
  .byte >(MIN_X+12*dx), >(MIN_X+13*dx), >(MIN_X+14*dx), >(MIN_X+15*dx)
  .byte >(MIN_X+16*dx), >(MIN_X+17*dx), >(MIN_X+18*dx), >(MIN_X+19*dx)
  .byte >(MIN_X+20*dx), >(MIN_X+21*dx), >(MIN_X+22*dx), >(MIN_X+23*dx)
  .byte >(MIN_X+24*dx), >(MIN_X+25*dx), >(MIN_X+26*dx), >(MIN_X+27*dx)
  .byte >(MIN_X+28*dx), >(MIN_X+29*dx), >(MIN_X+30*dx), >(MIN_X+31*dx)

yLo:
  .byte <(MIN_Y+0*dy), <(MIN_Y+1*dy), <(MIN_Y+2*dy), <(MIN_Y+3*dy)
  .byte <(MIN_Y+4*dy), <(MIN_Y+5*dy), <(MIN_Y+6*dy), <(MIN_Y+7*dy)
  .byte <(MIN_Y+8*dy), <(MIN_Y+9*dy), <(MIN_Y+10*dy), <(MIN_Y+11*dy)
  .byte <(MIN_Y+12*dy), <(MIN_Y+13*dy), <(MIN_Y+14*dy), <(MIN_Y+15*dy)
  .byte <(MIN_Y+16*dy), <(MIN_Y+17*dy), <(MIN_Y+18*dy), <(MIN_Y+19*dy)
  .byte <(MIN_Y+20*dy), <(MIN_Y+21*dy), <(MIN_Y+22*dy), <(MIN_Y+23*dy)
  .byte <(MIN_Y+24*dy), <(MIN_Y+25*dy), <(MIN_Y+26*dy), <(MIN_Y+27*dy)
  .byte <(MIN_Y+28*dy), <(MIN_Y+29*dy), <(MIN_Y+30*dy), <(MIN_Y+31*dy)
yHi:
  .byte >(MIN_Y+0*dy), >(MIN_Y+1*dy), >(MIN_Y+2*dy), >(MIN_Y+3*dy)
  .byte >(MIN_Y+4*dy), >(MIN_Y+5*dy), >(MIN_Y+6*dy), >(MIN_Y+7*dy)
  .byte >(MIN_Y+8*dy), >(MIN_Y+9*dy), >(MIN_Y+10*dy), >(MIN_Y+11*dy)
  .byte >(MIN_Y+12*dy), >(MIN_Y+13*dy), >(MIN_Y+14*dy), >(MIN_Y+15*dy)
  .byte >(MIN_Y+16*dy), >(MIN_Y+17*dy), >(MIN_Y+18*dy), >(MIN_Y+19*dy)
  .byte >(MIN_Y+20*dy), >(MIN_Y+21*dy), >(MIN_Y+22*dy), >(MIN_Y+23*dy)
  .byte >(MIN_Y+24*dy), >(MIN_Y+25*dy), >(MIN_Y+26*dy), >(MIN_Y+27*dy)
  .byte >(MIN_Y+28*dy), >(MIN_Y+29*dy), >(MIN_Y+30*dy), >(MIN_Y+31*dy)

.org $fffc
.word start`;
