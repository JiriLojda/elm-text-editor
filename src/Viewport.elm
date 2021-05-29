module Viewport exposing (Viewport, changeWidthBy, contentHeight, contentWidth, height, init, left, moveBy, moveTo, moveViewportIfNecessary, shouldViewHorizontalScrollbar, shouldViewVerticalScrollbar, top, updateContentSize, updateSize, width)

-- CONSTANTS


viewportCaretPadding =
    20


viewportOverMoveToCaretMargin =
    10


viewportAllowedOverscroll =
    10



-- TYPES


type Viewport
    = Viewport ViewportInfo


type alias ViewportInfo =
    { top : Float
    , left : Float
    , height : Float
    , width : Float
    , scrollbarSize : Float
    , contentDimensions : ContentDimensions
    }


type alias ContentDimensions =
    { width : Float
    , height : Float
    }



-- INIT


init : ContentDimensions -> Viewport
init dimensions =
    Viewport
        { top = 0
        , left = 0
        , height = 100
        , width = 100
        , contentDimensions = dimensions
        , scrollbarSize = 15
        }



-- ACCESSORS


shouldViewVerticalScrollbar : Viewport -> Bool
shouldViewVerticalScrollbar (Viewport viewport) =
    shouldViewVerticalScrollbarInner viewport


shouldViewVerticalScrollbarInner : ViewportInfo -> Bool
shouldViewVerticalScrollbarInner viewport =
    viewport.contentDimensions.height > viewport.height


shouldViewHorizontalScrollbar : Viewport -> Bool
shouldViewHorizontalScrollbar (Viewport viewport) =
    shouldViewHorizontalScrollbarInner viewport


shouldViewHorizontalScrollbarInner : ViewportInfo -> Bool
shouldViewHorizontalScrollbarInner viewport =
    viewport.contentDimensions.width + viewportAllowedOverscroll > viewport.width


top : Viewport -> Float
top (Viewport viewport) =
    viewport.top


left : Viewport -> Float
left (Viewport viewport) =
    viewport.left


height : Viewport -> Float
height (Viewport viewport) =
    heightInner viewport


heightInner : ViewportInfo -> Float
heightInner viewport =
    if shouldViewHorizontalScrollbarInner viewport then
        viewport.height - viewport.scrollbarSize

    else
        viewport.height


width : Viewport -> Float
width (Viewport viewport) =
    widthInner viewport


widthInner : ViewportInfo -> Float
widthInner viewport =
    if shouldViewVerticalScrollbarInner viewport then
        viewport.width - viewport.scrollbarSize

    else
        viewport.width


contentHeight : Viewport -> Float
contentHeight (Viewport viewport) =
    viewport.contentDimensions.height


contentWidth : Viewport -> Float
contentWidth (Viewport viewport) =
    viewport.contentDimensions.width + viewportAllowedOverscroll



-- MODIFIERS


updateSize : Float -> Float -> Viewport -> Viewport
updateSize newWidth newHeight (Viewport viewport) =
    round
        { viewport
            | width = newWidth
            , height = newHeight
        }


changeWidthBy : Float -> Viewport -> Viewport
changeWidthBy widthDiff (Viewport viewport) =
    round
        { viewport
            | width = viewport.width + widthDiff
        }


updateContentSize : ContentDimensions -> Viewport -> Viewport
updateContentSize dimensions (Viewport viewport) =
    round
        { viewport
            | contentDimensions = dimensions
        }


moveBy : Float -> Float -> Viewport -> Viewport
moveBy newLeft newTop (Viewport viewport) =
    round
        { viewport
            | left = viewport.left + newLeft
            , top = viewport.top + newTop
        }


moveTo : Float -> Float -> Viewport -> Viewport
moveTo newLeft newTop (Viewport viewport) =
    round
        { viewport
            | left = newLeft
            , top = newTop
        }


round : ViewportInfo -> Viewport
round viewport =
    Viewport
        { viewport
            | left =
                viewport.left
                    |> min (viewport.contentDimensions.width + viewportAllowedOverscroll - widthInner viewport)
                    |> max 0
            , top =
                viewport.top
                    |> min (viewport.contentDimensions.height - heightInner viewport)
                    |> max 0
        }



-- SHOULD MOVE


type alias CaretPixelPosition =
    { x : Float
    , y : Float
    }


shouldMoveLeft : ViewportInfo -> CaretPixelPosition -> Bool
shouldMoveLeft viewport caretPos =
    caretPos.x - viewport.left < viewportCaretPadding


shouldMoveRight : ViewportInfo -> CaretPixelPosition -> Float -> Bool
shouldMoveRight viewport caretPos caretWidth =
    caretPos.x - viewport.left > widthInner viewport - viewportCaretPadding - caretWidth


shouldMoveUp : ViewportInfo -> CaretPixelPosition -> Bool
shouldMoveUp viewport caretPos =
    caretPos.y - viewport.top < viewportCaretPadding && viewport.top > 0


shouldMoveDown : ViewportInfo -> CaretPixelPosition -> Bool
shouldMoveDown viewport caretPos =
    caretPos.y - viewport.top > heightInner viewport - viewportCaretPadding


findNewX : ViewportInfo -> CaretPixelPosition -> Float -> Float
findNewX viewport caretPos caretWidth =
    if not (shouldMoveLeft viewport caretPos || shouldMoveRight viewport caretPos caretWidth) then
        viewport.left

    else if shouldMoveLeft viewport caretPos then
        caretPos.x - viewportCaretPadding - viewportOverMoveToCaretMargin + caretWidth

    else
        caretPos.x - widthInner viewport + viewportCaretPadding + viewportOverMoveToCaretMargin


findNewY : ViewportInfo -> CaretPixelPosition -> Float
findNewY viewport caretPos =
    if not (shouldMoveUp viewport caretPos || shouldMoveDown viewport caretPos) then
        viewport.top

    else if shouldMoveUp viewport caretPos then
        caretPos.y - viewportCaretPadding - viewportOverMoveToCaretMargin

    else
        caretPos.y - heightInner viewport + viewportCaretPadding + viewportOverMoveToCaretMargin


shouldMove : ViewportInfo -> CaretPixelPosition -> Float -> Bool
shouldMove viewport caretPos caretWidth =
    shouldMoveLeft viewport caretPos
        || shouldMoveRight viewport caretPos caretWidth
        || shouldMoveUp viewport caretPos
        || shouldMoveDown viewport caretPos


moveViewportIfNecessary : Viewport -> CaretPixelPosition -> Float -> Viewport
moveViewportIfNecessary (Viewport viewport) caretPos caretWidth =
    if shouldMove viewport caretPos caretWidth then
        let
            newX =
                findNewX viewport caretPos caretWidth

            newY =
                findNewY viewport caretPos
        in
        round { viewport | left = newX, top = newY }

    else
        Viewport viewport
