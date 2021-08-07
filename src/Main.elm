module Main exposing (Model)

import Array exposing (Array)
import Browser
import Browser.Dom
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List
import Random exposing (Generator)
import Task
import Time exposing (Posix)


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


type Direction
    = Up
    | Down
    | Left
    | Right


type MoveOutput
    = Move Direction
    | NoOp
    | Reset


type alias Controller =
    Array Direction -> MoveOutput


type alias RawLevel =
    { title : String
    , controller : Controller
    , stage : String
    }


type Tile
    = Floor
    | Death
    | Block
    | Start
    | End


type alias Level =
    { title : String
    , tiles : List Tile
    , width : Int
    , controller : Controller
    }


mapController : (Direction -> MoveOutput) -> Array Direction -> MoveOutput
mapController mapper inputs =
    case last inputs of
        Just input ->
            mapper input

        Nothing ->
            NoOp


applyTimes : Int -> (a -> a) -> a -> a
applyTimes times f i =
    if times <= 0 then
        i

    else
        applyTimes (times - 1) f (f i)


indexedMapController : (Int -> Direction -> MoveOutput) -> Array Direction -> MoveOutput
indexedMapController mapper inputs =
    case last inputs of
        Just input ->
            mapper (Array.length inputs - 1) input

        Nothing ->
            NoOp


countBy : (a -> Bool) -> Array a -> Int
countBy predicate array =
    array
        |> Array.filter predicate
        |> Array.length


count : a -> Array a -> Int
count item =
    countBy (\x -> x == item)


isVertical : Direction -> Bool
isVertical dir =
    case dir of
        Up ->
            True

        Down ->
            True

        Left ->
            False

        Right ->
            False


stage1 : String
stage1 =
    """
xxxxxxxxxxbxxxx
xxxxxxxxxxbbxxx
bbbbbbbbbbb bxx
b            bx
bS           Eb
b            bx
bbbbbbbbbbb bxx
xxxxxxxxxxbbxxx
xxxxxxxxxxbxxxx"""


stage2 : String
stage2 =
    """
xxbbbxxxxxxxxxxbb
bb        x     b
bS      x x x   b
bb      x   x  Eb
xxbbbxxxxxxxxxxbb"""


stage3 : String
stage3 =
    """
x       xxxxxx   E
    bx   xxxxx    
S   x   xxxx   xxx
xxxxx         xxxx
xxxxxbbbbxxxxxxxxx"""


stage4 : String
stage4 =
    """
xbxbxbxbxbxbxbxbxb
b     b xE  x    x
x       xbx   x  b
b  S  x   bbbbx  x
x     x x        b
bxbxbxbxbxbxbxbxbx"""


stage5 : String
stage5 =
    """
bbbxxxxxxxxxxxxxxx
bS  xb   b    xE b
b   x  x x b xb xb
bx x  xb x x     b
b    bx    b xxxxb
bxxxxxbxxxxxxxxxbb
"""


stage6 : String
stage6 =
    """
xxxxxxxxxxxxxxxxb
   x         x  S
 b b  xxbx xx    
 x   x     x  xbb
 xxbxx bxxb  xbbb
 Ex          xbbb
"""


stage7 : String
stage7 =
    """
bbb      xE   b
bbx  xxb xxxx x
bS x  x  x    b
b   x b xbx xbb
b   x b  xx   x
b  x  x    x  b
b  x b x b x  b
bx   x   x   xb
"""


stage8 : String
stage8 =
    """
bx   x   x   xb
b  x x x x b  x
bS b   b   xx b
bxxbxxxbxxxbx x
bE b   b   xx b
b  x x x x b  x
bx   x   x   xb
"""


stage9 : String
stage9 =
    """
bbx    x    
xxx xb x xx 
     b   x  
 xxxbSbxxx x
      xx   x
bxxxxxbx xxb
E         xb
"""


stageDebug =
    """
bbbbbbbbbbbbbbbbb
b               b
b               b
b S           E b
b               b
b               b
bbbbbbbbbbbbbbbbb
    """


stageHoles =
    """
E bbbbbbbbbbb E
  x         x  
b  x   x   x  b
bx x x x x x xb
b             b
bx x x S x x xb
b             b
bx x x x x x xb
b  x   x   x  b
  x         x  
E bbbbbbbbbbb E
    """


reverseDirection : Direction -> Direction
reverseDirection dir =
    case dir of
        Up ->
            Down

        Left ->
            Right

        Right ->
            Left

        Down ->
            Up


twistDirection : Direction -> Direction
twistDirection dir =
    case dir of
        Up ->
            Left

        Left ->
            Up

        Right ->
            Down

        Down ->
            Right


borkDirection : Direction -> Direction
borkDirection dir =
    case dir of
        Up ->
            Left

        Left ->
            Down

        Down ->
            Up

        Right ->
            Right


clockwiseDirection : Direction -> Direction
clockwiseDirection dir =
    case dir of
        Up ->
            Right

        Right ->
            Down

        Down ->
            Left

        Left ->
            Up


counterclockwiseDirection : Direction -> Direction
counterclockwiseDirection dir =
    case dir of
        Up ->
            Left

        Left ->
            Down

        Down ->
            Right

        Right ->
            Up


dedupidity : MoveOutput -> Array Direction -> MoveOutput
dedupidity dup inputs =
    let
        third =
            inputs |> Array.slice 0 -3 |> last
    in
    case ( third, last inputs ) of
        ( Just prev, Just next ) ->
            if prev == next then
                dup

            else
                Move next

        ( Nothing, Just dir ) ->
            Move dir

        ( _, _ ) ->
            NoOp


defaultController : Controller
defaultController =
    mapController
        (\x -> Move x)


last : Array a -> Maybe a
last array =
    Array.get (Array.length array - 1) array


even : Int -> Bool
even n =
    modBy 2 n == 0


everyOther : (a -> a) -> Int -> a -> a
everyOther f time val =
    if even time then
        val

    else
        f val


rawLevels : List RawLevel
rawLevels =
    [ { title = "Get moving!"
      , controller = defaultController
      , stage = stage1
      }
    , { title = "Don't fall off"
      , controller = defaultController
      , stage = stage2
      }

    -- , { title = "Last warm up"
    --   , controller = defaultController
    --   , stage = stage3
    --   }
    , { title = "Opposite day"
      , controller = mapController (\x -> Move (reverseDirection x))
      , stage = stage4
      }
    , { title = "Twisted"
      , controller = mapController (\x -> Move (twistDirection x))
      , stage = stage5
      }
    , { title = "Bork Gulch"
      , controller = mapController (\x -> Move (borkDirection x))
      , stage = stage6
      }
    , { title = "Bipolar"
      , controller =
            indexedMapController
                (\i x ->
                    Move (everyOther reverseDirection i x)
                )
      , stage = stage7
      }
    , { title = "Re:Unravel"
      , controller =
            indexedMapController
                (\i x ->
                    Move (everyOther twistDirection i x)
                )
      , stage = stage8
      }
    , { title = "Alt Bork Café"
      , controller =
            indexedMapController
                (\i x ->
                    Move (everyOther borkDirection i x)
                )
      , stage = stage9
      }
    , { title = "Disobedience"
      , controller =
            indexedMapController
                (\i x ->
                    if even i then
                        Move x

                    else if modBy 3 i == 0 then
                        Move (reverseDirection x)

                    else
                        NoOp
                )
      , stage = stage4
      }
    , { title = "Clock In"
      , controller =
            indexedMapController
                (\i x ->
                    Move (applyTimes i clockwiseDirection x)
                )
      , stage = stage5
      }
    , { title = "Counter Struck"
      , controller =
            indexedMapController
                (\i x ->
                    Move (applyTimes i counterclockwiseDirection x)
                )
      , stage = stage6
      }
    , { title = "Locksmith"
      , controller =
            \inputs ->
                case last inputs of
                    Just dir ->
                        Move
                            (dir
                                |> applyTimes ((inputs |> countBy isVertical) - 1) clockwiseDirection
                                |> applyTimes ((inputs |> countBy (isVertical >> not)) - 1) counterclockwiseDirection
                            )

                    Nothing ->
                        NoOp
      , stage = stage7
      }
    , { title = "Overclocked"
      , controller =
            \inputs ->
                case last inputs of
                    Just dir ->
                        Move
                            (dir
                                |> applyTimes (((inputs |> countBy isVertical) - 1) * 2) clockwiseDirection
                                |> applyTimes ((inputs |> countBy (isVertical >> not)) - 1) counterclockwiseDirection
                            )

                    Nothing ->
                        NoOp
      , stage = stage8
      }
    , { title = "Underclocked"
      , controller =
            \inputs ->
                case last inputs of
                    Just dir ->
                        Move
                            (dir
                                |> applyTimes ((inputs |> countBy isVertical) - 1) clockwiseDirection
                                |> applyTimes (((inputs |> countBy (isVertical >> not)) - 1) * 2) counterclockwiseDirection
                            )

                    Nothing ->
                        NoOp
      , stage = stage9
      }
    , { title = "Oscillate Rift"
      , controller =
            indexedMapController
                (\i x ->
                    if modBy 8 i > 3 then
                        Move (applyTimes i counterclockwiseDirection x)

                    else
                        Move (applyTimes i clockwiseDirection x)
                )
      , stage = stage4
      }
    , { title = "Flappers"
      , controller =
            \inputs ->
                case last inputs of
                    Just dir ->
                        let
                            pred =
                                if isVertical dir then
                                    isVertical

                                else
                                    isVertical >> not

                            flip =
                                inputs |> countBy pred |> even
                        in
                        Move
                            (if flip then
                                reverseDirection dir

                             else
                                dir
                            )

                    Nothing ->
                        NoOp
      , stage = stage5
      }
    , { title = "Dedupidity"
      , controller = dedupidity NoOp
      , stage = stage6
      }
    , { title = "Redupidity"
      , controller = \inputs -> inputs |> Array.map reverseDirection |> dedupidity Reset
      , stage = stage7
      }
    , { title = "Clock mine"
      , controller =
            indexedMapController
                (\i x ->
                    if x == applyTimes i clockwiseDirection Up then
                        Reset

                    else
                        Move x
                )
      , stage = stage8
      }
    , { title = "Counter mine"
      , controller =
            indexedMapController
                (\i x ->
                    if x == applyTimes i counterclockwiseDirection Down then
                        Reset

                    else
                        Move x
                )
      , stage = stage9
      }
    , { title = "Moderation"
      , controller =
            \inputs ->
                case last inputs of
                    Just dir ->
                        let
                            same =
                                inputs |> count dir

                            opposite =
                                inputs |> count (reverseDirection dir)
                        in
                        if same - (opposite * 2) > 5 then
                            Reset

                        else
                            Move dir

                    Nothing ->
                        NoOp
      , stage = stageHoles
      }
    ]


parseTile : Char -> Tile
parseTile tile =
    case tile of
        ' ' ->
            Floor

        'x' ->
            Death

        'b' ->
            Block

        'S' ->
            Start

        'E' ->
            End

        _ ->
            Death


parseLevel : RawLevel -> Level
parseLevel raw =
    let
        tileLines =
            raw.stage |> String.trim |> String.lines

        width =
            tileLines |> List.map String.length |> List.foldl max 0

        tiles =
            tileLines
                |> List.concatMap
                    (\line ->
                        line
                            |> String.padRight width 'x'
                            |> String.toList
                            |> List.map parseTile
                    )
    in
    { title = raw.title
    , controller = raw.controller
    , tiles = tiles
    , width = width
    }


type Screen
    = Home
    | About
    | Game
    | GameOver


type alias ViewPort =
    { width : Float
    , height : Float
    }


type alias Model =
    { screen : Screen
    , touch : Bool
    , level : Int
    , levels : List Level
    , levelCursor : Int
    , levelMoves : List Direction
    , viewPort : ViewPort
    , deaths : Int
    , moves : Int
    }


type Msg
    = ShowHome
    | ShowAbout
    | EnterGame Bool
    | HandleKeyboardEvent String
    | HandleClickEvent Direction
    | InitializeViewPort Browser.Dom.Viewport
    | UpdateViewPort Int Int


concatOrError : Result String a -> Result String (List a) -> Result String (List a)
concatOrError result acc =
    case ( acc, result ) of
        ( Ok list, Ok item ) ->
            Ok (list ++ [ item ])

        ( Err err, _ ) ->
            Err err

        ( _, Err err ) ->
            Err err


init : Int -> ( Model, Cmd Msg )
init flags =
    let
        levels =
            rawLevels
                |> List.map parseLevel

        model =
            { screen = Home
            , touch = False
            , level = 0
            , levels = levels
            , levelMoves = []
            , levelCursor = -1
            , viewPort = { width = 0.0, height = 0.0 }
            , deaths = 0
            , moves = 0
            }
    in
    ( model, Task.perform InitializeViewPort Browser.Dom.getViewport )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        defaultSubs =
            Browser.Events.onResize UpdateViewPort
    in
    if model.screen == Game then
        Sub.batch
            [ defaultSubs
            , onKeyDown (Decode.map HandleKeyboardEvent keyDecoder)
            ]

    else
        defaultSubs


parseKey : String -> Maybe Direction
parseKey key =
    case key of
        "w" ->
            Just Up

        "s" ->
            Just Down

        "a" ->
            Just Left

        "d" ->
            Just Right

        "ArrowUp" ->
            Just Up

        "ArrowLeft" ->
            Just Left

        "ArrowRight" ->
            Just Right

        "ArrowDown" ->
            Just Down

        _ ->
            Nothing


indexOf : a -> Array a -> Int
indexOf value array =
    let
        find =
            \( i, x ) z ->
                if x == value then
                    i

                else
                    z
    in
    array
        |> Array.indexedMap (\i x -> ( i, x ))
        |> Array.foldl find -1


currentLevel : Model -> Maybe Level
currentLevel model =
    model.levels
        |> Array.fromList
        |> Array.get model.level


startLevel : Model -> Model
startLevel model =
    case currentLevel model of
        Just l ->
            let
                levelCursor =
                    l.tiles |> Array.fromList |> indexOf Start
            in
            { model | levelCursor = levelCursor, levelMoves = [] }

        Nothing ->
            { model | screen = GameOver }


tryMove : Level -> Model -> Direction -> Maybe ( Int, Bool )
tryMove level model dir =
    let
        cur =
            model.levelCursor

        ( destinationCursor, withinBounds ) =
            case dir of
                Up ->
                    let
                        next =
                            cur - level.width
                    in
                    ( next, next > 0 )

                Left ->
                    let
                        next =
                            cur - 1
                    in
                    ( next, (cur // level.width) == (next // level.width) )

                Right ->
                    let
                        next =
                            cur + 1
                    in
                    ( next, (cur // level.width) == (next // level.width) )

                Down ->
                    let
                        next =
                            cur + level.width
                    in
                    ( next, next < List.length level.tiles )
    in
    if withinBounds then
        let
            destinationTile =
                level.tiles |> Array.fromList |> Array.get destinationCursor
        in
        case destinationTile of
            Just tile ->
                case tile of
                    Floor ->
                        Just ( destinationCursor, False )

                    Death ->
                        Nothing

                    Block ->
                        Just ( cur, False )

                    Start ->
                        Just ( destinationCursor, False )

                    End ->
                        Just ( destinationCursor, True )

            Nothing ->
                Nothing

    else
        Nothing


died : Model -> Model
died model =
    { model | deaths = model.deaths + 1 }


moved : Model -> Model
moved model =
    { model | moves = model.moves + 1 }


handleDirectionInput : Direction -> Model -> Model
handleDirectionInput direction model =
    case currentLevel model of
        Just level ->
            let
                inputMoves =
                    model.levelMoves ++ [ direction ]

                output =
                    level.controller (inputMoves |> Array.fromList)
            in
            case output of
                Reset ->
                    model |> moved |> died |> startLevel

                NoOp ->
                    { model | levelMoves = inputMoves }

                Move dir ->
                    case tryMove level model dir of
                        Just ( newCursor, finished ) ->
                            if finished then
                                let
                                    advancedModel =
                                        moved { model | level = model.level + 1 }
                                in
                                case currentLevel model of
                                    Just _ ->
                                        startLevel advancedModel

                                    Nothing ->
                                        { model | screen = GameOver }

                            else if newCursor == model.levelCursor then
                                { model | levelMoves = inputMoves }

                            else
                                moved { model | levelCursor = newCursor, levelMoves = inputMoves }

                        Nothing ->
                            model |> moved |> died |> startLevel

        Nothing ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowHome ->
            ( { model | screen = Home }, Cmd.none )

        ShowAbout ->
            ( { model | screen = About }, Cmd.none )

        EnterGame touch ->
            ( startLevel
                { model
                    | screen = Game
                    , level = 0
                    , touch = model.touch || touch
                    , deaths = 0
                    , moves = 0
                }
            , Cmd.none
            )

        HandleKeyboardEvent key ->
            let
                newModel =
                    case parseKey key of
                        Just direction ->
                            handleDirectionInput direction model

                        Nothing ->
                            model
            in
            ( newModel, Cmd.none )

        HandleClickEvent direction ->
            ( handleDirectionInput direction model, Cmd.none )

        InitializeViewPort viewPort ->
            ( { model
                | viewPort =
                    { width = viewPort.viewport.width
                    , height = viewPort.viewport.height
                    }
              }
            , Cmd.none
            )

        UpdateViewPort width height ->
            ( { model | viewPort = { width = toFloat width, height = toFloat height } }, Cmd.none )


tileType : Tile -> String
tileType tile =
    case tile of
        Death ->
            "death"

        Floor ->
            "floor"

        Block ->
            "block"

        Start ->
            "start"

        End ->
            "end"


tileSize : Int -> Int -> ViewPort -> Float
tileSize width total viewPort =
    let
        height =
            total // width

        tileWidth =
            viewPort.width / toFloat width

        tileHeight =
            viewPort.height / toFloat height

        maxSize =
            min tileWidth tileHeight

        damppening =
            0.8
    in
    maxSize * damppening


plural : String -> Int -> String
plural unit num =
    if num == 1 then
        String.fromInt num ++ " " ++ unit

    else
        String.fromInt num ++ " " ++ unit ++ "s"


px : Float -> String
px num =
    String.fromFloat num ++ "px"


view : Model -> Html Msg
view model =
    case model.screen of
        Home ->
            div [ class "overlay" ]
                [ div
                    [ class "overlay-content home" ]
                    [ h1 [ class "title" ]
                        [ text "Re:Move" ]
                    , p [ class "button-set" ]
                        [ button [ class "button button-primary", onClick (EnterGame False), onTouch (EnterGame True) ] [ text "Move it" ]
                        , button [ class "button button-secondary", onClick ShowAbout ] [ text "Huh?" ]
                        ]
                    ]
                ]

        About ->
            aboutView model

        Game ->
            div [ class "game" ]
                [ case currentLevel model of
                    Just l ->
                        div [ class "level" ]
                            [ p [ class "level-title" ]
                                [ b [] [ text ("Level " ++ (model.level + 1 |> String.fromInt) ++ ": " ++ l.title) ]
                                , span [] [ text ("Resets: " ++ String.fromInt model.deaths) ]
                                , span [] [ text ("Moves: " ++ String.fromInt model.moves) ]
                                ]
                            , if model.touch then
                                touchControls

                              else
                                text ""
                            , div [ class "tiles" ]
                                (l.tiles
                                    |> List.indexedMap
                                        (\i tile ->
                                            let
                                                size =
                                                    tileSize l.width (List.length l.tiles) model.viewPort

                                                margin =
                                                    size |> (\x -> x / 25) |> clamp 1 3

                                                tilePiece =
                                                    div
                                                        [ class
                                                            ("tile tile--"
                                                                ++ tileType tile
                                                                ++ (if i == model.levelCursor then
                                                                        " tile--active"

                                                                    else
                                                                        ""
                                                                   )
                                                            )
                                                        , style "width" (px size)
                                                        , style "height" (px size)
                                                        , style "border-width" (margin |> px)
                                                        , style "margin" (margin |> px)
                                                        ]
                                                        []
                                            in
                                            if (l.width - 1) == modBy l.width i then
                                                [ tilePiece, br [] [] ]

                                            else
                                                [ tilePiece ]
                                        )
                                    |> List.concat
                                )
                            ]

                    Nothing ->
                        p [] [ text "Level not found" ]
                ]

        GameOver ->
            div [ class "overlay" ]
                [ div
                    [ class "overlay-content home" ]
                    [ h1 [ class "score" ]
                        [ text ("The game is now done. You reset " ++ plural "time" model.deaths ++ " and moved " ++ plural "time" model.moves ++ ".")
                        ]
                    , p [ class "button-set" ]
                        [ button
                            [ class "button button-primary"
                            , onClick (EnterGame False)
                            , onTouch (EnterGame True)
                            ]
                            [ text "Play again" ]
                        , button [ class "button button-secondary", onClick ShowHome ] [ text "Return home" ]
                        ]
                    ]
                ]


onTouch : msg -> Attribute msg
onTouch message =
    Html.Events.on "touchstart" (Decode.succeed message)


touchControls : Html Msg
touchControls =
    div [ class "dpad" ]
        [ button [ class "dpad-button dpad-up", onClick (HandleClickEvent Up) ] [ text "Up" ]
        , button [ class "dpad-button dpad-right", onClick (HandleClickEvent Right) ] [ text "Right" ]
        , button [ class "dpad-button dpad-left", onClick (HandleClickEvent Left) ] [ text "Left" ]
        , button [ class "dpad-button dpad-down", onClick (HandleClickEvent Down) ] [ text "Down" ]
        ]


aboutView : Model -> Html Msg
aboutView model =
    div [ class "info overlay" ]
        [ header [ onClick ShowHome ] [ text "← Back to home screen" ]
        , article []
            [ h1 [] [ text "Re:Move" ]
            , h2 []
                [ text
                    ("D-pad controls are simple. You hit Up, it takes you up. You hit Left, it takes you left. Introducing a new generation of D-pads. Each one features a deterministic (no randomness) algorithm to decide how you move next based on your present and past moves. Move with urgency and focus through all "
                        ++ (model.levels |> List.length |> String.fromInt)
                        ++ " levels with only your determination, wit, Up, Down, Left, and Right."
                    )
                ]
            , p []
                [ text "Re:Move is written by "
                , a [ href "https://jew.ski/" ] [ text "Chris Andrejewski" ]
                , text ". The source code is "
                , a [ href "https://github.com/andrejewski/remove" ] [ text "open source" ]
                , text "."
                ]
            ]
        ]


main : Program Int Model Msg
main =
    Browser.element
        { init = init, subscriptions = subscriptions, update = update, view = view }
