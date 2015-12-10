import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing(map)
import Mouse
import Json.Decode

-- Helpers

deadLetterBox : Signal.Mailbox (Maybe a)
deadLetterBox = Signal.mailbox Nothing

preventPropagation : String -> Attribute
preventPropagation eventName =
  let
    options = { stopPropagation = True, preventDefault = False }
  in
    onWithOptions eventName options Json.Decode.value (\_ -> Signal.message deadLetterBox.address Nothing)

-- Model

type Node = TextNode Int Int String
type Edge = Link Node Node

type EditNode = EditNode Node String

type alias Model = {
  nodes : List Node,
  edges : List Edge,
  editing : Maybe EditNode
  }

-- Actions

type Action = CanvasClick Int Int | NodeClick Node | UpdateNodeText Node String

actionMailbox : Signal.Mailbox (Maybe Action)
actionMailbox = Signal.mailbox Nothing

onInput : Signal.Address (Maybe Action) -> Node -> Attribute
onInput address node =
    on "input" targetValue (\str -> Signal.message address (Just (UpdateNodeText node str)))

-- View

isEditable : Model -> Node -> Bool
isEditable model node =
  case model.editing of
    Nothing -> False
    Just (EditNode editableNode _) -> editableNode == node

options = { stopPropagation = True, preventDefault = False }

nodeStyle : Node -> Attribute
nodeStyle (TextNode x y s) =
  style [
    ("position", "absolute"),
    ("left", (toString x) ++ "px"),
    ("top", (toString y) ++ "px")
  ]

renderNode : Model -> Node -> Html
renderNode model node =
  case node of
    TextNode x y s ->
      if isEditable model node then
        textarea [
          placeholder "Enter text...",
          preventPropagation "click",
          onInput actionMailbox.address node,
          nodeStyle node
        ] [
          text s
        ]
      else
        div [
          onWithOptions "click" options Json.Decode.value (\_ -> Signal.message actionMailbox.address (Just (NodeClick node))),
          nodeStyle node
        ] [
          text s
        ]

transform : Model -> Html
transform model = 
  div [] (map (renderNode model) model.nodes)

-- Update
replaceNodeText : Node -> String -> (List Node) -> (List Node)
replaceNodeText node text nodes =
  case node of
    TextNode x y _ ->
      let
        newNode = TextNode x y text
        filteredNodes = List.filter (\x -> x /= node) nodes
      in
        newNode :: filteredNodes

update : Maybe Action -> Model -> Model
update action old =
  case action of
    Just (CanvasClick x y) ->
      case old.editing of
        Just (EditNode node s) ->
          if s == "" then { old | editing = Nothing }
          else { old |
            nodes = replaceNodeText node s old.nodes,
            editing = Nothing
          }
        Nothing ->
          let
            node = TextNode x y ""
          in { old|
            nodes = node :: old.nodes,
            editing = Just (EditNode node "")
          }
    Just (NodeClick node) ->
      case node of
        TextNode x y s -> { old | editing = Just (EditNode node s) }
    Just (UpdateNodeText node text) -> { old | editing = Just (EditNode node text) }
    Nothing -> old

-- Main

initialModel = {
  nodes = [ ],
  edges = [ ],
  editing = Nothing
  }

canvasClickSignal : Signal (Maybe Action)
canvasClickSignal =
  Signal.map
    (\(x, y) -> Just (CanvasClick x y))
    (Signal.sampleOn Mouse.clicks Mouse.position)

main : Signal Html
main =
  let
    signal = Signal.merge actionMailbox.signal canvasClickSignal
  in
    Signal.map transform (Signal.foldp update initialModel signal)
