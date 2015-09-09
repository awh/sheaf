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

type alias Model = {
  nodes : List Node,
  edges : List Edge,
  editing : Maybe Node
}

-- Actions

type Action = CanvasClick Int Int | NodeClick Node

actionMailbox : Signal.Mailbox (Maybe Action)
actionMailbox = Signal.mailbox Nothing

-- View

isEditable : Model -> Node -> Bool
isEditable model node =
  case model.editing of
    Nothing -> False
    Just editableNode -> editableNode == node

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

update : Maybe Action -> Model -> Model
update action old =
  case action of
    Just (CanvasClick x y) ->
      case old.editing of
        Just _ -> {
          nodes = old.nodes,
          edges = old.edges,
          editing = Nothing
        }
        Nothing ->
          let
            node = TextNode x y "Some text"
          in {
            nodes = node :: old.nodes,
            edges = old.edges,
            editing = Just node
          }
    Just (NodeClick node) -> {
      nodes = old.nodes,
      edges = old.edges,
      editing = Just node
    }

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
