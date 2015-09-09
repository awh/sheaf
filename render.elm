import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing(map)
import Mouse
import Json.Decode

type Node = TextNode Int Int String
type Edge = Link Node Node

type Action = CanvasClick Int Int | NodeClick Node

type alias Model = {
  nodes : List Node,
  edges : List Edge,
  contentEditable : Maybe Node,
  selected : Maybe Node
}

isEditable : Model -> Node -> Bool
isEditable model node =
  case model.contentEditable of
    Nothing -> False
    Just editableNode -> editableNode == node

mailbox : Signal.Mailbox (Maybe a)
mailbox = Signal.mailbox Nothing

actionMailbox : Signal.Mailbox (Maybe Action)
actionMailbox = Signal.mailbox Nothing

options = { stopPropagation = True, preventDefault = False }

renderNode : Model -> Node -> Html
renderNode model node =
  case node of
    TextNode x y s ->
      if isEditable model node then
        textarea [
          onWithOptions "click" options Json.Decode.value (\_ -> Signal.message mailbox.address Nothing),
          style [
            ("position", "absolute"),
            ("left", (toString x) ++ "px"),
            ("top", (toString y) ++ "px")
          ]
        ] [
          text s
        ]
      else
        div [
          onWithOptions "click" options Json.Decode.value (\_ -> Signal.message actionMailbox.address (Just (NodeClick node))),
          style [
            ("position", "absolute"),
            ("left", (toString x) ++ "px"),
            ("top", (toString y) ++ "px")
          ]
        ] [
          text s
        ]

transform : Model -> Html
transform model = 
  div [] (map (renderNode model) model.nodes)

update : Maybe Action -> Model -> Model
update action old =
  case action of
    Just (CanvasClick x y) ->
      let
        node = TextNode x y "Enter text..."
      in {
        nodes = node :: old.nodes,
        edges = old.edges,
        contentEditable = Just node,
        selected = Nothing }
    Just (NodeClick node) -> {
      nodes = old.nodes,
      edges = old.edges,
      contentEditable = Just node,
      selected = Nothing }

--

fooNode = TextNode 100 100 "Foo"
barNode = TextNode 200 100 "Bar"

initialModel = {
  nodes = [ fooNode, barNode ],
  edges = [ Link fooNode barNode ],
  contentEditable = Just fooNode,
  selected = Nothing }

clickPosition2CanvasClick (x, y) = Just (CanvasClick x y)

clickPositionSignal : Signal (Maybe Action)
clickPositionSignal = Signal.map clickPosition2CanvasClick (Signal.sampleOn Mouse.clicks Mouse.position)

signal = Signal.merge actionMailbox.signal clickPositionSignal

main : Signal Html
main = Signal.map transform (Signal.foldp update initialModel signal)
