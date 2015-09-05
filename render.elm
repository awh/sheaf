import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing(map)
import Mouse

type Node = TextNode Int Int String
type Edge = Link Node Node

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

renderNode : Model -> Node -> Html
renderNode model node =
  case node of
    TextNode x y s ->
      div [
        style [
          ("position", "absolute"),
          ("left", (toString x) ++ "px"),
          ("top", (toString y) ++ "px")
        ],
        contenteditable (isEditable model node)
      ] [
        text s
      ]

transform : Model -> Html
transform model = 
  div [] (map (renderNode model) model.nodes)

update : (Int, Int) -> Model -> Model
update (x, y) old =
  let
    node = TextNode x y "Wheee!"
  in {
    nodes = node :: old.nodes,
    edges = old.edges,
    contentEditable = old.contentEditable,
    selected = old.selected }

--

fooNode = TextNode 100 100 "Foo"
barNode = TextNode 200 100 "Bar"

initialModel = {
  nodes = [ fooNode, barNode ],
  edges = [ Link fooNode barNode ],
  contentEditable = Just fooNode,
  selected = Nothing }

clickPositionSignal : Signal (Int, Int)
clickPositionSignal = Signal.sampleOn Mouse.clicks Mouse.position

main : Signal Html
main = Signal.map transform (Signal.foldp update initialModel clickPositionSignal)
