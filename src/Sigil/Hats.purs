-- | Bridge from Sigil LayoutNode trees to HATS Tree values.
-- |
-- | Uses `toSvgPrimitive` from `Sigil.Svg.Attrs` so that the HATS path
-- | produces identical SVG attributes to the DOM FFI emit path.
module Sigil.Hats
  ( toTree
  , renderClassDeclSvg
  , renderSignatureSvg
  , renderAdtSvg
  , renderSigletSvg
  ) where

import Prelude

import Data.Maybe (Maybe(..))

import Hylograph.HATS (Tree, elem, staticStr, staticNum)
import Hylograph.Internal.Element.Types (ElementType(..))

import Sigil.Types (RenderType, SuperclassInfo)
import Sigil.Svg.Types (LayoutNode, Dimensions)
import Sigil.Svg.Attrs (toSvgPrimitive)
import Sigil.Svg.Layout.ClassDef (layoutClassDef)
import Sigil.Svg.Layout.Signature (layoutSignature)
import Sigil.Svg.Layout.ADT (layoutADT)
import Sigil.Svg.Layout.Siglet (layoutSiglet)

-- | Convert a LayoutNode to a HATS Tree, using the shared attribute mapping.
toTree :: LayoutNode -> Tree
toTree node =
  let prim = toSvgPrimitive node
      elemType = tagToElementType prim.tag
      svgAttrs = map (\a -> staticStr a.key a.value) prim.attrs
      attrs = case prim.textContent of
        Just t  -> svgAttrs <> [ staticStr "textContent" t ]
        Nothing -> svgAttrs
      children = map toTree prim.children
  in elem elemType attrs children

-- | Map SVG tag strings to HATS ElementType constructors.
tagToElementType :: String -> ElementType
tagToElementType = case _ of
  "text"   -> Text
  "rect"   -> Rect
  "line"   -> Line
  "circle" -> Circle
  "g"      -> Group
  "svg"    -> SVG
  _        -> Group  -- safe fallback

-- | Wrap a layout tree in an SVG element with viewBox dimensions.
wrapSvg :: LayoutNode -> Dimensions -> Tree
wrapSvg layout dims =
  let w = show dims.width
      h = show dims.height
  in elem SVG
    [ staticNum "width" dims.width
    , staticNum "height" dims.height
    , staticStr "viewBox" ("0 0 " <> w <> " " <> h)
    , staticStr "style" "overflow: visible;"
    ]
    [ toTree layout ]

-- | Render a type class definition as a HATS Tree.
renderClassDeclSvg
  :: { name :: String
     , typeParams :: Array String
     , superclasses :: Array SuperclassInfo
     , methods :: Array { name :: String, ast :: Maybe RenderType }
     }
  -> Tree
renderClassDeclSvg opts =
  let { layout, dimensions } = layoutClassDef opts
  in wrapSvg layout dimensions

-- | Render a value/type-synonym signature as a HATS Tree.
renderSignatureSvg
  :: { name :: String
     , sig :: String
     , ast :: RenderType
     , typeParams :: Array String
     , className :: Maybe String
     }
  -> Tree
renderSignatureSvg opts =
  let { layout, dimensions } = layoutSignature opts
  in wrapSvg layout dimensions

-- | Render an ADT (data type with constructor branches) as a HATS Tree.
renderAdtSvg
  :: { name :: String
     , typeParams :: Array String
     , constructors :: Array { name :: String, args :: Array RenderType }
     , keyword :: Maybe String
     }
  -> Tree
renderAdtSvg opts =
  let { layout, dimensions } = layoutADT opts
  in wrapSvg layout dimensions

-- | Render a siglet (elided miniature type signature) as a HATS Tree.
-- | Returns Nothing if the type can't fit in the given dimensions.
renderSigletSvg
  :: { ast :: RenderType, maxWidth :: Number, maxHeight :: Number }
  -> Maybe Tree
renderSigletSvg opts = case layoutSiglet opts of
  Nothing -> Nothing
  Just { layout, scaledWidth, scaledHeight } ->
    let w = show scaledWidth
        h = show scaledHeight
    in Just $ elem SVG
      [ staticNum "width" scaledWidth
      , staticNum "height" scaledHeight
      , staticStr "viewBox" ("0 0 " <> w <> " " <> h)
      , staticStr "style" "overflow: visible;"
      ]
      [ toTree layout ]
