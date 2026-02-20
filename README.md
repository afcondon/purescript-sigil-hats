# sigil-hats

Bridge from [Sigil](../purescript-sigil) layout trees to [HATS](../purescript-hylograph-selection) `Tree` values for declarative composition in Hylograph visualizations.

## What it does

Sigil renders PureScript type signatures as positioned SVG primitives (`LayoutNode`). HATS is Hylograph's declarative rendering layer. This package converts between them so that type-signature graphics can be composed into larger HATS scenes (force layouts, dashboards, explorers) without manual SVG construction.

## Anti-drift strategy

Both Sigil's direct-to-DOM path and this HATS bridge share a single attribute-mapping function, `Sigil.Svg.Attrs.toSvgPrimitive`. Any change to how Sigil encodes colors, fonts, or layout flows through both paths identically, preventing visual drift between standalone Sigil renders and HATS-embedded renders.

## API

### Core

```purescript
toTree :: LayoutNode -> Tree
```

Convert any Sigil `LayoutNode` tree to a HATS `Tree`.

### Convenience renderers

Each takes a structured record and returns a ready-to-embed `Tree` (wrapped in an `<svg>` element with correct `viewBox`):

| Function | Input | Use case |
|---|---|---|
| `renderClassDeclSvg` | Class name, params, superclasses, methods | Full type-class card |
| `renderSignatureSvg` | Name, raw sig string, parsed AST | Value / type-synonym signature |
| `renderAdtSvg` | Name, params, constructors | Algebraic data type |
| `renderSigletSvg` | Elided AST, max dimensions | Miniature inline signature |

`renderSigletSvg` returns `Maybe Tree` &mdash; `Nothing` when the type cannot fit within the requested bounds.

## Quick example

Render the `map` signature as a siglet and embed it in a HATS scene:

```purescript
import Sigil.Parse (parseToRenderType, elideAST)
import Sigil.Hats (renderSigletSvg)
import Hylograph.HATS (elem)
import Hylograph.HATS.Friendly as F
import Hylograph.Internal.Selection.Types (ElementType(..))

mapSiglet :: Maybe Tree
mapSiglet = do
  ast <- parseToRenderType "(a -> b) -> f a -> f b"
  renderSigletSvg { ast: elideAST ast, maxWidth: 140.0, maxHeight: 50.0 }

-- Embed in a larger tree:
card :: Tree
card = elem Group [ F.transform "translate(100,200)" ]
  [ elem Rect [ F.width 160.0, F.height 80.0, F.fill "#fff" ] []
  , case mapSiglet of
      Just t  -> elem Group [ F.transform "translate(10,20)" ] [ t ]
      Nothing -> elem Group [] []
  ]
```

## Demo

24 PureScript type classes across 8 hierarchies in a zoomable, draggable force-directed grid. Each node is a full class declaration rendered via this bridge.

**[Live demo](https://afcondon.github.io/purescript-sigil-hats/)**

https://github.com/user-attachments/assets/FunctorMonad.mov

To run locally:

```bash
cd demo
spago bundle
open public/index.html
```
