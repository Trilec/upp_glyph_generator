# U++ Icon Builder — Design Guide v03

Goal: a single-file U++ app that edits simple vector primitives (Rect, Circle, Line, Triangle, **Text (TOP-aligned)**, Curve) with normalized geometry, per-primitive operations, selection & editing, live BufferPainter code-generation, and JSON save/load. 

---

## 1) Project shape

* **One file:** `main.cpp`
* **Uses:** `Core`, `CtrlLib`, `Draw`, `Painter` (U++ **2025.1+**)
* **Main window:** `MainWin : TopWindow` with:

  * **Left column:** Tools, Ops, Actions, Style (incl. Text controls), Canvas
  * **Right column:** Code preview (`DocEdit`) with header (title + Copy/Save/Load buttons)
* **Canvas control:** `Canvas : Ctrl` — owns document, selection, interaction, paint, JSON, history
* **No virtuals for primitives.** A **function-pointer registry** (`PrimitiveOps`) is indexed by `PType`

---

## 2) Coordinates & snapping (the backbone)

* Work inside a **clip/inset rectangle** centered in the canvas (aspect-controlled).
* Store *all* geometry **normalized** to the inset: `[0..1]` along X and Y.
* Convert on the fly using these helpers (they match the code exactly):

```cpp
static inline int    X (const Rect& r, double nx){ return r.left + int(r.Width()*nx + 0.5); }
static inline int    Y (const Rect& r, double ny){ return r.top  + int(r.Height()*ny + 0.5); }
static inline int    R (const Rect& r, double nr){ return int(min(r.Width(), r.Height())*nr + 0.5); }
static inline double NX(const Rect& r, int px)   { return (px - r.left) / double(max(1, r.Width())); }
static inline double NY(const Rect& r, int py)   { return (py - r.top)  / double(max(1, r.Height())); }
static inline int    Snap1D(int v, int o, int s) { return o + ((v - o + s/2) / s) * s; }
```

* **Snap in pixel space**, then convert to normalized → avoids drift.
* **Aspect & inset:** `Canvas::GetInsetRect()` respects an aspect list and a `clip_scale_n` factor.

---

## 3) Data model

```cpp
enum class Tool  { Cursor, CreateShape };
enum class PType { Rect, Circle, Line, Triangle, Curve, Text };

enum class LineStyle { Solid, LongDash, ShortDash, Dotted };

struct Style : Moveable<Style> {
    Color  fill = Color(163,201,168), stroke = Color(30,53,47);
    int    strokeWidth = 2;
    bool   evenOdd = false, enableFill = true, enableStroke = true;
    String dash; // stroke dash (freeform)
    double fillOpacity = 1.0, strokeOpacity = 1.0, opacity = 1.0;

    // Outline (separate pass)
    bool   outlineEnable = false, outlineOutside = true;
    Color  outlineColor = Red();
    int    outlineWidth = 0;                 // extra outer width
    LineStyle outlineStyle = LineStyle::Solid;
    int    outlineOffsetX = 0, outlineOffsetY = 0; // fake shadow offsets
    String outlineDash; double outlineOpacity = 1.0;

    LineStyle strokeStyle = LineStyle::Solid;
};

struct TextData : Moveable<TextData> {
    String text = "Text";
    String face = "";     // empty => default
    double sizeN = 0.18;  // relative to inset height
    bool   bold = false, italic = false;
};

struct CurveData : Moveable<CurveData> {
    bool   cubic = true;  // false => quadratic
    bool   closed = false;
    Pointf a0, a1;        // anchors
    Pointf c0, c1;        // controls (c1 ignored for quadratic)
};

struct Shape : Moveable<Shape> {
    PType type = PType::Rect;
    Style style;

    // Rect
    double x=0, y=0, w=0, h=0;
    // Circle
    double cx=0, cy=0, r=0;         // r relative to min(inset w,h)
    // Line / Triangle
    Pointf p1, p2, p3;
    // Payloads
    TextData  text;
    CurveData curve;
};
```

---

## 4) Primitive protocol (ops registry)

A primitive implements this protocol:

```cpp
struct PrimitiveOps {
    void (*EmitPainter)(BufferPainter&, const Rect&, const Shape&);
    bool (*HitBody)(const Rect&, const Shape&, Point);
    int  (*HitVertex)(const Rect&, const Shape&, Point);
    void (*DrawOverlay)(Draw&, const Rect&, const Shape&);
    void (*BeginCreate)(Shape&, const Rect&, Point);
    void (*DragCreate)(Shape&, const Rect&, Point, Point, bool, int);
    void (*BeginEdit)(Shape&, const Rect&, Point, int, double&, double&);
    void (*DragEdit)(Shape&, const Rect&, Point, bool, int, bool, int, double&, double&);
    void (*EmitCode)(String&, const Shape&);
};

struct ToolSpec : Moveable<ToolSpec> { PType type; const char* label; const char* tip; };

const PrimitiveOps&     GetOps(PType);
const Vector<ToolSpec>& GetToolSpecs();
```

**Why:** tiny, non-virtual dispatch keeps `Canvas` generic; adding a primitive is adding one row in a table.

---

## 5) Canvas responsibilities

`Canvas : Ctrl` owns:

* **Model:** `Vector<Shape> shapes; int selected = -1;`
* **Interaction:** `Tool tool = Cursor; PType creation_type = Rect;`
* **Options:** `bool snap = true; bool clip = true; int grid = 8;`
* **View:** `aspect_ix` (see §11), `clip_scale_n = 0.80`, `bg_color`, `show_grid`
* **History:** `Vector<String> hist; int hist_ix = -1;` (stores JSON snapshots)
* **Text template:** `String text_template = "Text";` (used on Text creation)
* **Callbacks:** `Callback WhenSelection; Callback WhenShapesChanged;`

### Event flow

* **Create:**
  `LeftDown` (CreateShape) → new `Shape` of `creation_type`; `BeginCreate` → `DragCreate` while moving → `LeftUp` finalizes, fires `WhenShapesChanged`, pushes history.

* **Select/Edit:**
  `LeftDown` (Cursor) → vertex hit (topmost) else body hit → `BeginEdit` (captures normalized grab) → `DragEdit` for move/resize → `LeftUp` finalize + history.

* **Keyboard:**
  `Delete` to remove selection (others: see Actions row in §11).

---

## 6) Paint pipeline

1. **Background:** `w.DrawRect(GetSize(), bg_color)`
2. **Offscreen:** `ImageBuffer ib(sz); ib.SetKind(IMAGE_OPAQUE); BufferPainter p(ib, MODE_ANTIALIASED); p.Clear(bg_color);`
3. **Inset paper:** draw white `Rect` for the clip area
4. **Grid (optional):** draw vertical/horizontal 1-px lines over the inset region
5. **Clip (optional):** emit inset path → `Clip()`; keep active for shapes; `End()` to pop
6. **Shapes:** `for (s : shapes) GetOps(s.type).EmitPainter(p, inset, s);`
7. **Blit:** `w.DrawImage(0,0, Image(ib));`
8. **Overlay:** if `selected` valid, `GetOps(...).DrawOverlay(w, inset, shape)`
9. **Frame:** draw the inset border (4 thin rects) last

**Guards:** `MIN_EMIT_PX = 6` for tiny primitives; keep every `Begin()` balanced with `End()`.

---

## 7) Style application & outline pass

Three high-level passes (exactly as the code does):

* **Pass_Fill** — respects `enableFill`, `evenOdd`, and `fillOpacity * opacity`
* **Pass_Stroke** — respects `enableStroke`, `strokeOpacity * opacity`, and dash (preset or freeform)
* **Pass_Outline** — extra rim; optional pixel **Translate** by `(outlineOffsetX, outlineOffsetY)`, uses combined opacity; width = main stroke + `2 * outlineWidth` (at least 1)

**Order:**
If `outlineOutside` is true → outline, then fill, then stroke.
Else → fill, stroke, then outline.

**Dash strings:** validated; only applied when ≥2 strictly positive segments.

**Opacity clamp:** helper `Clamp01(double)` keeps `[0..1]`. `Upp::clamp` is used for UI mapping (0..100 → 0..1).

---

## 8) Primitive specifics

* **Rect**

  * Path: 4 segments + `Close()`
  * Hit: normalized rect (inflated by 4 px)
  * Handles: 4 corners
  * EmitCode: emits path, then optional style ops, then `End()`

* **Circle**

  * Path: start at **east**; two `SvgArc(Pointf(rr,rr), 0, false, true, …)` semicircles; `Close()`
  * Hit: if filled, interior or ring; else, ring-tolerance (`max(6, stroke/2 + 4)`)
  * Handles: center (move) + east (radius)
  * EmitCode: mirrors the two-arc approach

* **Line**

  * Path: `Move(a); Line(b)`
  * Hit: `IsNearSegment(...)`
  * Handles: endpoints
  * EmitCode: path → optional opacity/dash → stroke/fill (fill usually no-op)

* **Triangle**

  * Path: A→B→C→`Close()`
  * Hit: filled → `IsPointInTriangle`; outline → near any edge
  * Handles: 3 vertices (overlay draws helper squares slightly outside)
  * EmitCode: path → optional even/odd → fill/stroke

* **Text (TOP-aligned)**

  * **Semantics:** `s.y` is **TOP of the text row** (not baseline). Glyphs are placed with pen at `(X(inset,s.x), Y(inset,s.y))`. No ascent offset is added.
  * Font: built from normalized height `sizeN` → pixels via `inset.Height() * sizeN`; face, bold, italic
  * Emit: `p.Character(pen, ch, F)` per character; advance `pen.x` by `GetTextSize(String(ch,1), F).cx`
  * Hit: `TextPixelRect(...)` inflated by 4 px; handles: 4 corners
  * Create/Resize: vertical drag sets height (`sizeN`), top remains at initial `y`

* **Curve (Quadratic / Cubic; open/closed)**

  * Path: `Move(a0)` then `Quadratic(c0,a1)` **or** `Cubic(c0,c1,a1)`; optional `Close()`
  * Hit: inflated AABB of `{a0,a1,c0,c1}` (fast editor heuristic)
  * Handles: `a0`, `c0`, `[c1]`, `a1` (c1 only for cubic); overlay links anchors to controls
  * EmitCode: mirrors the quadratic/cubic choice and `Close()` when `closed`

---

## 9) JSON I/O (robust)

* **Root fields:**
  `snap, clip, grid, aspect_ix, clip_scale_n, bg_r, bg_g, bg_b, text_template, shapes`

* **Per shape (common):**
  `"type"` (int), `"style"` (map)

* **Style map keys:**
  `fill_r, fill_g, fill_b, stroke_r, stroke_g, stroke_b, stroke_w, evenOdd, dash, enableFill, enableStroke, opacity, fillOpacity, strokeOpacity, outlineEnable, outlineOutside, outline_r, outline_g, outline_b, outline_w, outlineOpacity, strokeStyle, outlineStyle, outlineDash, outlineOffsetX, outlineOffsetY`

* **Shape-specific keys:**

  * Rect: `x, y, w, h`
  * Circle: `cx, cy, r`
  * Line: `p1x, p1y, p2x, p2y`
  * Triangle: `p1x, p1y, p2x, p2y, p3x, p3y`
  * Text: `x, y, txt, face, sizeN, bold, italic`
  * Curve: `cubic, closed, a0x, a0y, a1x, a1y, c0x, c0y, c1x, c1y`

* **Decoding:** loader accepts either a `ValueMap` or an *array of `{key,value}`* pairs; selection is set to last shape or `-1` if empty; callbacks `WhenSelection/WhenShapesChanged` are fired; `Refresh()` at end.

---

## 10) Code generation (right pane)

* Header (fixed):

```cpp
void DrawIcon(Draw& w, const Rect& inset)
{
    BufferPainter p(w, MODE_ANTIALIASED);

    // shape snippets appended here…
}
```

* Each primitive appends a snippet (`EmitCode`) that mirrors runtime drawing:

  * Path first, then **conditional** style ops (opacity, dash, even-odd), then Fill/Stroke, then `End()`
  * Text is exported **TOP-aligned** to match the editor

* Small helpers (`EmitOpacityCode`, `EmitDashCode`) keep output clean.

---

## 11) UI layout (MainWin)

* **Tools row:** `Cursor` + buttons for `Rect`, `Circle`, `Line`, `Triangle`, `Text`, `Curve` (from `GetToolSpecs()`).
* **Ops row:**

  * `Snap` (Option) ↔ `canvas.snap`
  * `Clip` (Option) ↔ `canvas.clip`
  * `ClipAspect` (DropList) indices over:

    ```
    {1:1, 4:3, 3:2, 16:9, 21:9, 9:16, 2:3, 3:4}
    ```
  * `SampleRes` (DropList): {32, 64, 128, 512} → `canvas.sample_width`
  * `Grid` (Option visual flag) + `Step` (EditInt, 2..64) → `canvas.grid`
* **Actions row:** `Duplicate`, `FlipX`, `FlipY`, `LayerUp`, `LayerDn`, `Undo`, `Redo`, `Reset` (style of selection), `Clear` (all), `Delete` (selection), `BG` (enable) + `ColorPusher`, `Save` (JSON), `Load` (JSON)
* **Style panel:**

  * **Fill:** toggle + color + *Fill Opacity* (0..100 slider)
  * **Stroke:** toggle + color + *Width* (EditIntSpin 0..128) + *Type* (Solid/LongDash/ShortDash/Dotted) + *Opacity* + *Dash string*
  * **Outline:** toggle + color + *Width* + *Type* + *Opac* + *Dash* + *OffX/Y*
  * **EvenOdd** (global fill rule toggle)
* **Text subrow:** `Text`, `Codes` (comma-separated char codes), `Font` (system faces), `B`, `I`
* **Code pane (right):** title + `Copy`, plus `Save/Load` buttons in the header; `DocEdit` below

**Behavioral notes:**

* UI writes back via `PullStyleFromUI()`; selection change mirrors to UI via `PushStyleToUI()`.
* Dash *preset* DropLists write suggested strings into the corresponding dash `EditString` and re-apply.

---

## 12) Layering & transforms

* `LayerUp/Down`: swap neighbor elements; adjust `selected` accordingly
* `Duplicate`: copy shape, nudge by ~6 px in normalized units relative to inset size
* `FlipX/FlipY`: reflect around clip center `(0.5, 0.5)` in normalized space

---

## 13) Testing & guards (practical checklist)

* **Creation/Editing:** all primitives with snap ON/OFF
* **Text:** verify **TOP-aligned** creation and vertical resizing behavior
* **Style toggles:** Fill / Stroke / Outline / EvenOdd; stroke width extremes; dashes (valid/invalid)
* **Outline:** outside vs inside ordering; offsets X/Y
* **Undo/Redo:** after mixed operations; ensure history remains consistent
* **JSON:** Save → Clear → Load round-trip fidelity
* **Exported code:** compile and compare visual parity at 1×/2×/3×; small/degenerate shapes are skipped as expected
* **Clip:** ensure `Clip()` is always paired with `End()`; overlays are drawn un-clipped

---

## 14) Extending with a new primitive (recipe)

1. Add payload fields to `Shape` if needed.
2. Implement the 9-function `PrimitiveOps` set.
3. Register it in `Facets()` with a `ToolSpec` (label + tip).
4. (Optional) Add/adjust style controls if it requires new UI.

---

## 15) Configuration defaults (current behavior)

* `snap = true`, `clip = true`, `grid = 8`
* `aspect_ix = 0` → **1:1** aspect initially
* `clip_scale_n = 0.80` (inset occupies 80% of canvas)
* `bg_color = SColorFace()` unless `BG` override is enabled (then `ColorPusher` value)
* `sample_width = 128` (for future exports)
* `text_template = "Text"`
* **Thresholds:** `MIN_EMIT_PX = 6`, `HANDLE_PX = 9`

---

## 16) Known work items (tracked)

* **Pixel-centering of exported code** (paths/pen placement parity at 1×)
* **Export edge cases:** tiny shapes, outline+dash combinations under clipping
* **QoL:** keyboard nudges, alignment tools, symbol bank, PNG/ICO exporters

---

## 17) Implementation notes & conventions

* Prefer **value semantics** (`Moveable<T>`, `Vector<T>`) and scope-bound lifetimes.
* GUI operations occur on the **main thread**; painter `Begin/End` are balanced; any `Clip()` must be popped.
* **Clamping:** Use `Clamp01(double)` for opacity; UI sliders map via `Upp::clamp(int_to_0_1)`.
* **Dash parsing:** freeform string → parsed into positive segments (0/negatives ignored); applied only if ≥2 segments.
* **Top-aligned text:** both runtime and emitted code intentionally render glyphs from the **top** (no ascent offset); the bounding box uses `max(pxh, textSize.cy)` for safety.
* **Circle rendering:** two semicircular `SvgArc` calls for robust, true circles.

---

### Release checklist (for maintainers)

* Verify `.upp` uses: `Core`, `CtrlLib`, `Draw`, `Painter`
* Build with U++ **2025.1+**; run smoke tests from §13
* README screenshot updated; MIT year/name correct
* Tag version and list the known work items in the GitHub issue tracker

---

**End of Guide**
