/*
================================================================================
 primitives.h: Enums, structs, helpers, and declarations for primitive operations.
================================================================================
*/

#ifndef PRIMITIVES_H
#define PRIMITIVES_H

#include <CtrlLib/CtrlLib.h>
#include <Draw/Draw.h>
#include <Painter/Painter.h>
#include <plugin/jpg/jpg.h>
#include <plugin/png/png.h>

using namespace Upp;

// Enum classes for type-safety and scoping.
enum class LineStyle { Solid, LongDash, ShortDash, Dotted };
enum class Tool { Cursor, CreateShape };
enum class PType { Rect, Circle, Line, Triangle, Curve, Text };

// Converts LineStyle to/from int for UI (e.g., DropList).
// Params: s/i - style/index. Returns: converted value. Robust to invalid input.
int ToI(LineStyle s);
LineStyle FromI(int i);

// Guesses LineStyle from dash string (e.g., "12,4" -> LongDash).
// Params: s - dash string. Returns: guessed style (Solid if unknown).
LineStyle GuessStyleFromString(const String& s);

// Style struct for shape rendering (moveable for Vector efficiency).
struct Style : Moveable<Style> {
    Color fill = Color(120, 120, 120);
    Color stroke = Color(60, 60, 60);
    int strokeWidth = 2;
    bool evenOdd = false;
    String dash;  // e.g., "12,4"
    bool enableFill = true;
    bool enableStroke = true;

    double fillOpacity = 1.0;    // 0..1
    double strokeOpacity = 1.0;  // 0..1
    double opacity = 1.0;        // global 0..1

    // Outline properties.
    bool outlineEnable = false;
    bool outlineOutside = true;
    Color outlineColor = Red();
    int outlineWidth = 0;
    LineStyle outlineStyle = LineStyle::Solid;
    int outlineOffsetX = 0;
    int outlineOffsetY = 0;
    String outlineDash;
    double outlineOpacity = 1.0;

    LineStyle strokeStyle = LineStyle::Solid;
};

// Style overrides for code emission.
struct StyleOverrides : Moveable<StyleOverrides> {
    Color fillColor = Color(130, 130, 130);
    Color strokeColor = Color(70, 70, 70);
    Color outlineColor = Color(40, 40, 40);

    bool useFill = false;
    bool useStroke = false;
    bool useOutline = false;

    String fillName = "FillCol";
    String strokeName = "StrokeCol";
    String outlineName = "OutlineCol";
};

// Text payload.
struct TextData {
    String text = "Text";
    String face = "";
    double sizeN = 0.18;  // normalized to inset height
    bool bold = false;
    bool italic = false;
};

// Curve payload.
struct CurveData {
    bool cubic = true;   // false = quadratic
    bool closed = false;
    Pointf a0, a1;       // anchors
    Pointf c0, c1;       // controls (c1 ignored if !cubic)
};

// Shape struct (moveable).
struct Shape : Moveable<Shape> {
    PType type = PType::Rect;
    Style style;

    // Rect-specific.
    double x = 0, y = 0, w = 0, h = 0;
    double rxN = 0.0;  // normalized corner radius X
    double ryN = 0.0;  // normalized corner radius Y

    // Circle-specific.
    double cx = 0, cy = 0, r = 0;

    // Line/Triangle/Curve-specific.
    Pointf p1, p2, p3;

    // Payloads.
    TextData text;
    CurveData curve;
};

// Normalized <-> pixel helpers (global, inline for perf).
inline int X(const Rect& r, double nx) { return r.left + int(r.Width() * nx + 0.5); }
inline int Y(const Rect& r, double ny) { return r.top + int(r.Height() * ny + 0.5); }
inline int R(const Rect& r, double nr) { return int(min(r.Width(), r.Height()) * nr + 0.5); }
inline double NX(const Rect& r, int px) { return (px - r.left) / double(max(1, r.Width())); }
inline double NY(const Rect& r, int py) { return (py - r.top) / double(max(1, r.Height())); }
inline int Snap1D(int v, int origin, int step) { return origin + ((v - origin + step / 2) / step) * step; }
inline double Clamp01(double v) { return Upp::clamp(v, 0.0, 1.0); }

// Formats double to 4 decimals (thread-local buffer for safety).
inline const char* Decimal4(double v) {
    static thread_local String s;
    s = FormatDouble(v, 4);
    return ~s;
}

// Hit-testing helpers.
inline bool IsNearSegment(Point p, Point a, Point b, int tol) {
    if (a == b) return abs(p.x - a.x) <= tol && abs(p.y - a.y) <= tol;
    double vx = b.x - a.x, vy = b.y - a.y, wx = p.x - a.x, wy = p.y - a.y;
    double vv = vx * vx + vy * vy;
    if (vv <= 1e-9) return false;
    double t = (wx * vx + wy * vy) / vv;
    t = Upp::clamp(t, 0.0, 1.0);
    double qx = a.x + t * vx, qy = a.y + t * vy, dx = p.x - qx, dy = p.y - qy;
    return dx * dx + dy * dy <= double(tol * tol);  // dist^2 avoids sqrt
}
inline bool IsPointInTriangle(Point p, Point a, Point b, Point c) {
    auto s = [&](Point p1, Point p2, Point p3) {
        return (p1.x - p3.x) * (p2.y - p3.y) - (p2.x - p3.x) * (p1.y - p3.y);
    };
    bool b1 = s(p, a, b) < 0, b2 = s(p, b, c) < 0, b3 = s(p, c, a) < 0;
    return (b1 == b2) && (b2 == b3);
}

// Dash helper.
inline String DashFrom(LineStyle ls, const String& custom) {
    if (!custom.IsEmpty()) return custom;
    switch (ls) {
        case LineStyle::LongDash: return "12,4";
        case LineStyle::ShortDash: return "8,4";
        case LineStyle::Dotted: return "2,4";
        default: return String();
    }
}

// Applies style to Painter (sanitizes dash/opacity).
// Params: p - Painter; st - style to apply.
inline void ApplyStyle(BufferPainter& p, const Style& st) {
    if (st.opacity < 1.0) p.Opacity(Clamp01(st.opacity));
    if (!st.dash.IsEmpty()) {
        Vector<double> seg;
        const char* s = ~st.dash;
        while (*s) {
            while (*s == ' ' || *s == '\t' || *s == ',') s++;
            char* end = nullptr;
            double v = strtod(s, &end);
            if (end == s) break;
            if (v > 0) seg.Add(v);
            s = end;
        }
        if (seg.GetCount() >= 2) {
            String norm;
            for (int i = 0; i < seg.GetCount(); ++i) {
                if (i) norm << ',';
                norm << AsString(seg[i]);
            }
            p.Dash(norm, 0.0);
        }
    }
}

// Templates for common Painter passes (reused across primitives).
template <class BuildPath>
inline void Pass_Outline(BufferPainter& p, BuildPath build, const Style& st) {
    if (!st.outlineEnable || st.outlineWidth <= 0) return;
    p.Begin();
    if (st.outlineOffsetX || st.outlineOffsetY) p.Translate(st.outlineOffsetX, st.outlineOffsetY);
    build();
    double o = Clamp01(st.outlineOpacity) * Clamp01(st.opacity);
    if (o < 1.0) p.Opacity(o);
    String d = DashFrom(st.outlineStyle, st.outlineDash);
    if (!d.IsEmpty()) p.Dash(d, 0.0);
    int W = (st.enableStroke ? st.strokeWidth : 0) + max(1, 2 * st.outlineWidth);
    p.Stroke(W, st.outlineColor);
    p.End();
}

template <class BuildPath>
inline void Pass_Fill(BufferPainter& p, BuildPath build, const Style& st) {
    if (!st.enableFill) return;
    p.Begin();
    build();
    if (st.evenOdd) p.EvenOdd(true);
    double o = Clamp01(st.fillOpacity) * Clamp01(st.opacity);
    if (o < 1.0) p.Opacity(o);
    p.Fill(st.fill);
    p.End();
}

template <class BuildPath>
inline void Pass_Stroke(BufferPainter& p, BuildPath build, const Style& st) {
    if (!st.enableStroke) return;
    p.Begin();
    build();
    double o = Clamp01(st.strokeOpacity) * Clamp01(st.opacity);
    if (o < 1.0) p.Opacity(o);
    String d = DashFrom(st.strokeStyle, st.dash);
    if (!d.IsEmpty()) p.Dash(d, 0.0);
    p.Stroke(st.strokeWidth, st.stroke);
    p.End();
}

// Common code emission helpers.
inline void EmitOpacityCode(String& out, double o) {
    o = Clamp01(o);
    if (o < 1.0) out << Format("    p.Opacity(%.3f);\n", o);
}

inline void EmitDashCode(String& out, LineStyle ls, const String& custom) {
    String d = DashFrom(ls, custom);
    if (!d.IsEmpty()) out << Format("    p.Dash(\"%s\", 0.0);\n", ~d);
}

// Ops struct for per-primitive behavior.
struct PrimitiveOps {
    void (*EmitPainter)(BufferPainter&, const Rect&, const Shape&);  // Renders shape to Painter.
    bool (*HitBody)(const Rect&, const Shape&, Point);               // Checks body hit.
    int (*HitVertex)(const Rect&, const Shape&, Point);              // Returns vertex index if hit, -1 else.
    void (*DrawOverlay)(Draw&, const Rect&, const Shape&);           // Draws selection overlay.
    void (*BeginCreate)(Shape&, const Rect&, Point);                 // Starts creation.
    void (*DragCreate)(Shape&, const Rect&, Point, Point, bool, int);// Drags during creation.
    void (*BeginEdit)(Shape&, const Rect&, Point, int, double&, double&);  // Starts edit.
    void (*DragEdit)(Shape&, const Rect&, Point, bool, int, bool, int, double&, double&);  // Drags edit.
    void (*EmitCode)(String&, const Shape&, const StyleOverrides&);  // Emits C++ code.
};

// Tool spec for UI.
struct ToolSpec : Moveable<ToolSpec> {
    PType type;
    const char* label;
    const char* tip;
};

// Registry access.
const PrimitiveOps& GetOps(PType t);                // Gets ops for type, fallback to Rect.
const Vector<ToolSpec>& GetToolSpecs();             // Gets tool specs for UI.

static constexpr int MIN_EMIT_PX = 6;
static constexpr int HANDLE_PX = 9;

#endif // PRIMITIVES_H