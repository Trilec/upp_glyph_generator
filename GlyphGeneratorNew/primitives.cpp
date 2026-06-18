/*
================================================================================
 primitives.cpp: Implementations for per-primitive operations and registry.
================================================================================
*/

#include "primitives.h"

// Converts LineStyle to int for UI (e.g., DropList index).
// Params: s - style. Returns: index (0 for Solid).
int ToI(LineStyle s) {
    switch(s) {
        case LineStyle::LongDash:  return 1;
        case LineStyle::ShortDash: return 2;
        case LineStyle::Dotted:    return 3;
        default:                   return 0; // Solid
    }
}

// Converts int to LineStyle for UI.
// Params: i - index. Returns: style (Solid if invalid).
LineStyle FromI(int i) {
    switch(i) {
        case 1:  return LineStyle::LongDash;
        case 2:  return LineStyle::ShortDash;
        case 3:  return LineStyle::Dotted;
        default: return LineStyle::Solid;
    }
}

// Guesses LineStyle from dash string.
// Params: s - dash string. Returns: guessed style (Solid if unknown).
LineStyle GuessStyleFromString(const String& s) {
    if(s.IsEmpty()) return LineStyle::Solid;
    if(s == "12,4") return LineStyle::LongDash;
    if(s == "8,4")  return LineStyle::ShortDash;
    if(s == "2,4")  return LineStyle::Dotted;
    return LineStyle::Solid; // treat unknown as custom-solid (dash string still applied)
}

// Rect_EmitPainter - Builds and renders rect path (sharp or rounded) with style passes.
void Rect_EmitPainter(BufferPainter& p, const Rect& inset, const Shape& s) {
    Rect r = RectC(X(inset, s.x), Y(inset, s.y),
                   X(inset, s.x + s.w) - X(inset, s.x),
                   Y(inset, s.y + s.h) - Y(inset, s.y));
    r.Normalize();
    if(r.Width() < MIN_EMIT_PX || r.Height() < MIN_EMIT_PX) return;

    // Convert normalized radii to px and clamp to half-edges
    int rx = min(R(inset, s.rxN), r.Width()  / 2);
    int ry = min(R(inset, s.ryN), r.Height() / 2);

    const Style& st = s.style;

    auto Path = [&] {
        if(rx > 0 || ry > 0) {
            // Rounded rect
            p.RoundedRectangle(r.left, r.top, r.Width(), r.Height(), rx, ry);
        } else {
            // Sharp rect
            p.Move(Pointf(r.left, r.top));
            p.Line(Pointf(r.right, r.top));
            p.Line(Pointf(r.right, r.bottom));
            p.Line(Pointf(r.left, r.bottom));
            p.Close();
        }
    };

    if(st.outlineEnable && st.outlineOutside) Pass_Outline(p, [&]{ p.Begin(); Path(); p.End(); }, st);
    Pass_Fill  (p, [&]{ p.Begin(); Path(); p.End(); }, st);
    Pass_Stroke(p, [&]{ p.Begin(); Path(); p.End(); }, st);
    if(st.outlineEnable && !st.outlineOutside) Pass_Outline(p, [&]{ p.Begin(); Path(); p.End(); }, st);
}

// Rect_HitBody - Checks if point m hits rect body (inflated for tolerance).
bool Rect_HitBody(const Rect& inset, const Shape& s, Point m) {
    Rect r = RectC(X(inset, s.x), Y(inset, s.y),
                   X(inset, s.x + s.w) - X(inset, s.x),
                   Y(inset, s.y + s.h) - Y(inset, s.y));
    r.Normalize();
    return r.Inflated(4).Contains(m);
}

// Rect_HitVertex - Returns corner index if m near vertex, -1 else.
int Rect_HitVertex(const Rect& inset, const Shape& s, Point m) {
    Point tl(X(inset, s.x), Y(inset, s.y));
    Point tr(X(inset, s.x + s.w), Y(inset, s.y));
    Point br(X(inset, s.x + s.w), Y(inset, s.y + s.h));
    Point bl(X(inset, s.x), Y(inset, s.y + s.h));
    auto Near = [&](Point a){ return abs(a.x - m.x) <= HANDLE_PX && abs(a.y - m.y) <= HANDLE_PX; };
    if(Near(tl)) return 0;
    if(Near(tr)) return 1;
    if(Near(br)) return 2;
    if(Near(bl)) return 3;
    return -1;
}

// Rect_DrawOverlay - Draws selection box and handles on w.
void Rect_DrawOverlay(Draw& w, const Rect& inset, const Shape& s) {
    Color sel = SColorMark();
    Rect r(Point(X(inset, s.x), Y(inset, s.y)),
           Point(X(inset, s.x + s.w), Y(inset, s.y + s.h)));
    r.Normalize();

    // box
    w.DrawRect(r.left, r.top, r.Width(), 1, sel);
    w.DrawRect(r.left, r.bottom, r.Width() + 1, 1, sel);
    w.DrawRect(r.left, r.top, 1, r.Height(), sel);
    w.DrawRect(r.right, r.top, 1, r.Height(), sel);

    // handles
    auto H = [&](Point p){ w.DrawRect(p.x - 3, p.y - 3, 7, 7, sel); };
    H(r.TopLeft()); H(r.TopRight()); H(r.BottomLeft()); H(r.BottomRight());
}

// Rect_BeginCreate - Initializes rect creation at start point.
void Rect_BeginCreate(Shape& s, const Rect& inset, Point start) {
    s.type = PType::Rect;
    s.x = NX(inset, start.x);
    s.y = NY(inset, start.y);
    s.w = s.h = 0;
}

// Rect_DragCreate - Updates rect size during drag.
void Rect_DragCreate(Shape& s, const Rect& inset, Point, Point cur, bool snap, int grid) {
    if(snap){ cur.x = Snap1D(cur.x, inset.left, grid); cur.y = Snap1D(cur.y, inset.top, grid); }
    s.w = NX(inset, cur.x) - s.x;
    s.h = NY(inset, cur.y) - s.y;
}

// Rect_BeginEdit - Stores grab position normalized.
void Rect_BeginEdit(Shape& s, const Rect& inset, Point grab, int, double& gx, double& gy) {
    gx = NX(inset, grab.x); gy = NY(inset, grab.y);
}

// Rect_DragEdit - Updates rect during drag (move or resize by vertex hv).
void Rect_DragEdit(Shape& s, const Rect& inset, Point cur, bool snap, int grid, bool moving, int hv, double& gx, double& gy) {
    if(snap){ cur.x = Snap1D(cur.x, inset.left, grid); cur.y = Snap1D(cur.y, inset.top, grid); }
    double nx = NX(inset, cur.x), ny = NY(inset, cur.y);

    if(moving) { s.x += nx - gx; s.y += ny - gy; gx = nx; gy = ny; return; }

    switch(hv) {
        case 0: s.w += s.x - nx; s.h += s.y - ny; s.x = nx; s.y = ny; break;             // TL
        case 1: s.w =  nx - s.x; s.h += s.y - ny;                s.y = ny; break;        // TR
        case 2: s.w =  nx - s.x; s.h =  ny - s.y;                            break;      // BR
        case 3: s.h =  ny - s.y; s.w += s.x - nx; s.x = nx;                 break;       // BL
        default: break;
    }
}

// Rect_EmitCode - Emits C++ code for rect, reserving string for efficiency.
void Rect_EmitCode(String& out, const Shape& s, const StyleOverrides& ov) {
    const Style& st = s.style;
    out.Reserve(out.GetLength() + 512);  // Pre-reserve for efficiency
    out << "    // Rect\n";
    out << "    p.Begin();\n";
    out << "    p.Move(Point("  << EX(s.x)         << ", " << EY(s.y)         << "));\n";
    out << "    p.Line(Point("  << EX(s.x + s.w)   << ", " << EY(s.y)         << "));\n";
    out << "    p.Line(Point("  << EX(s.x + s.w)   << ", " << EY(s.y + s.h)   << "));\n";
    out << "    p.Line(Point("  << EX(s.x)         << ", " << EY(s.y + s.h)   << "));\n";
    out << "    p.Close();\n";
    EmitOpacityCode(out, st.opacity);
    if(st.evenOdd) out << "    p.EvenOdd(true);\n";
    EmitDashCode(out, st.strokeStyle, st.dash);

    if(st.enableFill){
        if(ov.useFill) out << Format("    p.Fill(%s);  //<- Override\n", ~ov.fillName);
        else out << Format("    p.Fill(Color(%d,%d,%d));\n", st.fill.GetR(), st.fill.GetG(), st.fill.GetB());
    }
    if(st.enableStroke){
        if(ov.useStroke) out << Format("    p.Stroke(%d, %s);  //<- Override\n", st.strokeWidth, ~ov.strokeName);
        else out << Format("    p.Stroke(%d, Color(%d,%d,%d));\n", st.strokeWidth, st.stroke.GetR(), st.stroke.GetG(), st.stroke.GetB());
    }
    out << "    p.End();\n\n";

    if(st.outlineEnable && st.outlineWidth > 0){
        out << "    // Rect outline\n";
        out << "    p.Begin();\n";
        out << "    p.Move(Point("  << EX(s.x)         << ", " << EY(s.y)         << "));\n";
        out << "    p.Line(Point("  << EX(s.x + s.w)   << ", " << EY(s.y)         << "));\n";
        out << "    p.Line(Point("  << EX(s.x + s.w)   << ", " << EY(s.y + s.h)   << "));\n";
        out << "    p.Line(Point("  << EX(s.x)         << ", " << EY(s.y + s.h)   << "));\n";
        out << "    p.Close();\n";
        if(st.outlineOffsetX || st.outlineOffsetY)
            out << Format("    p.Translate(%d, %d);\n", st.outlineOffsetX, st.outlineOffsetY);
        double oo = Clamp01(st.outlineOpacity) * Clamp01(st.opacity);
        if(oo < 1.0) out << Format("    p.Opacity(%.3f);\n", oo);
        EmitDashCode(out, st.outlineStyle, st.outlineDash);
        const int W = (st.enableStroke ? st.strokeWidth : 0) + max(1, 2*st.outlineWidth);

        if(ov.useOutline) out << Format("    p.Stroke(%d, %s);  //<- Override\n", W, ~ov.outlineName);
        else out << Format("    p.Stroke(%d, Color(%d,%d,%d));\n", W, st.outlineColor.GetR(), st.outlineColor.GetG(), st.outlineColor.GetB());
        out << "    p.End();\n\n";
    }
}

// Circle_EmitPainter - Builds and renders circle path with style passes.
void Circle_EmitPainter(BufferPainter& p, const Rect& inset, const Shape& s) {
    const int cx = X(inset, s.cx);
    const int cy = Y(inset, s.cy);
    const int rr = R(inset, s.r);
    if(rr < 1) return;

    const Style& st = s.style;
    auto Path = [&]{
        p.Move(Pointf(cx + rr, cy));
        p.SvgArc(Pointf(rr, rr), 0, false, true, Pointf(cx - rr, cy));
        p.SvgArc(Pointf(rr, rr), 0, false, true, Pointf(cx + rr, cy));
        p.Close();
    };

    if(st.outlineEnable && st.outlineOutside) Pass_Outline(p, [&]{ p.Begin(); Path(); p.End(); }, st);
    Pass_Fill  (p, [&]{ p.Begin(); Path(); p.End(); }, st);
    Pass_Stroke(p, [&]{ p.Begin(); Path(); p.End(); }, st);
    if(st.outlineEnable && !st.outlineOutside) Pass_Outline(p, [&]{ p.Begin(); Path(); p.End(); }, st);
}

// Circle_HitBody - Checks if point m hits circle (disc or ring based on fill/stroke).
bool Circle_HitBody(const Rect& inset, const Shape& s, Point m) {
    const int cx = X(inset, s.cx);
    const int cy = Y(inset, s.cy);
    const int r  = R(inset, s.r);
    if(r < 1) return false;

    const int dx = m.x - cx, dy = m.y - cy;
    const double d = sqrt(double(dx * dx + dy * dy));
    const int tol = max(6, s.style.strokeWidth / 2 + 4);

    return s.style.enableFill ? (d <= r || fabs(d - r) <= tol) : (fabs(d - r) <= tol);
}

// Circle_HitVertex - Returns vertex index if m near vertex, -1 else.
int Circle_HitVertex(const Rect& inset, const Shape& s, Point m) {
    const Point c(X(inset, s.cx), Y(inset, s.cy));
    const Point e(c.x + R(inset, s.r), c.y); // east handle
    auto Near = [&](Point a){ return abs(a.x - m.x) <= HANDLE_PX && abs(a.y - m.y) <= HANDLE_PX; };
    if(Near(c)) return 0; // center
    if(Near(e)) return 1; // radius handle
    return -1;
}

// Circle_DrawOverlay - Draws selection box and handles on w.
void Circle_DrawOverlay(Draw& w, const Rect& inset, const Shape& s) {
    const int cx = X(inset, s.cx);
    const int cy = Y(inset, s.cy);
    const int rr = R(inset, s.r);
    if(rr < 1) return;

    const Color sel = SColorMark();
    const Rect  bb  = RectC(cx - rr, cy - rr, 2 * rr, 2 * rr);

    // box
    w.DrawRect(bb.left, bb.top, bb.Width(), 1, sel);
    w.DrawRect(bb.left, bb.bottom, bb.Width(), 1, sel);
    w.DrawRect(bb.left, bb.top, 1, bb.Height(), sel);
    w.DrawRect(bb.right, bb.top, 1, bb.Height()+1, sel);

    // handles
    w.DrawRect(cx - 2, cy - 2, 5, 5, sel);          // center
    w.DrawRect(cx + rr - 2, cy - 2, 5, 5, sel);     // east
}

// Circle_BeginCreate - Initializes circle creation at start point.
void Circle_BeginCreate(Shape& s, const Rect& inset, Point start) {
    s.type = PType::Circle;
    s.cx = NX(inset, start.x);
    s.cy = NY(inset, start.y);
    s.r  = 0.0;
}

// Circle_DragCreate - Updates radius during drag.
void Circle_DragCreate(Shape& s, const Rect& inset, Point, Point cur, bool snap, int grid) {
    if(snap){ cur.x = Snap1D(cur.x, inset.left, grid); cur.y = Snap1D(cur.y, inset.top, grid); }
    const double nx = NX(inset, cur.x);
    const double ny = NY(inset, cur.y);
    const double dx = nx - s.cx, dy = ny - s.cy;
    s.r = max(0.0, sqrt(dx * dx + dy * dy));
}

// Circle_BeginEdit - Stores grab position normalized.
void Circle_BeginEdit(Shape&, const Rect& inset, Point grab, int, double& gx, double& gy) {
    gx = NX(inset, grab.x); gy = NY(inset, grab.y);
}

// Circle_DragEdit - Updates circle during drag (move or resize radius).
void Circle_DragEdit(Shape& s, const Rect& inset, Point cur, bool snap, int grid, bool moving, int hv, double& gx, double& gy) {
    if(snap){ cur.x = Snap1D(cur.x, inset.left, grid); cur.y = Snap1D(cur.y, inset.top, grid); }
    const double nx = NX(inset, cur.x);
    const double ny = NY(inset, cur.y);

    if(moving || hv == 0) { s.cx += nx - gx; s.cy += ny - gy; gx = nx; gy = ny; return; }
    if(hv == 1) {
        const double dx = nx - s.cx, dy = ny - s.cy;
        s.r = max(0.0, sqrt(dx * dx + dy * dy));
    }
}

// Circle_EmitCode - Emits C++ code for circle, reserving string for efficiency.
void Circle_EmitCode(String& out, const Shape& s, const StyleOverrides& ov) {
    const Style& st = s.style;
    out.Reserve(out.GetLength() + 512);  // Pre-reserve for efficiency
    out << "    // Circle\n";
    out << "    p.Begin();\n";
    out << "    p.Circle(" << EX(s.cx) << ", " << EY(s.cy) << ", " << ER(s.r) << ");\n";
    EmitOpacityCode(out, st.opacity);
    EmitDashCode(out, st.strokeStyle, st.dash);

    if(st.enableFill){
        if(ov.useFill) out << Format("    p.Fill(%s);  //<- Override\n", ~ov.fillName);
        else out << Format("    p.Fill(Color(%d,%d,%d));\n", st.fill.GetR(), st.fill.GetG(), st.fill.GetB());
    }
    if(st.enableStroke){
        if(ov.useStroke) out << Format("    p.Stroke(%d, %s);  //<- Override\n", st.strokeWidth, ~ov.strokeName);
        else out << Format("    p.Stroke(%d, Color(%d,%d,%d));\n", st.strokeWidth, st.stroke.GetR(), st.stroke.GetG(), st.stroke.GetB());
    }
    out << "    p.End();\n\n";

    if(st.outlineEnable && st.outlineWidth > 0){
        out << "    // Circle outline\n";
        out << "    p.Begin();\n";
        out << "    p.Circle(" << EX(s.cx) << ", " << EY(s.cy) << ", " << ER(s.r) << ");\n";
        if(st.outlineOffsetX || st.outlineOffsetY)
            out << Format("    p.Translate(%d, %d);\n", st.outlineOffsetX, st.outlineOffsetY);
        double oo = Clamp01(st.outlineOpacity) * Clamp01(st.opacity);
        if(oo < 1.0) out << Format("    p.Opacity(%.3f);\n", oo);
        EmitDashCode(out, st.outlineStyle, st.outlineDash);
        const int W = (st.enableStroke ? st.strokeWidth : 0) + max(1, 2*st.outlineWidth);

        if(ov.useOutline) out << Format("    p.Stroke(%d, %s);  //<- Override\n", W, ~ov.outlineName);
        else out << Format("    p.Stroke(%d, Color(%d,%d,%d));\n", W, st.outlineColor.GetR(), st.outlineColor.GetG(), st.outlineColor.GetB());
        out << "    p.End();\n\n";
    }
}

// Line_EmitPainter - Builds and renders line path with style passes.
void Line_EmitPainter(BufferPainter& p, const Rect& inset, const Shape& s) {
    const Point a(X(inset, s.p1.x), Y(inset, s.p1.y));
    const Point b(X(inset, s.p2.x), Y(inset, s.p2.y));
    const int dx = b.x - a.x, dy = b.y - a.y;
    if(dx * dx + dy * dy < MIN_EMIT_PX * MIN_EMIT_PX) return;

    const Style& st = s.style;
    auto Path = [&]{ p.Move(Pointf(a)); p.Line(Pointf(b)); };

    if(st.outlineEnable && st.outlineOutside) Pass_Outline(p, [&]{ p.Begin(); Path(); p.End(); }, st);
    Pass_Fill  (p, [&]{ p.Begin(); Path(); p.End(); }, st);    // harmless when Fill disabled
    Pass_Stroke(p, [&]{ p.Begin(); Path(); p.End(); }, st);
    if(st.outlineEnable && !st.outlineOutside) Pass_Outline(p, [&]{ p.Begin(); Path(); p.End(); }, st);
}

// Line_HitBody - Checks if point m hits line segment (tolerance 6).
bool Line_HitBody(const Rect& inset, const Shape& s, Point m) {
    return IsNearSegment(m, Point(X(inset, s.p1.x), Y(inset, s.p1.y)), Point(X(inset, s.p2.x), Y(inset, s.p2.y)), 6);
}

// Line_HitVertex - Returns endpoint index if m near endpoint, -1 else.
int Line_HitVertex(const Rect& inset, const Shape& s, Point m) {
    Point a(X(inset, s.p1.x), Y(inset, s.p1.y));
    Point b(X(inset, s.p2.x), Y(inset, s.p2.y));
    auto Near = [&](Point q){ return abs(q.x - m.x) <= HANDLE_PX && abs(q.y - m.y) <= HANDLE_PX; };
    if(Near(a)) return 0;
    if(Near(b)) return 1;
    return -1;
}

// Line_DrawOverlay - Draws line and handles on w.
void Line_DrawOverlay(Draw& w, const Rect& inset, const Shape& s) {
    const Color sel = SColorMark();
    Point p1(X(inset, s.p1.x), Y(inset, s.p1.y));
    Point p2(X(inset, s.p2.x), Y(inset, s.p2.y));
    w.DrawLine(p1, p2, 1, sel);

    auto H = [&](Point p){ w.DrawRect(p.x - 3, p.y - 3, 7, 7, sel); };
    H(p1); H(p2);
}

// Line_BeginCreate - Initializes line creation at start point.
void Line_BeginCreate(Shape& s, const Rect& inset, Point start) {
    s.type = PType::Line;
    s.p1 = Pointf(NX(inset, start.x), NY(inset, start.y));
    s.p2 = s.p1;
}

// Line_DragCreate - Updates endpoint during drag.
void Line_DragCreate(Shape& s, const Rect& inset, Point, Point cur, bool snap, int grid) {
    if(snap){ cur.x = Snap1D(cur.x, inset.left, grid); cur.y = Snap1D(cur.y, inset.top, grid); }
    s.p2 = Pointf(NX(inset, cur.x), NY(inset, cur.y));
}

// Line_BeginEdit - Stores grab position normalized.
void Line_BeginEdit(Shape&, const Rect& inset, Point grab, int, double& gx, double& gy) {
    gx = NX(inset, grab.x); gy = NY(inset, grab.y);
}

// Line_DragEdit - Updates line during drag (move or resize endpoint).
void Line_DragEdit(Shape& s, const Rect& inset, Point cur, bool snap, int grid, bool moving, int hv, double& gx, double& gy) {
    if(snap){ cur.x = Snap1D(cur.x, inset.left, grid); cur.y = Snap1D(cur.y, inset.top, grid); }
    const double nx = NX(inset, cur.x), ny = NY(inset, cur.y);

    if(moving) { Pointf d(nx - gx, ny - gy); s.p1 += d; s.p2 += d; gx = nx; gy = ny; return; }
    if(hv == 0) s.p1 = Pointf(nx, ny); else s.p2 = Pointf(nx, ny);
}

// Line_EmitCode - Emits C++ code for line, reserving string for efficiency.
void Line_EmitCode(String& out, const Shape& s, const StyleOverrides& ov) {
    const Style& st = s.style;
    out.Reserve(out.GetLength() + 512);  // Pre-reserve for efficiency
    out << "    // Line\n";
    out << "    p.Begin();\n";
    out << "    p.Move(Point(" << EX(s.p1.x) << ", " << EY(s.p1.y) << "));\n";
    out << "    p.Line(Point(" << EX(s.p2.x) << ", " << EY(s.p2.y) << "));\n";
    EmitOpacityCode(out, st.opacity);
    EmitDashCode(out, st.strokeStyle, st.dash);

    if(st.enableStroke){
        if(ov.useStroke) out << Format("    p.Stroke(%d, %s);  //<- Override\n", st.strokeWidth, ~ov.strokeName);
        else out << Format("    p.Stroke(%d, Color(%d,%d,%d));\n", st.strokeWidth, st.stroke.GetR(), st.stroke.GetG(), st.stroke.GetB());
    }
    if(st.enableFill){
        if(ov.useFill) out << Format("    p.Fill(%s);  //<- Override\n", ~ov.fillName);
        else out << Format("    p.Fill(Color(%d,%d,%d));\n", st.fill.GetR(), st.fill.GetG(), st.fill.GetB());
    }
    out << "    p.End();\n\n";

    if(st.outlineEnable && st.outlineWidth > 0){
        out << "    // Line outline\n";
        out << "    p.Begin();\n";
        out << "    p.Move(Point(" << EX(s.p1.x) << ", " << EY(s.p1.y) << "));\n";
        out << "    p.Line(Point(" << EX(s.p2.x) << ", " << EY(s.p2.y) << "));\n";
        if(st.outlineOffsetX || st.outlineOffsetY)
            out << Format("    p.Translate(%d, %d);\n", st.outlineOffsetX, st.outlineOffsetY);
        double oo = Clamp01(st.outlineOpacity) * Clamp01(st.opacity);
        if(oo < 1.0) out << Format("    p.Opacity(%.3f);\n", oo);
        EmitDashCode(out, st.outlineStyle, st.outlineDash);
        const int W = (st.enableStroke ? st.strokeWidth : 0) + max(1, 2*st.outlineWidth);

        if(ov.useOutline) out << Format("    p.Stroke(%d, %s);  //<- Override\n", W, ~ov.outlineName);
        else out << Format("    p.Stroke(%d, Color(%d,%d,%d));\n", W, st.outlineColor.GetR(), st.outlineColor.GetG(), st.outlineColor.GetB());
        out << "    p.End();\n\n";
    }
}

// Triangle_EmitPainter - Builds and renders triangle path with style passes.
void Triangle_EmitPainter(BufferPainter& p, const Rect& inset, const Shape& s) {
    const Point P[3] = {
        Point(X(inset, s.p1.x), Y(inset, s.p1.y)),
        Point(X(inset, s.p2.x), Y(inset, s.p2.y)),
        Point(X(inset, s.p3.x), Y(inset, s.p3.y))
    };

    Rect bbox = Rect(P[0], P[0]); bbox |= P[1]; bbox |= P[2];
    if(bbox.Width() < MIN_EMIT_PX || bbox.Height() < MIN_EMIT_PX) return;

    const int TwiceArea = abs(P[0].x * (P[1].y - P[2].y) +
                              P[1].x * (P[2].y - P[0].y) +
                              P[2].x * (P[0].y - P[1].y));
    if(TwiceArea < MIN_EMIT_PX * MIN_EMIT_PX) return;

    const Style& st = s.style;
    auto Path = [&]{
        p.Move(Pointf(P[0])); p.Line(Pointf(P[1])); p.Line(Pointf(P[2])); p.Close();
    };

    if(st.outlineEnable && st.outlineOutside) Pass_Outline(p, [&]{ p.Begin(); Path(); p.End(); }, st);
    Pass_Fill  (p, [&]{ p.Begin(); Path(); p.End(); }, st);
    Pass_Stroke(p, [&]{ p.Begin(); Path(); p.End(); }, st);
    if(st.outlineEnable && !st.outlineOutside) Pass_Outline(p, [&]{ p.Begin(); Path(); p.End(); }, st);
}

// Triangle_HitBody - Checks if point m hits triangle (fill or edges).
bool Triangle_HitBody(const Rect& inset, const Shape& s, Point m) {
    Point a(X(inset, s.p1.x), Y(inset, s.p1.y));
    Point b(X(inset, s.p2.x), Y(inset, s.p2.y));
    Point c(X(inset, s.p3.x), Y(inset, s.p3.y));
    if(s.style.enableFill) return IsPointInTriangle(m, a, b, c);
    return IsNearSegment(m, a, b, 6) || IsNearSegment(m, b, c, 6) || IsNearSegment(m, c, a, 6);
}

// Triangle_HitVertex - Returns vertex index if m near vertex (with outside bias), -1 else.
int Triangle_HitVertex(const Rect& inset, const Shape& s, Point m) {
    static constexpr int OUTSIDE_BIAS_PX = HANDLE_PX / 2;
    const Point pA(X(inset, s.p1.x), Y(inset, s.p1.y));
    const Point pB(X(inset, s.p2.x), Y(inset, s.p2.y));
    const Point pC(X(inset, s.p3.x), Y(inset, s.p3.y));
    const double cx = (pA.x + pB.x + pC.x) / 3.0;
    const double cy = (pA.y + pB.y + pC.y) / 3.0;

    auto HitOut = [&](const Point& v) -> bool {
        double dx = v.x - cx, dy = v.y - cy;
        double len = sqrt(dx * dx + dy * dy);
        int ox = 0, oy = 0;
        if (len > 1e-6) {
            dx /= len; dy /= len;
            ox = int(dx * OUTSIDE_BIAS_PX + (dx >= 0 ? 0.5 : -0.5));
            oy = int(dy * OUTSIDE_BIAS_PX + (dy >= 0 ? 0.5 : -0.5));
        }
        Rect hit = RectC(v.x - HANDLE_PX, v.y - HANDLE_PX, 2 * HANDLE_PX + 1, 2 * HANDLE_PX + 1);
        hit.Offset(ox, oy);
        return hit.Contains(m);
    };

    if(HitOut(pA)) return 0;
    if(HitOut(pB)) return 1;
    if(HitOut(pC)) return 2;
    return -1;
}

// Triangle_DrawOverlay - Draws edges and handles on w.
void Triangle_DrawOverlay(Draw& w, const Rect& inset, const Shape& s) {
    const Color sel = SColorMark();
    Point a(X(inset, s.p1.x), Y(inset, s.p1.y));
    Point b(X(inset, s.p2.x), Y(inset, s.p2.y));
    Point c(X(inset, s.p3.x), Y(inset, s.p3.y));
    w.DrawLine(a, b, 1, sel);
    w.DrawLine(b, c, 1, sel);
    w.DrawLine(c, a, 1, sel);

    auto H = [&](Point p){ w.DrawRect(p.x - 3, p.y - 3, 7, 7, sel); };
    H(a); H(b); H(c);
}

// Triangle_BeginCreate - Initializes triangle creation at start point.
void Triangle_BeginCreate(Shape& s, const Rect& inset, Point start_px) {
    s.type = PType::Triangle;
    s.p1 = Pointf(NX(inset, start_px.x), NY(inset, start_px.y));
    s.p2 = s.p1; s.p3 = s.p1;
}

// Triangle_DragCreate - Updates points during drag.
void Triangle_DragCreate(Shape& s, the Rect& inset, Point, Point cur_px, bool snap, int grid) {
    if(snap){ cur_px.x = Snap1D(cur_px.x, inset.left, grid); cur_px.y = Snap1D(cur_px.y, inset.top, grid); }
    const Pointf p1 = s.p1;
    const Pointf p2 = Pointf(NX(inset, cur_px.x), NY(inset, cur_px.y));
    const Pointf p3 = Pointf(2 * p1.x - p2.x, p2.y);
    s.p2 = p2; s.p3 = p3;
}

// Triangle_BeginEdit - Stores grab position normalized.
void Triangle_BeginEdit(Shape& s, the Rect& inset, Point grab, int, double& gx, double& gy) {
    gx = NX(inset, grab.x); gy = NY(inset, grab.y);
}

// Triangle_DragEdit - Updates triangle during drag (move or resize vertex).
void Triangle_DragEdit(Shape& s, the Rect& inset, Point cur, bool snap, int grid, bool moving, int hv, double& gx, double& gy) {
    if(snap){ cur.x = Snap1D(cur.x, inset.left, grid); cur.y = Snap1D(cur.y, inset.top, grid); }
    const double nx = NX(inset, cur.x), ny = NY(inset, cur.y);
    if(moving){ const Pointf d(nx - gx, ny - gy); s.p1 += d; s.p2 += d; s.p3 += d; gx = nx; gy = ny; return; }
    if(hv == 0) s.p1 = Pointf(nx, ny);
    if(hv == 1) s.p2 = Pointf(nx, ny);
    if(hv == 2) s.p3 = Pointf(nx, ny);
}

// Triangle_EmitCode - Emits C++ code for triangle, reserving string for efficiency.
void Triangle_EmitCode(String& out, const Shape& s, const StyleOverrides& ov) {
    const Style& st = s.style;
    out.Reserve(out.GetLength() + 512);  // Pre-reserve for efficiency
    out << "    // Triangle\n";
    out << "    p.Begin();\n";
    out << "    p.Move(Point(" << EX(s.p1.x) << ", " << EY(s.p1.y) << "));\n";
    out << "    p.Line(Point(" << EX(s.p2.x) << ", " << EY(s.p2.y) << "));\n";
    out << "    p.Line(Point(" << EX(s.p3.x) << ", " << EY(s.p3.y) << "));\n";
    out << "    p.Close();\n";
    EmitOpacityCode(out, st.opacity);
    if(st.evenOdd) out << "    p.EvenOdd(true);\n";
    EmitDashCode(out, st.strokeStyle, st.dash);

    if(st.enableFill){
        if(ov.useFill) out << Format("    p.Fill(%s);  //<- Override\n", ~ov.fillName);
        else out << Format("    p.Fill(Color(%d,%d,%d));\n", st.fill.GetR(), st.fill.GetG(), st.fill.GetB());
    }
    if(st.enableStroke){
        if(ov.useStroke) out << Format("    p.Stroke(%d, %s);  //<- Override\n", st.strokeWidth, ~ov.strokeName);
        else out << Format("    p.Stroke(%d, Color(%d,%d,%d));\n", st.strokeWidth, st.stroke.GetR(), st.stroke.GetG(), st.stroke.GetB());
    }
    out << "    p.End();\n\n";

    if(st.outlineEnable && st.outlineWidth > 0){
        out << "    // Triangle outline\n";
        out << "    p.Begin();\n";
        out << "    p.Move(Point(" << EX(s.p1.x) << ", " << EY(s.p1.y) << "));\n";
        out << "    p.Line(Point(" << EX(s.p2.x) << ", " << EY(s.p2.y) << "));\n";
        out << "    p.Line(Point(" << EX(s.p3.x) << ", " << EY(s.p3.y) << "));\n";
        out << "    p.Close();\n";
        if(st.outlineOffsetX || st.outlineOffsetY)
            out << Format("    p.Translate(%d, %d);\n", st.outlineOffsetX, st.outlineOffsetY);
        double oo = Clamp01(st.outlineOpacity) * Clamp01(st.opacity);
        if(oo < 1.0) out << Format("    p.Opacity(%.3f);\n", oo);
        EmitDashCode(out, st.outlineStyle, st.outlineDash);
        const int W = (st.enableStroke ? st.strokeWidth : 0) + max(1, 2*st.outlineWidth);

        if(ov.useOutline) out << Format("    p.Stroke(%d, %s);  //<- Override\n", W, ~ov.outlineName);
        else out << Format("    p.Stroke(%d, Color(%d,%d,%d));\n", W, st.outlineColor.GetR(), st.outlineColor.GetG(), st.outlineColor.GetB());
        out << "    p.End();\n\n";
    }
}

// Text helper - Creates font with height pxH and styles.
Font MakeFontPx(const TextData& td, int pxH) {
    Font f;
    if(!IsNull(td.face)) f.FaceName(td.face);
    f.Height(pxH);
    if(td.bold)   f.Bold();
    if(td.italic) f.Italic();
    return f;
}

// Text helper - Computes pixel rect for text (defensive sizing).
inline Rect TextPixelRect(const Rect& inset, const Shape& s) {
    const TextData& td = s.text;
    const int pxh = max(1, int(inset.Height() * td.sizeN + 0.5));
    Font F = MakeFontPx(td, pxh);

    const String txt = td.text.IsEmpty() ? String("Text") : td.text;
    const Size tsz = GetTextSize(txt, F);

    const int x = X(inset, s.x);
    const int y = Y(inset, s.y);                // TOP (not baseline)
    const int h = max(pxh, tsz.cy);             // prefer larger defensively
    return RectC(x, y, max(tsz.cx, 10), h);
}

// Text_EmitPainter - Builds and renders text path with style passes.
void Text_EmitPainter(BufferPainter& p, const Rect& inset, const Shape& s) {
    const TextData& td = s.text;
    if(td.text.IsEmpty()) return;

    const int pxh = max(0, int(inset.Height() * td.sizeN + 0.5));
    if(pxh < MIN_EMIT_PX) return;

    Font F = MakeFontPx(td, pxh);
    const int top = Y(inset, s.y);
    const Style& st = s.style;

    auto Path = [&]{
        Pointf pen(X(inset, s.x), top);    // TOP (no ascent)
        for(int i = 0; i < td.text.GetCount(); ++i){
            const int ch = td.text[i];
            p.Character(pen, ch, F);
            pen.x += GetTextSize(String(ch,1), F).cx;
        }
    };

    if(st.outlineEnable && st.outlineOutside) Pass_Outline(p, [&]{ p.Begin(); Path(); p.End(); }, st);
    Pass_Fill  (p, [&]{ p.Begin(); Path(); p.End(); }, st);
    Pass_Stroke(p, [&]{ p.Begin(); Path(); p.End(); }, st);
    if(st.outlineEnable && !st.outlineOutside) Pass_Outline(p, [&]{ p.Begin(); Path(); p.End(); }, st);
}

// Text_HitBody - Checks if point m hits text rect (inflated).
bool Text_HitBody(const Rect& inset, const Shape& s, Point m) { return TextPixelRect(inset, s).Inflated(4).Contains(m); }

// Text_HitVertex - Returns corner index if m near vertex, -1 else.
int Text_HitVertex(const Rect& inset, const Shape& s, Point m) {
    const Rect r = TextPixelRect(inset, s);
    const Point tl = r.TopLeft();
    const Point tr(r.right, r.top);
    const Point br = r.BottomRight();
    const Point bl(r.left, r.bottom);
    auto Near = [&](Point a){ return abs(a.x-m.x)<=HANDLE_PX && abs(a.y-m.y)<=HANDLE_PX; };
    if(Near(tl)) return 0;
    if(Near(tr)) return 1;
    if(Near(br)) return 2;
    if(Near(bl)) return 3;
    return -1;
}

// Text_DrawOverlay - Draws selection box and handles on w.
void Text_DrawOverlay(Draw& w, const Rect& inset, const Shape& s) {
    const Color sel = SColorMark();
    const Rect  r   = TextPixelRect(inset, s);

    // box
    w.DrawRect(r.left, r.top, r.Width(), 1, sel);
    w.DrawRect(r.left, r.bottom, r.Width(), 1, sel);
    w.DrawRect(r.left, r.top, 1, r.Height(), sel);
    w.DrawRect(r.right, r.top, 1, r.Height()+1, sel);

    // handles
    auto H = [&](Point p){ w.DrawRect(p.x - 3, p.y - 3, 7, 7, sel); };
    H(r.TopLeft()); H(Point(r.right, r.top)); H(Point(r.left, r.bottom)); H(r.BottomRight());
}

// Text_BeginCreate - Initializes text creation at start point.
void Text_BeginCreate(Shape& s, const Rect& inset, Point start) {
    s.type = PType::Text;
    s.x = NX(inset, start.x);
    s.y = NY(inset, start.y);    // TOP (normalized in drag)
    if(s.text.text.IsEmpty()) s.text.text = "Text";
}

// Text_DragCreate - Updates text height during drag.
void Text_DragCreate(Shape& s, const Rect& inset, Point start, Point cur, bool snap, int grid) {
    if(snap) cur.y = Snap1D(cur.y, inset.top, grid);

    const double y0 = NY(inset, start.y);
    const double y1 = NY(inset, cur.y);

    const double top = min(y0, y1);
    const double h   = max(0.02, fabs(y1 - y0));

    s.y          = top;     // TOP
    s.text.sizeN = h;       // normalized height
}

// Text_BeginEdit - Stores grab position normalized.
void Text_BeginEdit(Shape&, const Rect& inset, Point grab, int, double& gx, double& gy) {
    gx = NX(inset, grab.x); gy = NY(inset, grab.y);
}

// Text_DragEdit - Updates text during drag (move or resize height).
void Text_DragEdit(Shape& s, const Rect& inset, Point cur, bool snap, int grid, bool moving, int hv, double& gx, double& gy) {
    if(snap){ cur.x = Snap1D(cur.x, inset.left, grid); cur.y = Snap1D(cur.y, inset.top, grid); }
    const double nx = NX(inset, cur.x), ny = NY(inset, cur.y);

    if(moving){ s.x += nx - gx; s.y += ny - gy; gx = nx; gy = ny; return; }

    double bottom = s.y + s.text.sizeN;
    switch(hv){
        case 0: case 1: // top edge
            s.y = ny;
            if(s.y > bottom) Swap(s.y, bottom);
            s.text.sizeN = max(0.02, bottom - s.y);
            break;
        case 2: case 3: // bottom edge
            bottom = ny;
            if(bottom < s.y) Swap(s.y, bottom);
            s.text.sizeN = max(0.02, bottom - s.y);
            break;
        default: break;
    }
}

// Text_EmitCode - Emits C++ code for text, reserving string for efficiency.
void Text_EmitCode(String& out, const Shape& s, const StyleOverrides& ov) {
    const Style& st = s.style;
    const String T = s.text.text.IsEmpty() ? String("Text") : s.text.text;

    out.Reserve(out.GetLength() + 512);  // Pre-reserve for efficiency
    out << "    // Text (TOP-aligned)\n";
    out << "    p.Begin();\n";
    out << "    {\n";
    out << "        Pointf pen(" << EX(s.x) << ", " << EY(s.y) << ");\n";
    out << "        Font F; F.Height((int)(inset.Height() * " << Decimal4(s.text.sizeN) << " + 0.5));\n";
    if(!s.text.face.IsEmpty()) out << "        F.FaceName(\"" << s.text.face << "\");\n";
    if(s.text.bold)            out << "        F.Bold();\n";
    if(s.text.italic)          out << "        F.Italic();\n";
    out << "        String T = \"" << T << "\";\n";
    out << "        for(int i=0;i<T.GetCount();++i){ int ch=T[i]; p.Character(pen,ch,F); pen.x += GetTextSize(String(ch,1),F).cx; }\n";
    out << "    }\n";
    EmitOpacityCode(out, st.opacity);
    if(st.evenOdd) out << "    p.EvenOdd(true);\n";
    EmitDashCode(out, st.strokeStyle, st.dash);

    if(st.enableFill){
        if(ov.useFill) out << Format("    p.Fill(%s);  //<- Override\n", ~ov.fillName);
        else out << Format("    p.Fill(Color(%d,%d,%d));\n", st.fill.GetR(), st.fill.GetG(), st.fill.GetB());
    }
    if(st.enableStroke){
        if(ov.useStroke) out << Format("    p.Stroke(%d, %s);  //<- Override\n", st.strokeWidth, ~ov.strokeName);
        else out << Format("    p.Stroke(%d, Color(%d,%d,%d));\n", st.strokeWidth, st.stroke.GetR(), st.stroke.GetG(), st.stroke.GetB());
    }
    out << "    p.End();\n\n";

    if(st.outlineEnable && st.outlineWidth > 0){
        out << "    // Text outline\n";
        out << "    p.Begin();\n";
        out << "    {\n";
        out << "        Pointf pen(" << EX(s.x) << ", " << EY(s.y) << ");\n";
        out << "        Font F; F.Height((int)(inset.Height() * " << Decimal4(s.text.sizeN) << " + 0.5));\n";
        if(!s.text.face.IsEmpty()) out << "        F.FaceName(\"" << s.text.face << "\");\n";
        if(s.text.bold)            out << "        F.Bold();\n";
        if(s.text.italic)          out << "        F.Italic();\n";
        out << "        String T = \"" << T << "\";\n";
        out << "        for(int i=0;i<T.GetCount();++i){ int ch=T[i]; p.Character(pen,ch,F); pen.x += GetTextSize(String(ch,1),F).cx; }\n";
        out << "    }\n";
        if(st.outlineOffsetX || st.outlineOffsetY)
            out << Format("    p.Translate(%d, %d);\n", st.outlineOffsetX, st.outlineOffsetY);
        double oo = Clamp01(st.outlineOpacity) * Clamp01(st.opacity);
        if(oo < 1.0) out << Format("    p.Opacity(%.3f);\n", oo);
        EmitDashCode(out, st.outlineStyle, st.outlineDash);
        const int W = (st.enableStroke ? st.strokeWidth : 0) + max(1, 2*st.outlineWidth);

        if(ov.useOutline) out << Format("    p.Stroke(%d, %s);  //<- Override\n", W, ~ov.outlineName);
        else out << Format("    p.Stroke(%d, Color(%d,%d,%d));\n", W, st.outlineColor.GetR(), st.outlineColor.GetG(), st.outlineColor.GetB());
        out << "    p.End();\n\n";
    }
}

// Curve_EmitPainter - Builds and renders curve path with style passes.
void Curve_EmitPainter(BufferPainter& p, const Rect& inset, const Shape& s) {
    const Style& st = s.style;
    const CurveData& c = s.curve;
    auto P = [&](Pointf q) { return Pointf(X(inset, q.x), Y(inset, q.y)); };

    auto Path = [&]{
        p.Move(P(c.a0));
        if(c.cubic) p.Cubic(P(c.c0), P(c.c1), P(c.a1));
        else        p.Quadratic(P(c.c0), P(c.a1));
        if(c.closed) p.Close();
    };

    if(st.outlineEnable && st.outlineOutside) Pass_Outline(p, [&]{ p.Begin(); Path(); p.End(); }, st);
    if(c.closed) Pass_Fill(p, [&]{ p.Begin(); Path(); p.End(); }, st);
    Pass_Stroke(p, [&]{ p.Begin(); Path(); p.End(); }, st);
    if(st.outlineEnable && !st.outlineOutside) Pass_Outline(p, [&]{ p.Begin(); Path(); p.End(); }, st);
}

// Curve_HitBody - Checks if point m hits curve bounds (inflated for tolerance).
bool Curve_HitBody(const Rect& inset, const Shape& s, Point m) {
    auto P = [&](Pointf q) { return Point(X(inset, q.x), Y(inset, q.y)); };
    const CurveData& c = s.curve;
    Point a0 = P(c.a0), a1 = P(c.a1), k0 = P(c.c0), k1 = P(c.c1);
    Rect tight(min(min(a0.x, a1.x), min(k0.x, k1.x)),
               min(min(a0.y, a1.y), min(k0.y, k1.y)),
               max(max(a0.x, a1.x), max(k0.x, k1.x)),
               max(max(a0.y, a1.y), max(k0.y, k1.y)));
    return tight.Inflated(6).Contains(m);
}

// Curve_HitVertex - Returns vertex index if m near vertex, -1 else.
int Curve_HitVertex(const Rect& inset, const Shape& s, Point m) {
    auto P = [&](Pointf q) { return Point(X(inset, q.x), Y(inset, q.y)); };
    const CurveData& c = s.curve;
    Point pts[4] = {P(c.a0), P(c.c0), P(c.c1), P(c.a1)};
    const int n = c.cubic ? 4 : 3;
    for(int i = 0; i < n; ++i)
        if(abs(pts[i].x - m.x) <= HANDLE_PX && abs(pts[i].y - m.y) <= HANDLE_PX)
            return i;
    return -1;
}

// Curve_DrawOverlay - Draws control lines and handles on w.
void Curve_DrawOverlay(Draw& w, const Rect& inset, the Shape& s) {
    const Color sel = SColorMark();
    auto P = [&](Pointf q) { return Point(X(inset, q.x), Y(inset, q.y)); };
    const CurveData& c = s.curve;
    Point a0 = P(c.a0), a1 = P(c.a1), k0 = P(c.c0), k1 = P(c.c1);

    w.DrawLine(a0, k0, 1, sel);
    if(c.cubic) w.DrawLine(a1, k1, 1, sel);

    auto H = [&](Point pt){ w.DrawRect(pt.x - 3, pt.y - 3, 6, 6, sel); };
    H(a0); H(k0); if(c.cubic) H(k1); H(a1);
}

// Curve_BeginCreate - Initializes curve creation at start point.
void Curve_BeginCreate(Shape& s, const Rect& inset, Point start) {
    s.type = PType::Curve;
    Pointf q(NX(inset, start.x), NY(inset, start.y));
    s.curve.a0 = s.curve.a1 = s.curve.c0 = s.curve.c1 = q;
    s.curve.cubic = true; s.curve.closed = false;
}

// Curve_DragCreate - Updates curve during drag (seeds controls at 1/3, 2/3).
void Curve_DragCreate(Shape& s, const Rect& inset, Point, Point cur, bool snap, int grid) {
    if(snap){ cur.x = Snap1D(cur.x, inset.left, grid); cur.y = Snap1D(cur.y, inset.top, grid); }
    s.curve.a1 = Pointf(NX(inset, cur.x), NY(inset, cur.y));
    // seed handles at 1/3 and 2/3 along chord
    s.curve.c0 = Pointf((s.curve.a0.x * 2 + s.curve.a1.x) / 3.0,
                        (s.curve.a0.y * 2 + s.curve.a1.y) / 3.0);
    s.curve.c1 = Pointf((s.curve.a0.x + s.curve.a1.x * 2) / 3.0,
                        (s.curve.a0.y + s.curve.a1.y * 2) / 3.0);
}

// Curve_BeginEdit - Stores grab position normalized.
void Curve_BeginEdit(Shape& s, const Rect& inset, Point grab, int, double& gx, double& gy) {
    gx = NX(inset, grab.x); gy = NY(inset, grab.y);
}

// Curve_DragEdit - Updates curve during drag (move or resize anchor/control).
void Curve_DragEdit(Shape& s, const Rect& inset, Point cur, bool snap, int grid, bool moving, int hv, double& gx, double& gy) {
    if(snap){ cur.x = Snap1D(cur.x, inset.left, grid); cur.y = Snap1D(cur.y, inset.top, grid); }
    const double nx = NX(inset, cur.x), ny = NY(inset, cur.y);
    if(moving) {
        const Pointf d(nx - gx, ny - gy);
        s.curve.a0 += d; s.curve.a1 += d; s.curve.c0 += d; if(s.curve.cubic) s.curve.c1 += d;
        gx = nx; gy = ny; return;
    }
    Pointf np(nx, ny);
    switch(hv) {
        case 0: s.curve.a0 = np; break;
        case 1: s.curve.c0 = np; break;
        case 2: if(s.curve.cubic) s.curve.c1 = np; break;
        case 3: s.curve.a1 = np; break;
        default: break;
    }
}

// Curve_EmitCode - Emits C++ code for curve, reserving string for efficiency.
void Curve_EmitCode(String& out, const Shape& s, const StyleOverrides& ov) {
    const Style& st = s.style;
    const CurveData& c = s.curve;
    out.Reserve(out.GetLength() + 512);  // Pre-reserve for efficiency
    out << "    // Curve\n";
    out << "    p.Begin();\n";
    out << "    p.Move(Point(" << EX(c.a0.x) << ", " << EY(c.a0.y) << "));\n";
    if(c.cubic)
        out << "    p.Cubic(Point(" << EX(c.c0.x) << ", " << EY(c.c0.y) << "), Point(" << EX(c.c1.x) << ", " << EY(c.c1.y) << "), Point(" << EX(c.a1.x) << ", " << EY(c.a1.y) << "));\n";
    else
        out << "    p.Quadratic(Point(" << EX(c.c0.x) << ", " << EY(c.c0.y) << "), Point(" << EX(c.a1.x) << ", " << EY(c.a1.y) << "));\n";
    if(c.closed) out << "    p.Close();\n";
    EmitOpacityCode(out, st.opacity);
    if(st.evenOdd) out << "    p.EvenOdd(true);\n";
    EmitDashCode(out, st.strokeStyle, st.dash);

    if(c.closed && st.enableFill){
        if(ov.useFill) out << Format("    p.Fill(%s);  //<- Override\n", ~ov.fillName);
        else out << Format("    p.Fill(Color(%d,%d,%d));\n", st.fill.GetR(), st.fill.GetG(), st.fill.GetB());
    }
    if(st.enableStroke){
        if(ov.useStroke) out << Format("    p.Stroke(%d, %s);  //<- Override\n", st.strokeWidth, ~ov.strokeName);
        else out << Format("    p.Stroke(%d, Color(%d,%d,%d));\n", st.strokeWidth, st.stroke.GetR(), st.stroke.GetG(), st.stroke.GetB());
    }
    out << "    p.End();\n\n";

    if(st.outlineEnable && st.outlineWidth > 0){
        out << "    // Curve outline\n";
        out << "    p.Begin();\n";
        out << "    p.Move(Point(" << EX(c.a0.x) << ", " << EY(c.a0.y) << "));\n";
        if(c.cubic)
            out << "    p.Cubic(Point(" << EX(c.c0.x) << ", " << EY(c.c0.y) << "), Point(" << EX(c.c1.x) << ", " << EY(c.c1.y) << "), Point(" << EX(c.a1.x) << ", " << EY(c.a1.y) << "));\n";
        else
            out << "    p.Quadratic(Point(" << EX(c.c0.x) << ", " << EY(c.c0.y) << "), Point(" << EX(c.a1.x) << ", " << EY(c.a1.y) << "));\n";
        if(c.closed) out << "    p.Close();\n";
        if(st.outlineOffsetX || st.outlineOffsetY)
            out << Format("    p.Translate(%d, %d);\n", st.outlineOffsetX, st.outlineOffsetY);

        const double oo = Clamp01(st.outlineOpacity) * Clamp01(st.opacity);
        if(oo < 1.0) out << Format("    p.Opacity(%.3f);\n", oo);

        EmitDashCode(out, st.outlineStyle, st.outlineDash);
        const int W = (st.enableStroke ? st.strokeWidth : 0) + max(1, 2*st.outlineWidth);
        if(ov.useOutline) out << Format("    p.Stroke(%d, %s);  //<- Override\n", W, ~ov.outlineName);
        else out << Format("    p.Stroke(%d, Color(%d,%d,%d));\n", W, st.outlineColor.GetR(), st.outlineColor.GetG(), st.outlineColor.GetB());
        out << "    p.End();\n\n";
    }
}

// Registry build - Registers all primitives with their ops and tool specs.
struct FacetRow : Moveable<FacetRow> {
    PType t;
    PrimitiveOps ops;
    ToolSpec spec;
};

Vector<FacetRow>& Facets() {
    static Vector<FacetRow> F;
    if(!F.IsEmpty()) return F;

    auto add = [&](PType t, PrimitiveOps ops, const char* label, const char* tip) {
        FacetRow& r = F.Add();
        r.t = t; r.ops = ops;
        r.spec.type = t; r.spec.label = label; r.spec.tip = tip;
    };

    PrimitiveOps R { Rect_EmitPainter, Rect_HitBody, Rect_HitVertex, Rect_DrawOverlay, Rect_BeginCreate, Rect_DragCreate, Rect_BeginEdit, Rect_DragEdit, Rect_EmitCode };
    PrimitiveOps C { Circle_EmitPainter, Circle_HitBody, Circle_HitVertex, Circle_DrawOverlay, Circle_BeginCreate, Circle_DragCreate, Circle_BeginEdit, Circle_DragEdit, Circle_EmitCode };
    PrimitiveOps L { Line_EmitPainter, Line_HitBody, Line_HitVertex, Line_DrawOverlay, Line_BeginCreate, Line_DragCreate, Line_BeginEdit, Line_DragEdit, Line_EmitCode };
    PrimitiveOps T { Triangle_EmitPainter, Triangle_HitBody, Triangle_HitVertex, Triangle_DrawOverlay, Triangle_BeginCreate, Triangle_DragCreate, Triangle_BeginEdit, Triangle_DragEdit, Triangle_EmitCode };
    PrimitiveOps TX { Text_EmitPainter, Text_HitBody, Text_HitVertex, Text_DrawOverlay, Text_BeginCreate, Text_DragCreate, Text_BeginEdit, Text_DragEdit, Text_EmitCode };
    PrimitiveOps CV { Curve_EmitPainter, Curve_HitBody, Curve_HitVertex, Curve_DrawOverlay, Curve_BeginCreate, Curve_DragCreate, Curve_BeginEdit, Curve_DragEdit, Curve_EmitCode };

    add(PType::Rect,     R,  "Rect",     "Insert rectangle");
    add(PType::Circle,   C,  "Circle",   "Insert circle");
    add(PType::Line,     L,  "Line",     "Insert line");
    add(PType::Triangle, T,  "Triangle", "Insert triangle");
    add(PType::Text,     TX, "Text",     "Insert text");
    add(PType::Curve,    CV, "Curve",    "Insert curve");

    return F;
}

const PrimitiveOps& GetOps(PType t) {
    const Vector<FacetRow>& v = Facets();
    for(const FacetRow& r : v) if(r.t == t) return r.ops;
    return v[0].ops; // fallback to Rect
}

const Vector<ToolSpec>& GetToolSpecs() {
    static Vector<ToolSpec> S;
    if(!S.IsEmpty()) return S;
    for(const FacetRow& r : Facets()) S.Add(r.spec);
    return S;
}