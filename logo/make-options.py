"""Generate the wfmR Earthrise logo options as stroke-only SVGs.

All marks share one Earth (radius 21, terminator tilted 22 degrees so the
shadow sits lower-left) and one stroke weight, so the options differ only
along the axis each is meant to test.
"""
import math

SW = 4            # stroke width in viewBox units (viewBox is 100 x 100)
R = 21.0          # Earth radius
A = 8.5           # terminator semi-minor axis: lit fraction ~ (1 + A/R)/2 = 0.70
TILT = -22.0      # degrees, SVG convention (negative = anticlockwise on screen)
COLOUR = "#575279"  # Rose Pine dawn text, the package's body-text colour


def f(v):
    s = f"{v:.2f}".rstrip("0").rstrip(".")
    return "0" if s == "-0" else s


def pt(p):
    return f"{f(p[0])},{f(p[1])}"


def rot(x, y, deg, cx=0.0, cy=0.0):
    t = math.radians(deg)
    x, y = x - cx, y - cy
    return (cx + x * math.cos(t) - y * math.sin(t), cy + x * math.sin(t) + y * math.cos(t))


def poles(cx, cy, r=R, tilt=TILT):
    """Top and bottom of the terminator, on the Earth's circle."""
    return rot(cx, cy - r, tilt, cx, cy), rot(cx, cy + r, tilt, cx, cy)


def circle(cx, cy, r=R):
    return f'<circle cx="{f(cx)}" cy="{f(cy)}" r="{f(r)}"/>'


def terminator(cx, cy, end=None, r=R, a=A, tilt=TILT):
    top, bot = poles(cx, cy, r, tilt)
    end = end or bot
    return f'<path d="M{pt(top)} A{f(a)},{f(r)} {f(tilt)} 0 0 {pt(end)}"/>'


def gibbous(cx, cy, r=R, a=A, tilt=TILT):
    top, bot = poles(cx, cy, r, tilt)
    return (f'<path d="M{pt(top)} A{f(r)},{f(r)} 0 1 1 {pt(bot)} '
            f'A{f(a)},{f(r)} {f(tilt)} 0 1 {pt(top)} Z"/>')


def quad(p0, p1, p2):
    return f'<path d="M{pt(p0)} Q{pt(p1)} {pt(p2)}"/>'


def bez(p0, p1, p2, t):
    u = 1 - t
    return (u * u * p0[0] + 2 * u * t * p1[0] + t * t * p2[0],
            u * u * p0[1] + 2 * u * t * p1[1] + t * t * p2[1])


def horizon_y(x, p0, p1, p2):
    # x is linear in t because the control point sits at the midpoint
    t = (x - p0[0]) / (p2[0] - p0[0])
    return bez(p0, p1, p2, t)[1]


def bisect(g, lo, hi, n=60):
    glo = g(lo)
    for _ in range(n):
        mid = (lo + hi) / 2
        gm = g(mid)
        if (gm < 0) == (glo < 0):
            lo, glo = mid, gm
        else:
            hi = mid
    return (lo + hi) / 2


def rising(cx, cy, p0, p1, p2, r=R, a=A, tilt=TILT):
    """Earth cut by the horizon: limb arc and terminator stop at the curve."""
    def g(t):
        x, y = bez(p0, p1, p2, t)
        return math.hypot(x - cx, y - cy) - r
    tl = bisect(g, 0.0, 0.5)
    tr = bisect(g, 1.0, 0.5)
    left, right = bez(p0, p1, p2, tl), bez(p0, p1, p2, tr)
    limb = f'<path d="M{pt(left)} A{f(r)},{f(r)} 0 1 1 {pt(right)}"/>'

    def term_pt(phi):
        x, y = a * math.cos(math.radians(phi)), -r * math.sin(math.radians(phi))
        x, y = rot(x, y, tilt)
        return (cx + x, cy + y)

    def h(phi):
        x, y = term_pt(phi)
        return y - horizon_y(x, p0, p1, p2)   # > 0 once below the horizon
    phi_cross = bisect(h, 90.0, 270.0)
    return limb + "\n  " + terminator(cx, cy, end=term_pt(phi_cross), r=r, a=a, tilt=tilt)


def catmull_rom(points):
    pts = [points[0]] + list(points) + [points[-1]]
    d = f"M{pt(points[0])}"
    for i in range(1, len(pts) - 2):
        p0, p1, p2, p3 = pts[i - 1], pts[i], pts[i + 1], pts[i + 2]
        c1 = (p1[0] + (p2[0] - p0[0]) / 6, p1[1] + (p2[1] - p0[1]) / 6)
        c2 = (p2[0] - (p3[0] - p1[0]) / 6, p2[1] - (p3[1] - p1[1]) / 6)
        d += f" C{pt(c1)} {pt(c2)} {pt(p2)}"
    return f'<path d="{d}"/>'


def svg(body, name):
    return (f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100" color="{COLOUR}" '
            f'fill="none" stroke="currentColor" stroke-width="{SW}" '
            f'stroke-linecap="round" stroke-linejoin="round">\n'
            f'  <title>wfmR: Earthrise, {name}</title>\n  {body}\n</svg>\n')


# Shared horizon: a gentle arc (radius ~150, so the Moon is 7 Earths across)
H0, H1, H2 = (8, 79), (50, 67), (92, 79)
EARTH = (50.0, 44.0)

options = {
    "a-three-strokes": circle(*EARTH) + "\n  " + terminator(*EARTH) + "\n  " + quad(H0, H1, H2),
    "b-lit-limb": gibbous(*EARTH) + "\n  " + quad(H0, H1, H2),
}

# C: the Earth sits lower and the horizon cuts it; shift the whole group up to centre it
C0, C1, C2 = (8, 67), (50, 55), (92, 67)
options["c-rising"] = rising(50.0, 54.0, C0, C1, C2) + "\n  " + quad(C0, C1, C2)

# D: rugged skyline, bumps of about half a stroke width on the shared arc
xs = [8, 18, 29, 40, 50, 60, 71, 82, 92]
offs = [0.0, -1.5, 0.5, -0.6, 1.2, -1.8, 0.0, -1.0, 0.3]
terrain = [(x, horizon_y(x, H0, H1, H2) + o) for x, o in zip(xs, offs)]
options["d-terrain"] = circle(*EARTH) + "\n  " + terminator(*EARTH) + "\n  " + catmull_rom(terrain)

# E: option A rotated 90 degrees anticlockwise, the photo's original orientation
def turn(p):
    return (p[1], 100 - p[0])
ecx, ecy = turn(EARTH)
top, bot = (turn(q) for q in poles(*EARTH))
options["e-as-shot"] = (
    circle(ecx, ecy) + "\n  "
    + f'<path d="M{pt(top)} A{f(A)},{f(R)} {f(TILT - 90)} 0 0 {pt(bot)}"/>' + "\n  "
    + quad(turn(H0), turn(H1), turn(H2))
)

# F: the lunar surface as a filled slab with the shared curve on top; the Earth
# is cut by it as in C, so the only filled element is the ground
slab = f'<path fill="currentColor" d="M{pt(C0)} Q{pt(C1)} {pt(C2)} V77 H{f(C0[0])} Z"/>'
options["f-solid-ground"] = rising(50.0, 54.0, C0, C1, C2) + "\n  " + slab

for key, body in options.items():
    with open(f"earthrise-{key}.svg", "w") as fh:
        fh.write(svg(body, key.split("-", 1)[1].replace("-", " ")))
    print("wrote", f"earthrise-{key}.svg")
