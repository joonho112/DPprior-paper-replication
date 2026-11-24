#!/usr/bin/env python3
# Author: JoonHo Lee (jlee296@ua.edu)
"""Render the accepted Figure 5 at manuscript size with embedded Lato typefaces."""
import argparse
import csv
import math
import subprocess
import sys
from pathlib import Path

from reportlab.lib.colors import HexColor, white
from reportlab.pdfbase.pdfmetrics import registerFont, stringWidth
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.pdfgen.canvas import Canvas

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "common"))
from figure_runtime import lato_font

WIDTH, HEIGHT = 540, 460.8
INK, MUTED = "#1E2831", "#475762"
HAIR, GRID, SLATE = "#CDD5D8", "#DAE0E2", "#4C6271"
TEAL, POS, NEG = "#006C70", "#CCE4E0", "#F2DBD1"
APPS = ("star", "bd", "rasch")
NAMES = ("Project STAR", "Writing-to-learn", "Vocabulary")
UNITS = ("Achievement SDs", "Standardized mean difference", "Logits")
SAMPLES = ("79 schools", "48 studies", "500 respondents")


def read_rows(path):
    with path.open(newline="") as stream:
        return list(csv.DictReader(stream))


def read_panels(directory):
    a = read_rows(directory / "figure-5-panel-a.csv")
    rms = {
        r["application"]: (float(r["rms_change_same_unit"]), float(r["rms_change_same_percentile"]))
        for r in a
    }
    if set(rms) != set(APPS) or len(a) != 3:
        raise ValueError("Figure 5 panel A needs one RMS pair for each application.")
    rows = read_rows(directory / "figure-5-panel-b.csv")
    profiles = {}
    for app in APPS:
        group = [r for r in rows if r["app"] == app and r["block"] == "full"]
        profile = sorted((float(r["p"]), float(r["shift"])) for r in group)
        if [p for p, _ in profile] != [i / 200 for i in range(1, 200)]:
            raise ValueError(f"{app}: expected all 199 matching percentiles.")
        for row in group:
            signed = float(row["dual_quantile"]) - float(row["gamma_quantile"])
            if not math.isclose(signed, float(row["shift"]), rel_tol=0, abs_tol=1e-12):
                raise ValueError(f"{app}: percentile change must be Dual-Anchor minus Gamma.")
        if any(not -0.093 < x < 0.081 for _, x in profile):
            raise ValueError(f"{app}: a percentile change falls outside the figure axis.")
        profiles[app] = profile
    return rms, profiles


def draw(fn, rms, profiles, split):
    for font, style in (("Lato", "Regular"), ("LatoBold", "Bold")):
        registerFont(TTFont(font, lato_font(style)))
    c = Canvas(str(fn), pagesize=(WIDTH, HEIGHT), invariant=True)
    c.setAuthor("JoonHo Lee (jlee296@ua.edu)")
    c.setCreator("ReportLab")
    c.setTitle("Figure 5. Prior sensitivity in three applications (option 1)")

    def text(x, y, s, pt, *, bold=False, col=INK, align="left"):
        font = "LatoBold" if bold else "Lato"
        width = stringWidth(s, font, pt)
        start = {"left": x, "center": x - width / 2, "right": x - width}[align]
        assert start >= 0 and start + width <= WIDTH and pt * 0.5 <= y <= HEIGHT - pt * 0.4, (
            s,
            x,
            y,
            width,
        )
        c.setFillColor(HexColor(col))
        c.setFont(font, pt)
        c.drawString(start, HEIGHT - y - pt * 0.33, s)

    def seg(x1, y1, x2, y2, col, w):
        c.setStrokeColor(HexColor(col))
        c.setLineWidth(w)
        c.setLineCap(0)
        c.line(x1, HEIGHT - y1, x2, HEIGHT - y2)

    def linepath(points, col, w, fill=False):
        p = c.beginPath()
        for i, (x, y) in enumerate(points):
            if i == 0:
                p.moveTo(x, HEIGHT - y)
            else:
                p.lineTo(x, HEIGHT - y)
        if fill:
            p.close()
            c.setFillColor(HexColor(col))
            c.drawPath(p, fill=1, stroke=0)
        else:
            c.setStrokeColor(HexColor(col))
            c.setLineWidth(w)
            c.setLineCap(1)
            c.setLineJoin(1)
            c.drawPath(p, fill=0, stroke=1)

    def marker(x, y, diam, open_circle):
        r = diam / 2
        if open_circle:
            c.setLineWidth(1.10)
            c.setStrokeColor(HexColor(INK))
            c.setFillColor(white)
            c.circle(x, HEIGHT - y, r, stroke=1, fill=1)
        else:
            linepath([(x, y - r), (x + r, y), (x, y + r), (x - r, y)], INK, 0, True)

    def title(letter, y, word):
        size = 14
        text(11, y, letter, size, col=TEAL, bold=True)
        text(32, y, word, size, bold=True)

    text(
        10, 13, "Changing the concentration prior from Gamma(1, 1) to Dual-Anchor", 15.4, bold=True
    )
    seg(10, 26, 530, 26, INK, 0.9)
    title("A", 41, "How far did the estimates move?")
    text(
        10,
        59,
        "Root mean square (RMS) difference between priors, in the units shown for each application.",
        11.0,
        col=MUTED,
    )
    head = 11.6
    note = 10.5
    text(10, 84, "Application", head, bold=True)
    for x, title_a, sub in [
        (205, "Individual estimates", "Posterior means for the same unit"),
        (368, "Distribution among these units", "Effect values at every percentile"),
    ]:
        text(x, 80, title_a, head, bold=True)
        text(x, 95, sub, note, col=MUTED)
    starts = (205, 368)
    ends = (337, 514)
    track = 87
    seg(10, 105, 530, 105, HAIR, 0.7)
    for i, app in enumerate(APPS):
        y = 119 + 27 * i
        text(10, y - 4, NAMES[i], 11.7, bold=True)
        text(10, y + 9, SAMPLES[i] + " · " + UNITS[i][0].lower() + UNITS[i][1:], 10.3, col=MUTED)
        for x, end, val in zip(starts, ends, rms[app]):
            seg(x, y, x + track, y, GRID, 2.45)
            seg(x, y, x + track * val / 0.02, y, SLATE, 3.9)
            text(end, y, f"{val:.4f}", 12, bold=True, align="right")
    seg(10, 194, 530, 194, HAIR, 0.85)
    title("B", 208, "Where in the distribution did the estimates move?")
    text(
        10,
        227,
        "Each curve shows Dual-Anchor minus Gamma(1, 1) at matching percentiles.",
        10.9,
        col=MUTED,
    )
    text(
        10,
        241,
        "Above zero, the estimated effect or ability is higher under Dual-Anchor.",
        10.9,
        col=MUTED,
    )
    centers = (0, 177, 354)
    tick = 10.35
    circle = 6.1
    diamond = 7.0
    for idx, (app, offset) in enumerate(zip(APPS, centers)):
        origin = 10 + offset
        x_min, x_max = origin + 31.5, origin + 163
        top, bottom = (291, 366.0)
        X = lambda p: x_min + (x_max - x_min) * p
        Y = lambda v: bottom - (v + 0.093) * (bottom - top) / 0.174
        text(origin, 260, NAMES[idx], 12.5, bold=True)
        text(origin, 275, UNITS[idx], 10.65, col=MUTED)
        z = Y(0)
        pts = profiles[app]
        for sign, shade in [(1, POS), (-1, NEG)]:
            area = [(X(p), Y(sign * max(0, sign * v))) for p, v in pts]
            linepath(area + [(area[-1][0], z), (area[0][0], z)], shade, 0, True)
        for v, label in [(0.08, "+0.08"), (0, "0"), (-0.08, "−0.08")]:
            seg(x_min, Y(v), x_max, Y(v), SLATE if v == 0 else GRID, 1.05 if v == 0 else 0.6)
            text(x_min - 4, Y(v), label, tick, col=MUTED, align="right")
        linepath([(X(p), Y(v)) for p, v in pts], INK, 1.65)
        values = dict(pts)
        for rank, dia in [(0.5, circle), (0.99, diamond)]:
            marker(X(rank), Y(values[rank]), dia, rank == 0.5)
        for p, lab in [(0.01, "1"), (0.5, "50"), (0.99, "99")]:
            seg(X(p), bottom, X(p), bottom + 3, SLATE, 0.75)
            text(X(p), bottom + 11.3, lab, tick, col=MUTED, align="center")
        for rank, lab, y, dia in [
            (0.5, "Median shift", 406, circle),
            (0.99, "99th-percentile shift", 423, diamond),
        ]:
            marker(origin + 4, y, dia, rank == 0.5)
            val = f"{values[rank]:+.3f}".replace("-", "−")
            bold = rank == 0.99
            text(origin + 14, y, lab + " " + val, 11.15, bold=bold)
    text(WIDTH / 2, 391, "Percentile among the observed units", 10.5, col=MUTED, align="center")
    seg(10, 433.5, 530, 433.5, HAIR, 0.7)
    foot_pt = 10.15
    text(
        10,
        442,
        f"For vocabulary, the all-percentile RMS change of {rms['rasch'][1]:.4f} logits was close to the within-prior",
        foot_pt,
        col=MUTED,
    )
    text(
        10,
        454,
        f"split-chain differences of {split[0]:.4f} and {split[1]:.4f} logits.",
        foot_pt,
        col=MUTED,
    )
    c.showPage()
    c.save()
    subprocess.run(
        ["pdftocairo", "-png", "-singlefile", "-r", "175", str(fn), str(fn.with_suffix(""))],
        check=True,
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("frame_dir", type=Path)
    parser.add_argument("output_dir", type=Path)
    args = parser.parse_args()
    rms, profiles = read_panels(args.frame_dir)
    splits = [
        r
        for r in read_rows(args.output_dir / "osm-table-H.18.csv")
        if r["app"] == "rasch" and r["metric"] == "w2"
    ]
    if len(splits) != 1:
        raise ValueError("Expected the vocabulary split-chain comparison.")
    split = [float(splits[0][k]) for k in ("within_gamma", "within_dual")]
    args.output_dir.mkdir(parents=True, exist_ok=True)
    draw(args.output_dir / "main-figure-5.pdf", rms, profiles, split)
