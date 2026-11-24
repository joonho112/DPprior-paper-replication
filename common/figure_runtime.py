#!/usr/bin/env python3
# Author: JoonHo Lee (jlee296@ua.edu)
"""Resolve the tested rendering dependencies for the figures."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

ROOT = Path(__file__).resolve().parent.parent
FONTS = {
    "Regular": "6f6940be0835c3ddec9199e5fc42be4cbc61ebcfd58c623fdf719366253f1780",
    "Bold": "bf1b8130069b44b9148eeece35e5423bedac49777ba746615b826b8276574a7b",
}


def lato_font(style):
    """Use an exact Lato face, including the TeX Live copy on offline hosts."""
    name = "Lato-" + style + ".ttf"
    candidates = []
    if os.environ.get("DPP_LATO_DIR"):
        candidates.append(Path(os.environ["DPP_LATO_DIR"]) / name)
    for cmd in (["fc-match", "-f", "%{file}", "Lato:style=" + style],
                ["kpsewhich", name]):
        if shutil.which(cmd[0]):
            result = subprocess.run(cmd, capture_output=True, text=True)
            if result.returncode == 0 and result.stdout.strip():
                candidates.append(Path(result.stdout.strip()))
    for path in candidates:
        try:
            if hashlib.sha256(path.read_bytes()).hexdigest() == FONTS[style]:
                return str(path)
        except OSError:
            continue
    raise RuntimeError("Cannot find the pinned " + name +
                       ". Install TeX Live package lato or set DPP_LATO_DIR.")


def figure_python():
    explicit = os.environ.get("DPP_PYTHON")
    local = ROOT / ".venv" / ("Scripts/python.exe" if os.name == "nt" else "bin/python3")
    executable = explicit or (str(local) if local.is_file() else sys.executable)
    probe = ("import importlib.metadata as m,sys; "
             "print(sys.version.split()[0]); "
             "print(m.version('reportlab'),m.version('pillow'),m.version('charset-normalizer')); "
             "from reportlab.pdfbase.ttfonts import TTFont; from reportlab.pdfgen.canvas import Canvas")
    result = subprocess.run([executable, "-c", probe], capture_output=True, text=True)
    lines = result.stdout.splitlines()
    required = [s.split("==")[1] for s in (ROOT / "common/figure-requirements.txt").read_text().splitlines() if s]
    if result.returncode or len(lines) != 2 or lines[1].split() != required:
        raise RuntimeError("Figure 5 requires " + ", ".join((ROOT / "common/figure-requirements.txt").read_text().splitlines()) +
                           ". Install into .venv or set DPP_PYTHON. Interpreter checked: " + executable)
    return executable, lines[0], dict(zip(("reportlab", "pillow", "charset-normalizer"), required))


def check():
    python, version, packages = figure_python()
    programs = {}
    for name, flag in (("pdflatex", "--version"), ("pdftocairo", "-v"),
                       ("pdftoppm", "-v"), ("fc-match", "-V"), ("kpsewhich", "--version")):
        program = shutil.which(name)
        if not program:
            raise RuntimeError("Missing required rendering tool " + name)
        output = subprocess.check_output([program, flag], stderr=subprocess.STDOUT, text=True)
        programs[name] = output.splitlines()[0]
    for name in ("standalone.cls", "graphicx.sty", "tikz.sty", "helvet.sty", "amsmath.sty", "amssymb.sty"):
        if subprocess.run(["kpsewhich", name], capture_output=True).returncode:
            raise RuntimeError("Missing TeX input " + name)
    return dict(python=version, packages=packages, programs=programs,
                fonts={style: Path(lato_font(style)).name for style in FONTS})


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--python", action="store_true", help="print the pinned Python interpreter")
    args = parser.parse_args()
    try:
        print(figure_python()[0] if args.python else json.dumps(check(), indent=2))
    except (OSError, RuntimeError, subprocess.SubprocessError) as exc:
        raise SystemExit(str(exc))
