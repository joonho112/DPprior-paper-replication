#!/usr/bin/env python3
# Author: JoonHo Lee (jlee296@ua.edu)
"""Rebuild the reviewed distribution in a fresh directory, with measured isolation."""
import argparse
import csv
import hashlib
import json
import os
from pathlib import Path
import platform
import shlex
import shutil
import socket
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
LEDGER = Path("provenance/release-allowlist.csv")


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def snapshot(destination):
    with (ROOT / LEDGER).open() as f:
        rows = list(csv.DictReader(f))
    reviewed = {}
    for row in rows:
        name = row["path"]
        p = Path(name)
        if p.is_absolute() or ".." in p.parts or name in reviewed or p == LEDGER:
            raise ValueError("Invalid or repeated allowlist path: " + name)
        source = ROOT / p
        if not source.is_file() or source.resolve() != source or digest(source) != row["sha256"]:
            raise ValueError("Unreviewed source: " + name)
        target = destination / p
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, target)
        reviewed[name] = row["sha256"]
    shutil.copy2(ROOT / LEDGER, destination / LEDGER)
    reviewed[str(LEDGER)] = digest(ROOT / LEDGER)
    actual = {p.relative_to(destination).as_posix(): digest(p) for p in destination.rglob("*") if p.is_file()}
    if reviewed != actual or any((destination / p).exists() for p in ("outputs", "data-derived", ".venv", ".library")):
        raise ValueError("Clean snapshot differs from the reviewed distribution")
    return reviewed


def cached_r(project, library):
    with (project / "provenance/dependency-closure.csv").open() as f:
        pins = [r for r in csv.DictReader(f) if "rebuild" in r["profiles"].split(";") and r["priority"] != "base"]
    names = [r["package"] for r in pins]
    expression = ('.script_dir<-getwd();source("common/R/bootstrap.R");for(p in c(' +
                  ",".join(json.dumps(p) for p in names) +
                  '))cat(p,"\\t",find.package(p),"\\n",sep="")')
    output = subprocess.check_output(["Rscript", "--vanilla", "-e", expression], cwd=ROOT, text=True)
    locations = dict(row.strip().split("\t") for row in output.splitlines())
    for name in names:
        shutil.copytree(locations[name], library / name)
    return names


def choose_python(fresh, requested):
    if requested:
        candidate = shutil.which(requested) or requested
    elif (ROOT / ".venv/bin/python3").is_file():
        candidate = str(ROOT / ".venv/bin/python3")
    else:
        candidate = (shutil.which("python3.13") or shutil.which("python3.12") or sys.executable) if fresh else sys.executable
    base = subprocess.check_output([candidate, "-c", "import sys; print(sys._base_executable)"], text=True).strip()
    # Apple's framework Python sometimes reports the venv symlink itself here.
    # The base interpreter must be executable even when the original checkout is hidden.
    return candidate, str(Path(base).resolve())


def cached_python(source, target):
    # Copy only the distribution files for the three explicit, versioned packages.
    # Entry points and bytecode can contain installation paths and are unnecessary for plotting.
    get_files = """import importlib.metadata as m,json
packages = {}
for name in ("reportlab", "pillow", "charset-normalizer"):
    d = m.distribution(name)
    packages[name] = dict(version=d.version, files={str(f):str(d.locate_file(f)) for f in d.files
        if not f.is_absolute() and ".." not in f.parts and "__pycache__" not in f.parts and f.suffix != ".pyc"})
print(json.dumps(packages))"""
    packages = json.loads(subprocess.check_output([source, "-c", get_files], text=True))
    destination = Path(subprocess.check_output([target, "-c", "import sysconfig;print(sysconfig.get_path('purelib'))"], text=True).strip())
    for metadata in packages.values():
        for name, path in metadata["files"].items():
            dest = destination / name
            dest.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(path, dest)
    return {k: p["version"] for k, p in packages.items()}


def r_runtime_layout():
    # The standard R installation can also hold hundreds of locally installed packages.
    # Make only the shipped base and recommended packages available outside .library.
    expression = ('lib<-normalizePath(.Library);cat(lib,"\\n",sep="");'
                  'x<-installed.packages(lib.loc=lib,priority=c("base","recommended"));cat(rownames(x),sep="\\n")')
    lines = subprocess.check_output(["Rscript", "--vanilla", "-e", expression], text=True).splitlines()
    library = Path(lines[0]).resolve()
    shipped = sorted(lines[1:])
    outside = sorted(p.name for p in library.iterdir() if (p/"DESCRIPTION").is_file() and p.name not in shipped)
    return library, shipped, outside


def sandbox(temp, home, allow_network, allow_unsandboxed, r_layout):
    library, shipped, incidental = r_layout
    hidden = [home]
    if not ROOT.is_relative_to(home):
        hidden.append(ROOT)
    if platform.system() == "Darwin" and shutil.which("sandbox-exec"):
        policy = "(version 1)\n(allow default)\n"
        if not allow_network:
            policy += "(deny network*)\n"
        for src in hidden:
            policy += "(deny file-read* file-write* (subpath " + json.dumps(str(src)) + "))\n"
        if incidental:
            allowed = " ".join("(subpath " + json.dumps(str(library/p)) + ")" for p in shipped)
            policy += ("(deny file-read* (require-all (subpath " + json.dumps(str(library)) +
                       ") (require-not (literal " + json.dumps(str(library)) +
                       ")) (require-not (require-any " + allowed + "))))\n")
        path = temp / ("install.sb" if allow_network else "offline.sb")
        path.write_text(policy)
        return ["sandbox-exec", "-f", str(path)], "macOS Seatbelt"
    if platform.system() == "Linux" and shutil.which("bwrap"):
        prefix = ["bwrap", "--ro-bind", "/", "/", "--dev", "/dev", "--proc", "/proc", "--bind", str(temp), str(temp)]
        for src in hidden:
            prefix.extend(["--tmpfs", str(src)])
        if incidental:
            prefix.extend(["--tmpfs", str(library)])
            for name in shipped:
                path = str(library / name)
                prefix.extend(["--ro-bind", path, path])
        if not allow_network:
            prefix.append("--unshare-net")
        return prefix + ["--"], "bubblewrap"
    if not allow_unsandboxed:
        raise RuntimeError("Install bubblewrap on Linux or use sandbox-exec on macOS. --allow-unsandboxed records an unconfined test.")
    return [], "none"


def check_denials(prefix, cwd, env):
    # Establish that the file and a TCP listener really exist before testing the child.
    marker = ROOT / ".replication-root"
    marker.stat()
    original_home = Path.home().resolve()
    home_entry = marker if marker.is_relative_to(original_home) else next(original_home.iterdir())
    home_entry.stat()
    with socket.socket() as server:
        server.bind(("127.0.0.1", 0))
        server.listen(2)
        port = server.getsockname()[1]
        with socket.create_connection(("127.0.0.1", port), timeout=2):
            pass
        probe = """import errno,pathlib,socket,sys
for name in sys.argv[1:3]:
    try: pathlib.Path(name).stat()
    except (FileNotFoundError,PermissionError): pass
    else: raise SystemExit("Source or original home remained visible")
try: socket.create_connection(("127.0.0.1",int(sys.argv[3])),timeout=2)
except OSError as exc:
    if exc.errno not in (errno.EPERM,errno.EACCES,errno.ENETUNREACH,errno.ECONNREFUSED):
        raise
else: raise SystemExit("Original TCP listener remained accessible")
"""
        subprocess.run(prefix + ["python3", "-c", probe, str(marker), str(home_entry), str(port)], cwd=cwd, env=env, check=True)


def measured(cmd, prefix, cwd, env, path):
    begin = time.monotonic()
    with path.open("w") as log:
        proc = subprocess.Popen(prefix + cmd, cwd=cwd, env=env, stdout=log, stderr=subprocess.STDOUT)
        _, status, usage = os.wait4(proc.pid, 0)
        proc.returncode = os.waitstatus_to_exitcode(status)
    rss = usage.ru_maxrss if platform.system() == "Darwin" else usage.ru_maxrss * 1024
    result = dict(command=shlex.join(cmd), returncode=proc.returncode, elapsed_seconds=round(time.monotonic()-begin, 3),
                  peak_rss_bytes=int(rss), log=path.name)
    print(json.dumps(result), flush=True)
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--execute", action="store_true")
    parser.add_argument("--fresh-install", action="store_true", help="install locked R packages and wheels into empty project libraries before offline execution")
    parser.add_argument("--python", help="Python interpreter for the new environment")
    parser.add_argument("--allow-unsandboxed", action="store_true", help="allow a host lacking supported OS isolation")
    args = parser.parse_args()
    if not args.execute:
        print("Add --execute for the isolated cached rebuild. Add --fresh-install for first-time installation into empty libraries.")
        return
    began = time.monotonic()
    # R refuses to start when its TMPDIR contains whitespace. Keep the R
    # scratch path simple and deliberately keep spaces in the checkout itself.
    base_tmp = Path(tempfile.gettempdir())
    if any(ch.isspace() for ch in str(base_tmp)):
        base_tmp = Path("/tmp")
    temp = Path(tempfile.mkdtemp(prefix="dpprior-cold-", dir=base_tmp)).resolve()
    project = temp / "DPprior public copy"
    project.mkdir()
    print("Working directory:", temp, flush=True)
    home = temp / "isolated-home"
    home.mkdir()
    reviewed = snapshot(project)
    file_manifest = temp / "reviewed-files.json"
    file_manifest.write_text(json.dumps(reviewed, sort_keys=True, indent=2) + "\n")
    mode = "fresh" if args.fresh_install else "cached"
    result = dict(schema="public-cold-start/2", mode=mode, passed=False, workspace=str(temp),
                  reviewed_files=len(reviewed), ledger_sha256=reviewed[str(LEDGER)], input_snapshot_sha256=digest(file_manifest),
                  installation_steps=[], execution_steps=[], platform=platform.platform(), personal_profiles_disabled=True)
    destination = ROOT / "outputs/verification" / ("cold_start_" + mode + ".json")
    destination.parent.mkdir(parents=True, exist_ok=True)
    library = project / ".library"
    library.mkdir()
    tmpfiles = temp / "scratch"
    tmpfiles.mkdir()
    env = os.environ.copy()
    for key in ("DPP_PYTHON", "DPP_LATO_DIR", "V3_ROOT", "R_PROFILE", "R_ENVIRON", "R_LIBS", "PYTHONPATH", "PYTHONHOME", "QUARTO_PYTHON", "VIRTUAL_ENV"):
        env.pop(key, None)
    env.update(HOME=str(home), TMPDIR=str(tmpfiles), R_USER=str(home), R_LIBS_USER=str(library),
               R_LIBS_SITE=str(temp / "empty-site"), R_PROFILE_USER=os.devnull, R_ENVIRON_USER=os.devnull,
               PYTHONNOUSERSITE="1", PIP_NO_CACHE_DIR="1", PIP_DISABLE_PIP_VERSION_CHECK="1",
               XDG_CACHE_HOME=str(home / ".cache"), XDG_CONFIG_HOME=str(home / ".config"),
               TEXMFVAR=str(home / "tex-var"), TEXMFCONFIG=str(home / "tex-config"))
    try:
        r_layout = r_runtime_layout()
        offline, isolation = sandbox(temp, Path.home().resolve(), False, args.allow_unsandboxed, r_layout)
        install, _ = sandbox(temp, Path.home().resolve(), True, args.allow_unsandboxed, r_layout)
        result["isolation"] = isolation
        result["network_denied_during_rebuild"] = False
        result["source_checkout_denied_during_rebuild"] = False
        result["original_home_denied_during_rebuild"] = False
        result["shipped_r_packages"] = r_layout[1]
        result["incidental_global_r_packages_hidden"] = 0
        if offline:
            check_denials(offline, project, env)
            if r_layout[2]:
                sample = json.dumps(r_layout[2][0])
                probe = ('stopifnot(file.exists(file.path(.Library,"base","DESCRIPTION")),'
                         '!file.exists(file.path(.Library,' + sample + ',"DESCRIPTION")))')
                subprocess.run(offline + ["Rscript", "--vanilla", "-e", probe], cwd=project, env=env, check=True)
                result["incidental_global_r_packages_hidden"] = len(r_layout[2])
            result.update(network_denied_during_rebuild=True, source_checkout_denied_during_rebuild=True,
                          original_home_denied_during_rebuild=True)
        current_python, base_python = choose_python(args.fresh_install, args.python)
        python = project / ".venv/bin/python3"
        options = [] if args.fresh_install else ["--without-pip"]
        steps = result["installation_steps"]
        cmd = [base_python, "-m", "venv", *options, str(project / ".venv")]
        steps.append(measured(cmd, install, project, env, temp / "01-venv.log"))
        if steps[-1]["returncode"]:
            raise RuntimeError("Python venv creation failed")
        if args.fresh_install:
            env.setdefault("MAKEFLAGS", "-j4")
            for n, cmd in enumerate(([str(python), "-m", "pip", "install", "-r", "common/figure-requirements.txt"],
                                     ["Rscript", "--vanilla", "common/install_dependencies.R", "--profile", "rebuild", "--execute", "--prefer-binaries"]), 2):
                steps.append(measured(cmd, install, project, env, temp / f"{n:02d}-install.log"))
                if steps[-1]["returncode"]:
                    raise RuntimeError("Package installation failed; see " + steps[-1]["log"])
        else:
            result["copied_r_packages"] = cached_r(project, library)
            result["copied_python_packages"] = cached_python(current_python, str(python))
        result["installed_r_packages"] = sorted(p.name for p in library.iterdir() if p.is_dir())
        commands = [["quarto", "render", "docs"], ["python3", "verification/check_book.py"],
                    ["Rscript", "--vanilla", "00_setup.R", "--profile", "rebuild"],
                    ["Rscript", "--vanilla", "exhibits/00_build_all.R"], ["Rscript", "--vanilla", "verify_reproduction.R"],
                    ["python3", "docs/sync_assets.py", "--execute"], ["quarto", "render", "docs"],
                    ["python3", "verification/check_book.py", "--current"]]
        for i, cmd in enumerate(commands, 1):
            step = measured(cmd, offline, project, env, temp / f"{i:02d}-run.log")
            result["execution_steps"].append(step)
            if step["returncode"]:
                raise RuntimeError("Clean run failed; see " + step["log"])
        result["passed"] = True
    finally:
        result["elapsed_seconds"] = round(time.monotonic() - began, 3)
        for stage in ("installation", "execution"):
            result[stage + "_peak_rss_bytes"] = max((step["peak_rss_bytes"] for step in result[stage + "_steps"]), default=0)
        result["peak_rss_bytes"] = max(result["installation_peak_rss_bytes"], result["execution_peak_rss_bytes"])
        result["memory_measurement"] = "Largest OS-reported resident high-water mark among each command and its descendants"
        destination.write_text(json.dumps(result, indent=2) + "\n")
        print("Receipt:", destination, flush=True)


if __name__ == "__main__":
    main()
