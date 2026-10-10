#!/usr/bin/env python3
"""
compare_compilers.py — for every .f90 file in a directory, compile with
gfortran and ifort, then compare behaviour:

  both fail to compile  → consistent failure, skip
  one compiles, one not → discrepancy, report
  both compile          → run both, diff stdout
                           same output  → SUCCESS
                           diff output  → report diff

Usage:
  python compare_compilers.py <directory> [options]
"""

import argparse
import difflib
import os
import subprocess
import sys
import tempfile
from pathlib import Path


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def banner(text: str) -> str:
    bar = "=" * 64
    return f"\n{bar}\n{text}\n{bar}"


def compile_file(compiler: str, src: Path, out: Path, timeout: int = 60):
    """Returns (success: bool, stderr: str)."""
    try:
        r = subprocess.run(
            [compiler, str(src), "-o", str(out)],
            capture_output=True, text=True, timeout=timeout,
        )
        return r.returncode == 0, r.stderr
    except FileNotFoundError:
        return False, f"{compiler}: command not found"
    except subprocess.TimeoutExpired:
        return False, f"{compiler}: compilation timed out"


def run_exe(exe: Path, timeout: int):
    """Returns (stdout: str, stderr: str, returncode: int, timed_out: bool)."""
    try:
        r = subprocess.run(
            [str(exe)], capture_output=True, text=True,
            input="", timeout=timeout,
        )
        return r.stdout, r.stderr, r.returncode, False
    except subprocess.TimeoutExpired:
        return "", "", -1, True


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Compile .f90 files with gfortran and ifort and diff their output."
    )
    parser.add_argument("directory", help="Directory containing .f90 files")
    parser.add_argument(
        "--gfortran", default="gfortran",
        help="gfortran executable name/path (default: gfortran)"
    )
    parser.add_argument(
        "--ifort", default="ifort",
        help="ifort executable name/path (default: ifort)"
    )
    parser.add_argument(
        "--timeout", type=int, default=10,
        help="Per-program execution timeout in seconds (default: 10)"
    )
    parser.add_argument(
        "--compile-timeout", type=int, default=60,
        help="Per-file compilation timeout in seconds (default: 60)"
    )
    args = parser.parse_args()

    src_dir = Path(args.directory)
    if not src_dir.is_dir():
        print(f"Error: '{src_dir}' is not a directory.", file=sys.stderr)
        sys.exit(1)

    sources = sorted(src_dir.glob("*.f90"))
    if not sources:
        print(f"No .f90 files found in '{src_dir}'.")
        sys.exit(0)

    n_both_fail     = 0
    n_discrepancy   = 0
    n_output_match  = 0
    n_output_mismatch = 0
    n_runtime_issue = 0

    with tempfile.TemporaryDirectory() as tmpdir:
        tmp = Path(tmpdir)

        for src in sources:
            print(banner(src.name))

            g_exe = tmp / f"{src.stem}_gfortran"
            i_exe = tmp / f"{src.stem}_ifort"

            g_ok, g_err = compile_file(args.gfortran, src, g_exe, args.compile_timeout)
            i_ok, i_err = compile_file(args.ifort,    src, i_exe, args.compile_timeout)

            # ── both failed ──────────────────────────────────────────────
            if not g_ok and not i_ok:
                print("Both compilers FAILED — consistent failure, skipping.")
                n_both_fail += 1
                continue

            # ── one succeeded, one failed ────────────────────────────────
            if g_ok != i_ok:
                winner = args.gfortran if g_ok else args.ifort
                loser  = args.ifort    if g_ok else args.gfortran
                loser_err = i_err if g_ok else g_err
                print(f"DISCREPANCY: {winner} compiled OK but {loser} failed.")
                if loser_err.strip():
                    print(f"\n{loser} stderr:\n{loser_err.rstrip()}")
                n_discrepancy += 1
                continue

            # ── both compiled: run and compare ───────────────────────────
            print(f"Both compiled. Running (timeout={args.timeout}s)...")

            g_out, g_serr, g_rc, g_timeout = run_exe(g_exe, args.timeout)
            i_out, i_serr, i_rc, i_timeout = run_exe(i_exe, args.timeout)

            if g_timeout or i_timeout:
                timed_out = []
                if g_timeout: timed_out.append(args.gfortran)
                if i_timeout: timed_out.append(args.ifort)
                print(f"RUNTIME TIMEOUT after {args.timeout}s: {', '.join(timed_out)}")
                n_runtime_issue += 1
                continue

            # Report exit codes if they differ
            if g_rc != i_rc:
                print(f"Exit codes differ: {args.gfortran}={g_rc}, {args.ifort}={i_rc}")

            if g_out == i_out:
                rc_note = "" if g_rc == i_rc else " (exit codes differ — see above)"
                print(f"Output MATCHES ({len(g_out)} chars). SUCCESS.{rc_note}")
                n_output_match += 1
            else:
                print("Output DIFFERS:")
                diff = list(difflib.unified_diff(
                    g_out.splitlines(keepends=True),
                    i_out.splitlines(keepends=True),
                    fromfile=f"{args.gfortran} stdout",
                    tofile=f"{args.ifort} stdout",
                ))
                if diff:
                    sys.stdout.writelines(diff)
                else:
                    # Non-printable difference
                    print(f"  {args.gfortran}: {repr(g_out)}")
                    print(f"  {args.ifort}:    {repr(i_out)}")
                n_output_mismatch += 1

    # ── summary ─────────────────────────────────────────────────────────────
    total    = len(sources)
    reported = n_discrepancy + n_output_match + n_output_mismatch + n_runtime_issue

    print(banner("SUMMARY"))
    print(f"  Files processed              : {total}")
    print(f"  Both failed (consistent)     : {n_both_fail}")
    print(f"  Compiler discrepancy         : {n_discrepancy}")
    print(f"  Both ran, output matches     : {n_output_match}  ← successes")
    print(f"  Both ran, output differs     : {n_output_mismatch}")
    print(f"  Runtime issue / timeout      : {n_runtime_issue}")
    print(f"\n  SUCCESSES: {n_output_match} / {total}  "
          f"({'%.0f' % (100*n_output_match/total)}%)" if total else "")


if __name__ == "__main__":
    main()
