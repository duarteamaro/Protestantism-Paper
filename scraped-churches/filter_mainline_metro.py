"""Keep the historic mainline churches (EPUdF, UEPAL) in metropolitan France.

Filters on `union` rather than on the map category, which is noisy
('lutherien' also holds Malagasy and Korean diaspora churches).
--include-methodist also keeps the UEEMF.

    python filter_mainline_metro.py   # -> churches_mainline_metro.csv / .json
"""

import argparse
import csv
import json
from pathlib import Path

parser = argparse.ArgumentParser()
parser.add_argument("--in", dest="infile", type=Path, default=Path("churches_fpf_insee.csv"))
parser.add_argument("--out", default="churches_mainline_metro")
parser.add_argument("--include-methodist", action="store_true")
args = parser.parse_args()

unions = ("EPUdF", "UEPAL") + (("UEEMF",) if args.include_methodist else ())

with args.infile.open(encoding="utf-8-sig") as f:
    reader = csv.DictReader(f)
    fields = reader.fieldnames
    rows = list(reader)

kept = [r for r in rows
        if r["union"].startswith(unions)
        and r["insee_code"] and not r["insee_code"].startswith(("97", "98"))]

with open(f"{args.out}.csv", "w", newline="", encoding="utf-8-sig") as f:
    writer = csv.DictWriter(f, fieldnames=fields)
    writer.writeheader()
    writer.writerows(kept)
Path(f"{args.out}.json").write_text(json.dumps(kept, ensure_ascii=False, indent=2), encoding="utf-8")
