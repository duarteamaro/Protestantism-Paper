"""Scrape the church finder of the Federation protestante de France.

https://www.protestants.org/trouver-une-eglise-protestante-pres-de-chez-soi/
The map widget ships all markers (~1700 churches) as one base64 JSON blob in
the page, so a single GET is enough.

    python scrape_fpf_churches.py                  # -> churches_fpf.csv / .json
    python scrape_fpf_churches.py --html page.html # parse a saved copy
"""

import argparse
import base64
import csv
import html
import json
import re
from pathlib import Path
from urllib.request import Request, urlopen

URL = "https://www.protestants.org/trouver-une-eglise-protestante-pres-de-chez-soi/"
USER_AGENT = ("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 "
              "(KHTML, like Gecko) Chrome/124.0 Safari/537.36")
BLOB_RE = re.compile(r'class="wpgmp-map-data-container"\s+data-map-id="1">\s*([A-Za-z0-9+/=]+)')

# location field -> column, and extra_fields key -> column
LOCATION = {"postal_code": "postal_code", "city": "city"}
EXTRA = {
    "complement_adresse": "complement-dadresse", "departement": "departement",
    "appartenance": "appartenance", "union": "union", "site": "site",
    "facebook": "facebook-reseaux-sociaux", "tel_fixe": "tel-fixe",
    "tel_fixe_2": "tel-fixe-ou-mobile-2", "mobile": "mobile-1", "tel_contact": "tel-1",
    "mail_1": "mail-1", "mail_2": "mail-2", "contact_role": "contact-1",
    "contact_nom": "nom-1", "contact_prenom": "prenom-1",
}
FIELDS = ["id", "title", "denomination", "address", "complement_adresse", "postal_code",
          "city", "departement", "lat", "lng", "appartenance", "union", "site", "facebook",
          "tel_fixe", "tel_fixe_2", "mobile", "tel_contact", "mail_1", "mail_2",
          "contact_role", "contact_nom", "contact_prenom"]


def clean(value):
    if value is None:
        return ""
    return re.sub(r"\s+", " ", html.unescape(str(value)).strip())


def flatten(place):
    loc = place.get("location") or {}
    extra = loc.get("extra_fields") or {}
    cats = place.get("categories") or []
    row = {
        "id": place.get("id", ""),
        "title": clean(place.get("title")),
        "denomination": clean(cats[0].get("name", "")) if cats else "",
        "address": clean(place.get("address")),
        "lat": loc.get("lat", ""),
        "lng": loc.get("lng", ""),
    }
    row.update({col: clean(loc.get(key)) for col, key in LOCATION.items()})
    row.update({col: clean(extra.get(key)) for col, key in EXTRA.items()})
    return {col: row[col] for col in FIELDS}


parser = argparse.ArgumentParser()
parser.add_argument("--out", default="churches_fpf")
parser.add_argument("--html", type=Path)
args = parser.parse_args()

if args.html:
    page = args.html.read_text(encoding="utf-8", errors="replace")
else:
    req = Request(URL, headers={"User-Agent": USER_AGENT, "Accept-Language": "fr"})
    with urlopen(req, timeout=60) as resp:
        page = resp.read().decode("utf-8", errors="replace")

blob = BLOB_RE.search(page).group(1)
data = json.loads(base64.b64decode(blob + "=" * (-len(blob) % 4)))
rows = [flatten(p) for p in data.get("places", [])]

with open(f"{args.out}.csv", "w", newline="", encoding="utf-8-sig") as f:
    writer = csv.DictWriter(f, fieldnames=FIELDS)
    writer.writeheader()
    writer.writerows(rows)
Path(f"{args.out}.json").write_text(json.dumps(rows, ensure_ascii=False, indent=2), encoding="utf-8")
