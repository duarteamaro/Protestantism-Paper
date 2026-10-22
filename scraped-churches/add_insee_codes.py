"""Add INSEE commune codes to the FPF churches (churches_fpf.csv).

1. Reverse-geocode all points in one batch with the Base Adresse Nationale
   (nearest address).
2. Points BAN missed, matched from more than 500 m, or whose commune disagrees
   with the church's own city are re-checked with geo.api.gouv.fr
   (point in commune polygon), which wins when it answers.
3. Rows whose postal code points to another departement than the INSEE code
   have a wrong map point; they are re-geocoded from their address.

insee_source: 'ban', 'geoapi', 'address', 'ban_far' (no polygon found, keep
with care) or 'coord_postal_conflict' (unresolved).

    python add_insee_codes.py   # -> churches_fpf_insee.csv / .json
"""

import argparse
import csv
import io
import json
import sys
import time
import unicodedata
import urllib.parse
import urllib.request
import uuid
from pathlib import Path

BAN_REVERSE = "https://api-adresse.data.gouv.fr/reverse/csv/"
BAN_SEARCH = "https://api-adresse.data.gouv.fr/search/"
GEO_API = "https://geo.api.gouv.fr/communes"


def get_json(url, params):
    req = urllib.request.Request(f"{url}?{urllib.parse.urlencode(params)}",
                                 headers={"Accept": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=30) as resp:
            return json.loads(resp.read().decode("utf-8"))
    except Exception as exc:
        print(f"request failed: {params} ({exc})", file=sys.stderr)
        return None


def ban_reverse(points):
    """One multipart POST with all points -> {(lat, lng): hit}."""
    buf = io.StringIO()
    writer = csv.writer(buf)
    writer.writerow(["lat", "lon"])
    writer.writerows(points)
    boundary = uuid.uuid4().hex
    body = (f"--{boundary}\r\n"
            'Content-Disposition: form-data; name="data"; filename="points.csv"\r\n'
            "Content-Type: text/csv\r\n\r\n").encode() + buf.getvalue().encode("utf-8") \
        + f"\r\n--{boundary}--\r\n".encode()
    req = urllib.request.Request(BAN_REVERSE, data=body, headers={
        "Content-Type": f"multipart/form-data; boundary={boundary}"})
    with urllib.request.urlopen(req, timeout=300) as resp:
        text = resp.read().decode("utf-8")
    out = {}
    for rec in csv.DictReader(io.StringIO(text)):
        if rec.get("result_citycode"):
            try:
                distance = float(rec.get("result_distance") or "nan")
            except ValueError:
                distance = float("nan")
            out[(rec["lat"], rec["lon"])] = {"code": rec["result_citycode"],
                                             "city": rec.get("result_city", ""),
                                             "source": "ban", "distance_m": distance}
    return out


def geo_api(lat, lng):
    communes = get_json(GEO_API, {"lat": lat, "lon": lng, "fields": "code,nom", "format": "json"})
    if communes:
        return {"code": communes[0]["code"], "city": communes[0]["nom"], "source": "geoapi"}
    return None


def ban_search(query, dept, postcode=""):
    """Forward-geocode an address; only a hit in the expected departement counts."""
    params = {"q": query, "limit": 1}
    if postcode:
        params["postcode"] = postcode
    data = get_json(BAN_SEARCH, params)
    if not data or not data.get("features"):
        return None
    props = data["features"][0].get("properties", {})
    code = props.get("citycode", "")
    if code and dept_of_insee(code) == dept:
        return {"code": code, "city": props.get("city", ""), "source": "address"}
    return None


def norm_city(name):
    s = unicodedata.normalize("NFD", name.upper()).encode("ascii", "ignore").decode()
    for ch in ("-", "'", "’"):
        s = s.replace(ch, " ")
    return " ".join(s.replace("ST ", "SAINT ").split())


def same_city(a, b):
    a, b = norm_city(a), norm_city(b)
    return bool(a and b) and (a == b or a in b or b in a)


def dept_of_postal(postal):
    p = "".join(ch for ch in postal if ch.isdigit())
    if len(p) != 5:
        return ""
    return p[:3] if p.startswith(("97", "98")) else p[:2]


def dept_of_insee(code):
    if not code:
        return ""
    if code[:2] in ("2A", "2B"):
        return "20"
    return code[:3] if code.startswith(("97", "98")) else code[:2]


parser = argparse.ArgumentParser()
parser.add_argument("--in", dest="infile", type=Path, default=Path("churches_fpf.csv"))
parser.add_argument("--out", default="churches_fpf_insee")
args = parser.parse_args()

with args.infile.open(encoding="utf-8-sig") as f:
    rows = list(csv.DictReader(f))
points = list(dict.fromkeys((r["lat"], r["lng"]) for r in rows if r["lat"] and r["lng"]))

# 1. nearest address
resolved = ban_reverse(points)

# 2. point in polygon for doubtful points
site_cities = {}
for r in rows:
    site_cities.setdefault((r["lat"], r["lng"]), set()).add(r["city"])


def doubtful(p):
    hit = resolved.get(p)
    if hit is None or not hit["distance_m"] <= 500:
        return True
    return not any(same_city(c, hit["city"]) for c in site_cities.get(p, set()))


for lat, lng in [p for p in points if doubtful(p)]:
    hit = geo_api(lat, lng)
    if hit:
        resolved[(lat, lng)] = hit
    elif (lat, lng) in resolved:
        resolved[(lat, lng)]["source"] = "ban_far"
    time.sleep(0.15)

for r in rows:
    hit = resolved.get((r["lat"], r["lng"])) or {}
    r["insee_code"] = hit.get("code", "")
    r["insee_commune"] = hit.get("city", "")
    r["insee_source"] = hit.get("source", "")

# 3. map point and postal code in different departements: geocode the address.
# BAN does not cover the 98x collectivities, keep the polygon result there.
bad_points = [r for r in rows
              if r["insee_code"] and dept_of_postal(r["postal_code"])
              and dept_of_postal(r["postal_code"]) != dept_of_insee(r["insee_code"])
              and not r["insee_code"].startswith("98")]

for r in bad_points:
    postcode = "".join(ch for ch in r["postal_code"] if ch.isdigit())
    dept = dept_of_postal(r["postal_code"])
    street_city = " ".join(x for x in (r["address"], r["city"]) if x)
    # city names on the site are often mangled, so start with the street only
    attempts = [(r["address"], postcode), (street_city, postcode),
                (r["city"] or postcode, postcode), (street_city, "")]
    hit = None
    for query, pc in attempts:
        hit = ban_search(query, dept, pc)
        if hit:
            break
        time.sleep(0.1)
    if hit:
        r["insee_code"], r["insee_commune"], r["insee_source"] = hit["code"], hit["city"], hit["source"]
    else:
        r["insee_source"] = "coord_postal_conflict"
    time.sleep(0.15)

with open(f"{args.out}.csv", "w", newline="", encoding="utf-8-sig") as f:
    writer = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
    writer.writeheader()
    writer.writerows(rows)
Path(f"{args.out}.json").write_text(json.dumps(rows, ensure_ascii=False, indent=2), encoding="utf-8")
