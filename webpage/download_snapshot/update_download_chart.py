#!/usr/bin/env python3
"""Update the chart built from from_rokcy snapshots and current GitHub totals."""

import argparse
import csv
import json
import math
import os
import re
import subprocess
import urllib.error
import urllib.request
from datetime import datetime, timezone
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DATA_FILE = Path(__file__).with_name("download_snapshots.csv")
OUTPUTS = [
    ROOT / "images" / "release_downloads.png",
    ROOT / "_site" / "images" / "release_downloads.png",
]
FIELDS = ["checkpoint", "snapshot_date", "version", "downloads"]
COLORS = [
"#155590",
"#51999F",
"#bbe1f8",
"#60b0b7",
"#BFDFD2",
"#7BCOCD"
]
def checkpoint_value(value):
    if not re.fullmatch(r"\d{4}-(06|12)", value):
        raise argparse.ArgumentTypeError("use YYYY-06 or YYYY-12")
    return value


def current_checkpoint(now):
    return f"{now.year}-{now.month:02d}" if now.month in (6, 12) else None


def fetch_downloads():
    totals = {}
    headers = {
        "Accept": "application/vnd.github+json",
        "User-Agent": "gcam-china-download-chart",
        "X-GitHub-Api-Version": "2022-11-28",
    }
    token = os.getenv("GITHUB_TOKEN") or os.getenv("GH_TOKEN")
    if token:
        headers["Authorization"] = f"Bearer {token}"

    page = 1
    while True:
        url = (
            "https://api.github.com/repos/umd-cgs/gcam-china/releases"
            f"?per_page=100&page={page}"
        )
        try:
            request = urllib.request.Request(url, headers=headers)
            with urllib.request.urlopen(request, timeout=30) as response:
                releases = json.load(response)
        except urllib.error.HTTPError as error:
            if error.code == 403:
                raise RuntimeError("GitHub API rate limit reached; kept existing data")
            raise

        for release in releases:
            release_label = f"{release.get('tag_name', '')} {release.get('name', '')}"
            match = re.search(r"\bv(\d+(?:\.\d+)*)\b", release_label, re.IGNORECASE)
            if not match:
                continue
            version = f"v{match.group(1).split('.')[0]}"
            for asset in release.get("assets", []):
                totals[version] = totals.get(version, 0) + int(asset["download_count"])
        if len(releases) < 100:
            return totals
        page += 1


def read_rows():
    with DATA_FILE.open(newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def update_rows(rows, checkpoint):
    totals = fetch_downloads()
    snapshot_date = datetime.now(timezone.utc).date().isoformat()
    new_rows = [
        {
            "checkpoint": checkpoint,
            "snapshot_date": snapshot_date,
            "version": version,
            "downloads": str(downloads),
        }
        for version, downloads in totals.items()
    ]
    rows = [item for item in rows if item["checkpoint"] != checkpoint] + new_rows
    rows.sort(key=lambda item: (item["checkpoint"], version_key(item["version"])))
    with DATA_FILE.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=FIELDS)
        writer.writeheader()
        writer.writerows(rows)
    summary = ", ".join(f"{version}={totals[version]}" for version in sorted(totals, key=version_key))
    print(f"Updated {checkpoint}: {summary}")
    return rows


def version_key(version):
    return tuple(int(part) for part in version.removeprefix("v").split("."))


def render(rows):
    checkpoints = sorted({row["checkpoint"] for row in rows})
    versions = sorted({row["version"] for row in rows}, key=version_key)
    values = {
        (row["checkpoint"], row["version"]): int(row["downloads"])
        for row in rows
    }
    totals = [sum(values.get((checkpoint, version), 0) for version in versions) for checkpoint in checkpoints]
    width, height = 900, 520
    left, right, top, bottom = 90, 25, 45, 60
    plot_width, plot_height = width - left - right, height - top - bottom
    ymax = max(1000, (math.ceil(max(totals) / 500) + 1) * 500)
    centers = [left + plot_width * (i + 0.5) / len(checkpoints) for i in range(len(checkpoints))]
    bar_width = min(155, plot_width / len(checkpoints) * 0.62)

    def y(value):
        return top + plot_height * (1 - value / ymax)

    svg = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" viewBox="0 0 {width} {height}" role="img" aria-labelledby="title desc">',
        '<title id="title">GCAM-China cumulative release downloads by half-year</title>',
        '<desc id="desc">Half-year stacked bars split by GCAM-China model version.</desc>',
        f'<rect width="{width}" height="{height}" fill="white"/>',
        '<g font-family="Arial, Helvetica, sans-serif">',
    ]
    for tick in range(0, ymax + 1, 500):
        yy = y(tick)
        svg += [
            f'<line x1="{left}" y1="{yy:.1f}" x2="{width-right}" y2="{yy:.1f}" stroke="#d8dde1"/>',
            f'<text x="{left-14}" y="{yy+7:.1f}" text-anchor="end" font-size="20" fill="#59636a">{tick:,}</text>',
        ]
    svg.append(
        f'<line x1="{left}" y1="{y(0):.1f}" x2="{width-right}" y2="{y(0):.1f}" stroke="#24343e" stroke-width="2"/>'
    )

    legend_x, legend_y = left + 18, top + 16
    svg.append(
        f'<rect x="{legend_x-10}" y="{legend_y-10}" width="125" height="{len(versions)*34+18}" rx="5" fill="white" opacity="0.72"/>'
    )
    for index, version in enumerate(versions):
        item_y = legend_y + index * 34
        color = COLORS[index % len(COLORS)]
        svg += [
            f'<rect x="{legend_x}" y="{item_y}" width="38" height="18" fill="{color}" opacity="0.99"/>',
            f'<text x="{legend_x+50}" y="{item_y+17}" font-size="22" fill="#26343d">{version}</text>',
        ]

    for center, checkpoint, total in zip(centers, checkpoints, totals):
        x = center - bar_width / 2
        cumulative = 0
        for index, version in enumerate(versions):
            value = values.get((checkpoint, version), 0)
            if not value:
                continue
            bottom_y = y(cumulative)
            cumulative += value
            top_y = y(cumulative)
            svg.append(
                f'<rect x="{x:.1f}" y="{top_y:.1f}" width="{bar_width:.1f}" height="{bottom_y-top_y:.1f}" fill="{COLORS[index % len(COLORS)]}" opacity="0.99"/>'
            )
        svg += [
            f'<text x="{center:.1f}" y="{y(total)-10:.1f}" text-anchor="middle" font-size="21" font-weight="600" fill="#26343d">{total:,}</text>',
            f'<text x="{center:.1f}" y="{y(0)+32:.1f}" text-anchor="middle" font-size="20" fill="#26343d">{checkpoint}</text>',
        ]

    svg.append('</g></svg>')
    content = ("\n".join(svg) + "\n").encode()
    for output in OUTPUTS:
        subprocess.run(
            ["rsvg-convert", "--format", "png", "--output", str(output)],
            input=content,
            check=True,
        )
        print(f"Rendered {output.relative_to(ROOT)}")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--checkpoint", type=checkpoint_value)
    args = parser.parse_args()

    rows = read_rows()
    checkpoint = args.checkpoint or current_checkpoint(datetime.now(timezone.utc))
    if checkpoint:
        try:
            rows = update_rows(rows, checkpoint)
        except RuntimeError as error:
            print(f"Warning: {error}")
    else:
        print("No half-year ends this month; redrawing existing data only")
    render(rows)


if __name__ == "__main__":
    main()
