# GCAM-China download snapshots

This folder contains the files used to update the release-download chart:

- `download_snapshots.csv`: saved cumulative download totals by model version.
- `update_download_chart.py`: updates a half-year snapshot and redraws the chart.

The historical totals were consolidated from the spreadsheets in `from_rokcy`. Each version total includes all release-package assets for that major model version, so releases such as v7 and v7.1 are combined as v7. GitHub's Releases API only provides current cumulative download counts, so the CSV must be kept to preserve historical snapshots.

Run the updater with:

```bash
/usr/bin/python3 webpage/download_snapshot/update_download_chart.py
```

In June or December, the script fetches the current totals and saves the corresponding half-year checkpoint. In other months, it redraws the chart from the saved CSV without adding a new checkpoint. To record a specific half-year checkpoint manually, run:

```bash
/usr/bin/python3 webpage/download_snapshot/update_download_chart.py --checkpoint YYYY-06
```

The script uses `rsvg-convert` to write the chart directly as PNG. The generated chart is saved to `webpage/images/release_downloads.png` and copied to `webpage/_site/images/release_downloads.png` for the built site.
