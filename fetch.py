#!/usr/bin/env python3

import argparse
from datetime import date
import os
import requests
import sys


def get_cookie():
    filename = os.path.join(os.path.dirname(__file__), ".session-cookie")
    with open(filename, "r") as f:
        return f.read().strip()


def is_valid_date(year, day):
    if year < 2015:
        return False
    if day < 1 or day > 25:
        return False
    if date(year, 12, day) > date.today():
        return False
    return True


def main(argv):
    parser = argparse.ArgumentParser(description="Fetch AOC problem inputs.")
    parser.add_argument("--year", type=int)
    parser.add_argument("--day", type=int)
    parser.add_argument("--redownload", action="store_true")
    args = parser.parse_args(argv[1:])

    # Default to today, then validate.
    year = args.year or date.today().year
    day = args.day or date.today().day

    # Only download if the data doesn't already exist.
    filename = f"{year}/data/{day:02}.txt"
    if os.path.exists(filename) and not args.redownload:
        print("Already fetched input:", filename)
        return

    # Fetch the data
    print(f"Fetching input for {year}-12-{day:02}...")
    url = f"https://adventofcode.com/{year}/day/{day}/input"
    cookies = {"session": get_cookie()}
    response = requests.get(url, cookies=cookies)
    if not response.ok:
        print(f"Download failed (ERROR {response.status_code}):\n  {response.content}")
        return 1
    with open(filename, "w") as f:
        f.write(response.text[:-1])

    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
