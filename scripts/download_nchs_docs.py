#!/usr/bin/env python3
"""
Download NCHS natality documentation PDFs and convert to markdown using pdfplumber.
PDFs are downloaded temporarily, converted to markdown, then deleted.
"""

import os
import sys
import tempfile
from pathlib import Path

import pdfplumber
import requests

# Flush stdout for real-time output
sys.stdout.reconfigure(line_buffering=True)


OUTPUT_DIR = Path("/mnt/c/Users/skylo/git/artemis/data/raw/nchs/documentation")
BASE_URL = "https://data.nber.org/nvss/natality/inputs/pdf"

# PDF file patterns by year
# Structure: (year_to_download_from, filename, output_name)
PDF_FILES = [
    # Combined year files
    (1968, "Nat1968doc.pdf", "Nat1968doc"),
    (1969, "Nat1969-71doc.pdf", "Nat1969-71doc"),
    (1972, "Nat1972-77doc.pdf", "Nat1972-77doc"),
]

# Individual year files 1978-2004
for year in range(1978, 2005):
    PDF_FILES.append((year, f"Nat{year}doc.pdf", f"Nat{year}doc"))

# User guides 2005-2023
for year in range(2005, 2024):
    PDF_FILES.append((year, f"UserGuide{year}.pdf", f"UserGuide{year}"))


def download_pdf(year: int, filename: str) -> bytes | None:
    """Download a PDF file and return its content."""
    url = f"{BASE_URL}/{year}/{filename}"
    print(f"  Downloading {filename}...")

    try:
        response = requests.get(url, timeout=300)
        response.raise_for_status()
        return response.content
    except requests.RequestException as e:
        print(f"  [FAIL] Could not download: {e}")
        return None


def pdf_to_markdown(pdf_content: bytes, output_name: str, url: str) -> str:
    """Convert PDF content to markdown using pdfplumber with layout preservation."""
    lines = [
        f"# NCHS Natality Documentation - {output_name}",
        "",
        f"Source: {url}",
        "",
        "---",
        "",
    ]

    with tempfile.NamedTemporaryFile(suffix=".pdf", delete=False) as tmp:
        tmp.write(pdf_content)
        tmp_path = tmp.name

    try:
        with pdfplumber.open(tmp_path) as pdf:
            for i, page in enumerate(pdf.pages, 1):
                # Use layout=True to preserve horizontal spacing/structure
                # x_tolerance and y_tolerance help with character grouping
                text = page.extract_text(
                    layout=True,
                    x_tolerance=3,
                    y_tolerance=3,
                )

                if text:
                    # Wrap in code block to preserve spacing in markdown
                    lines.append(f"<!-- Page {i} -->")
                    lines.append("```")
                    lines.append(text)
                    lines.append("```")
                    lines.append("")

                # Progress indicator for large PDFs
                if i % 50 == 0:
                    print(f"    Processed {i}/{len(pdf.pages)} pages...")
    finally:
        os.unlink(tmp_path)

    return "\n".join(lines)


def process_pdf(year: int, filename: str, output_name: str) -> bool:
    """Download and convert a single PDF to markdown."""
    output_path = OUTPUT_DIR / f"{output_name}.md"

    # Skip if already exists
    if output_path.exists():
        print(f"  [SKIP] {output_name}.md already exists")
        return True

    # Download PDF
    url = f"{BASE_URL}/{year}/{filename}"
    pdf_content = download_pdf(year, filename)
    if pdf_content is None:
        return False

    # Convert to markdown
    print(f"  Converting to markdown...")
    try:
        markdown = pdf_to_markdown(pdf_content, output_name, url)
    except Exception as e:
        print(f"  [FAIL] Could not convert: {e}")
        return False

    # Write output
    output_path.write_text(markdown, encoding="utf-8")
    print(f"  [OK] Created {output_name}.md")
    return True


def check_for_addendums():
    """Check for and download any addendum files."""
    print("\n=== Checking for addendums ===")

    for year in range(2005, 2024):
        filename = f"UserGuide{year}_Addendum.pdf"
        url = f"{BASE_URL}/{year}/{filename}"

        try:
            # Quick HEAD request to check if file exists
            response = requests.head(url, timeout=10)
            if response.status_code == 200:
                print(f"Year {year} (Addendum):")
                process_pdf(year, filename, f"UserGuide{year}_Addendum")
        except requests.RequestException:
            pass


def main():
    # Ensure output directory exists
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    success_count = 0
    skip_count = 0
    fail_count = 0

    print("=== Downloading NCHS natality documentation ===\n")

    for year, filename, output_name in PDF_FILES:
        print(f"Year {year if '-' not in output_name else output_name.replace('Nat', '').replace('doc', '')}:")

        output_path = OUTPUT_DIR / f"{output_name}.md"
        if output_path.exists():
            print(f"  [SKIP] {output_name}.md already exists")
            skip_count += 1
            continue

        if process_pdf(year, filename, output_name):
            success_count += 1
        else:
            fail_count += 1

    # Check for addendums
    check_for_addendums()

    print("\n=== Complete ===")
    print(f"New files created: {success_count}")
    print(f"Skipped (already exist): {skip_count}")
    print(f"Failed: {fail_count}")
    print(f"\nDocumentation files saved to: {OUTPUT_DIR}")
    print(f"Total markdown files: {len(list(OUTPUT_DIR.glob('*.md')))}")


if __name__ == "__main__":
    main()
