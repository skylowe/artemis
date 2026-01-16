#!/usr/bin/env python3
"""
Download NCHS mortality documentation PDFs and convert to markdown using pdfplumber.
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


OUTPUT_DIR = Path("/mnt/c/Users/skylo/git/artemis/data/raw/nchs/mortality_documentation")
BASE_URL = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Dataset_Documentation/DVS/mortality"

# PDF file patterns by year
# Structure: (filename, output_name, description)
PDF_FILES = [
    # ICD-9 era (1979-1998)
    ("dt79icd9.pdf", "Mort1979_ICD9", "1979 ICD-9 Layout"),
    ("dt80icd9.pdf", "Mort1980_ICD9", "1980 ICD-9 Layout"),
    ("dt81icd9.pdf", "Mort1981_ICD9", "1981 ICD-9 Layout"),
    ("dt82icd9.pdf", "Mort1982_ICD9", "1982 ICD-9 Layout"),
    ("dt83icd9.pdf", "Mort1983_ICD9", "1983 ICD-9 Layout"),
    ("dt84icd9.pdf", "Mort1984_ICD9", "1984 ICD-9 Layout"),
    ("dt85icd9.pdf", "Mort1985_ICD9", "1985 ICD-9 Layout"),
    ("dt86icd9.pdf", "Mort1986_ICD9", "1986 ICD-9 Layout"),
    ("dt87icd9.pdf", "Mort1987_ICD9", "1987 ICD-9 Layout"),
    ("dt88icd9.pdf", "Mort1988_ICD9", "1988 ICD-9 Layout"),
    ("dt89icd9.pdf", "Mort1989_ICD9", "1989 ICD-9 Layout"),
    ("dt90icd9.pdf", "Mort1990_ICD9", "1990 ICD-9 Layout"),
    ("dt91icd9.pdf", "Mort1991_ICD9", "1991 ICD-9 Layout"),
    ("dt92icd9.pdf", "Mort1992_ICD9", "1992 ICD-9 Layout"),
    ("dt93icd9.pdf", "Mort1993_ICD9", "1993 ICD-9 Layout"),
    ("dt94icd9.pdf", "Mort1994_ICD9", "1994 ICD-9 Layout"),
    ("dt95icd9.pdf", "Mort1995_ICD9", "1995 ICD-9 Layout"),
    ("dt96icd9.pdf", "Mort1996_ICD9", "1996 ICD-9 Layout"),

    # ICD-10 transition and interim (1999-2002)
    ("interim1999p1.pdf", "Mort1999_Interim", "1999 Interim Layout"),
    ("interim2000p1.pdf", "Mort2000_Interim", "2000 Interim Layout"),
    ("interim2001p1.pdf", "Mort2001_Interim", "2001 Interim Layout"),
    ("interim2002p1.pdf", "Mort2002_Interim", "2002 Interim Layout"),

    # Record Layout era (2003-2014)
    ("Record_Layout_2003.pdf", "Record_Layout_2003", "2003 Record Layout"),
    ("Record_Layout_2004.pdf", "Record_Layout_2004", "2004 Record Layout"),
    ("Record_Layout_2005.pdf", "Record_Layout_2005", "2005 Record Layout"),
    ("Record_Layout_2006.pdf", "Record_Layout_2006", "2006 Record Layout"),
    ("Record_Layout_2007.pdf", "Record_Layout_2007", "2007 Record Layout"),
    ("Record_Layout_2008.pdf", "Record_Layout_2008", "2008 Record Layout"),
    ("Record_Layout_2009.pdf", "Record_Layout_2009", "2009 Record Layout"),
    ("Record_Layout_2010.pdf", "Record_Layout_2010", "2010 Record Layout"),
    ("Record_Layout_2011.pdf", "Record_Layout_2011", "2011 Record Layout"),
    ("Record_Layout_2012.pdf", "Record_Layout_2012", "2012 Record Layout"),
    ("Record_Layout_2013.pdf", "Record_Layout_2013", "2013 Record Layout"),
    ("Record_Layout_2014.pdf", "Record_Layout_2014", "2014 Record Layout"),

    # Multiple Cause Record Layout era (2015-2021)
    ("Multiple_Cause_Record_Layout_2015.pdf", "Multiple_Cause_Record_Layout_2015", "2015 Multiple Cause Layout"),
    ("Multiple_Cause_Record_Layout_2016.pdf", "Multiple_Cause_Record_Layout_2016", "2016 Multiple Cause Layout"),
    ("Multiple_Cause_Record_Layout_2017.pdf", "Multiple_Cause_Record_Layout_2017", "2017 Multiple Cause Layout"),
    ("Multiple_Cause_Record_Layout_2018.pdf", "Multiple_Cause_Record_Layout_2018", "2018 Multiple Cause Layout"),
    ("Multiple_Cause_Record_Layout_2019.pdf", "Multiple_Cause_Record_Layout_2019", "2019 Multiple Cause Layout"),
    ("Multiple-Cause-Record-Layout-2020.pdf", "Multiple_Cause_Record_Layout_2020", "2020 Multiple Cause Layout"),
    ("Multiple-Cause-Record-Layout-2021.pdf", "Multiple_Cause_Record_Layout_2021", "2021 Multiple Cause Layout"),

    # Public Use Documentation (2022+)
    ("2022-Mortality-Public-Use-File-Documentation.pdf", "Mortality_Public_Use_2022", "2022 Public Use Documentation"),
    ("2023-Mortality-Public-Use-File-Documentation.pdf", "Mortality_Public_Use_2023", "2023 Public Use Documentation"),
]


def download_pdf(filename: str) -> bytes | None:
    """Download a PDF file and return its content."""
    url = f"{BASE_URL}/{filename}"
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
        f"# NCHS Mortality Documentation - {output_name}",
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


def process_pdf(filename: str, output_name: str, description: str) -> bool:
    """Download and convert a single PDF to markdown."""
    output_path = OUTPUT_DIR / f"{output_name}.md"

    # Skip if already exists
    if output_path.exists():
        print(f"  [SKIP] {output_name}.md already exists")
        return True

    # Download PDF
    url = f"{BASE_URL}/{filename}"
    pdf_content = download_pdf(filename)
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


def main():
    # Ensure output directory exists
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    success_count = 0
    skip_count = 0
    fail_count = 0

    print("=== Downloading NCHS mortality documentation ===\n")

    for filename, output_name, description in PDF_FILES:
        print(f"{description}:")

        output_path = OUTPUT_DIR / f"{output_name}.md"
        if output_path.exists():
            print(f"  [SKIP] {output_name}.md already exists")
            skip_count += 1
            continue

        if process_pdf(filename, output_name, description):
            success_count += 1
        else:
            fail_count += 1

    print("\n=== Complete ===")
    print(f"New files created: {success_count}")
    print(f"Skipped (already exist): {skip_count}")
    print(f"Failed: {fail_count}")
    print(f"\nDocumentation files saved to: {OUTPUT_DIR}")
    print(f"Total markdown files: {len(list(OUTPUT_DIR.glob('*.md')))}")


if __name__ == "__main__":
    main()
