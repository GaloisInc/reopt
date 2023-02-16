from __future__ import print_function
from dataclasses import dataclass

import os.path
import re
import subprocess

from googleapiclient.discovery import build
from googleapiclient.errors import HttpError
from typing import Optional

from . import grammatech
from .grammatech import Binary
from . import sheets


# def get_binaries(service) -> list[str]:
#     return [binary for row in get_range(service, "TODO", "A5:A") for binary in row]


@dataclass
class Residual:
    from_address: str
    to_address: str
    footprint: str
    section: Optional[str]
    symbol: Optional[str]
    explanation: str


def process_reopt_residual_line(line: str) -> Residual:
    """
    "Parses" out a residual line of the form:

    abc123 - abc456 (symbol) [section] explanation
    """
    address = "[0-9a-f]+"
    footprint = "\\s*(\\d*)B"
    section = "(?: \\[(.*)\\])?"
    symbol = "(?: \\((.*)\\))?"
    explanation = ".*"
    line_re = (
        f"({address}) - ({address}) {footprint}{section}{symbol} ?({explanation})"
    )
    # print(line_re)
    print(line)
    match = re.match(line_re, line)
    if not match:
        raise Exception(
            f"{process_reopt_residual_line.__name__}: Could not RE match {line}"
        )
    # 0 is full match, 1+ are matched groups
    return Residual(
        from_address=match.group(1),
        to_address=match.group(2),
        footprint=match.group(3),
        section=match.group(4) or None,
        symbol=match.group(5) or None,
        explanation=match.group(6),
    )

    # [match.group(i) or "-" for i in range(1, 5)]


def main():
    """
    TODO
    """

    creds = sheets.get_credentials()

    try:
        service = build("sheets", "v4", credentials=creds)

        which_binary = Binary.HELLO
        (binary_tag, binary) = (which_binary, which_binary.value)

        for (
            compiler_tag,
            optimization_tag,
            pie_tag,
            strip_tag,
            # ) in grammatech.ALL_CONFIGURATIONS:
        ) in [
            (
                grammatech.Compiler.CLANG,
                grammatech.OptimizationLevel.O0,
                grammatech.PIE.No,
                grammatech.Strip.No,
            )
        ]:
            compiler = compiler_tag.value
            optimization = optimization_tag.value
            pie = f"{'' if pie_tag.value else 'no'}pie"
            strip = f"{'' if strip_tag.value else 'no'}strip"

            filename = (
                f"x86_64.ubuntu20.{binary}.{compiler}.{optimization}.{pie}.{strip}.elf"
            )
            filepath = f"./grammatech/x86_64/results_ubuntu20/{filename}"
            if not os.path.exists(os.path.join("../", filepath)):
                print(f"Skipping non-existent {filename}")
                break

            print(f"Running reopt-explore on {filename}")

            completed = subprocess.run(
                [
                    "cabal",
                    "run",
                    "-v0",
                    "reopt:exe:reopt-explore",
                    "--",
                    "residuals",
                    "--output-for-spreadsheet",
                    "--header",
                    "./grammatech/leafnode.h",
                    filepath,
                ],
                capture_output=True,
                cwd="../",
            )
            print(f"{filename} completed with status {completed.returncode}")
            print(completed.stderr.decode("utf-8"))

            lines = completed.stdout.decode("utf-8").splitlines()

            if len(lines) < 3:
                continue

            print(lines[0])
            print(lines[1])
            print(lines[2])

            residuals_footprint = lines[0].split(" ")[1]
            explained_footprint = lines[1].split(" ")[1]
            unexplained_footprint = lines[2].split(" ")[1]

            name = f"{compiler}.{optimization}.{pie}.{strip}"
            values: list[list[int | str]] = [
                [name, "Residuals", "Explained", "Unexplained", ""],
                [
                    name,
                    int(residuals_footprint),
                    int(explained_footprint),
                    int(unexplained_footprint),
                    "",
                ],
                [name, "Footprint", "Section", "Symbol", "Explanation"],
            ]
            for l in lines[3:]:
                if l == "":
                    continue
                r = process_reopt_residual_line(l)
                values += [
                    [
                        f"{r.from_address} - {r.to_address}",
                        int(r.footprint),
                        r.section,
                        r.symbol,
                        r.explanation,
                    ]
                ]
            print(values)

            # We add an extra column between every group for visual clarity in
            # the output
            nb_columns = len(values[0]) + 1

            reserved_rows = 10

            first_column = grammatech.get_column_for_options(
                compiler=compiler_tag,
                optimization=optimization_tag,
                pie=pie_tag,
                strip=strip_tag,
                initial_column_offset=4,  # Reserving first 4 columns for stats
                columns_per_configuration=nb_columns,
            )
            if first_column == None:
                break
            last_column = first_column + nb_columns - 1

            first_cell = sheets.get_cell_for_coordinates((reserved_rows + 1, first_column))
            last_cell = sheets.sheets_col(last_column)  # as many rows as needed

            print(f"First cell: {first_cell}")
            print(f"Last cell:  {last_cell}")
            print(f"Values    length: {len(values)}")
            print(f"Values[0] length: {len(values[0])}")

            print(values)

            result = (
                service.spreadsheets()
                .values()
                .update(
                    spreadsheetId=sheets.SPREADSHEET_ID,
                    range=f"Residuals ({binary})!{first_cell}:{last_cell}",
                    valueInputOption="RAW",
                    body={"values": values},
                )
                .execute()
            )

        # binaries = get_binaries(service)

        # print(binaries)

        # result = get_range(service, "B1:CG4")

        # print(result)

        # values = [ [0, 1, 2, 3, 4, 5] ]
        # body = { 'values': values }

        # result = service.spreadsheets().values().update(
        #     spreadsheetId=SPREADSHEET_ID,
        #     range=f"{SHEET_ID}!B5:CG5",
        #     valueInputOption="RAW",
        #     body=body,
        # ).execute()

        # print(f"{result.get('updatedCells')} cells updated.")

    except HttpError as err:
        print(err)


if __name__ == "__main__":
    main()
