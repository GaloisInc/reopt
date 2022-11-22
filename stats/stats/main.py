from __future__ import print_function
from cmath import pi

import os.path
import re
import subprocess

from enum import Enum

from googleapiclient.discovery import build
from googleapiclient.errors import HttpError
from typing import List, Tuple

from stats import grammatech
from stats.grammatech import Binary, Compiler, OptimizationLevel
from stats import sheets


# def get_binaries(service) -> list[str]:
#     return [binary for row in get_range(service, "TODO", "A5:A") for binary in row]


def process_reopt_residual_line(line: str) -> List[str]:
    """
    "Parses" out a residual line of the form:

    abc123 - abc456 (symbol) [section] reason
    """
    match = re.match("([0-9a-f]+ - [0-9a-f]+)(?: \((.*)\))?(?: \[(.*)\])?(?: (.*))?", line)
    if not match:
        raise Exception(
            f"{process_reopt_residual_line.__name__}: Could not RE match {line}"
        )
    # 0 is full match, 1 to 5 (excluded) are groups 1, 2, 3, and 4
    return [match.group(i) or "-" for i in range(1, 5)]


def main():
    """
    TODO
    """

    creds = sheets.get_credentials()

    try:
        service = build("sheets", "v4", credentials=creds)

        which_binary = Binary.ZIP
        (binary_tag, binary) = (which_binary, which_binary.value)

        for (
            compiler_tag,
            optimization_tag,
            pie_tag,
            strip_tag,
        ) in grammatech.ALL_CONFIGURATIONS:
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
            print(completed.stderr)

            nb_columns = 5 # 4 columns, cf. [process_reopt_residual_line], and one for spacing

            column = grammatech.get_column_for_options(
                compiler=compiler_tag,
                optimization=optimization_tag,
                pie=pie_tag,
                strip=strip_tag,
                initial_column_offset=4, # Reserving first 4 columns for stats
                columns_per_configuration=nb_columns,
            )
            if column == None:
                break

            first_cell = sheets.get_cell_for_coordinates((1, column))
            last_cell = sheets.sheets_col(column + nb_columns - 1)  # as many rows as needed

            name = f"{compiler}.{optimization}.{pie}.{strip}"
            values = [[name, 'Symbol', 'Section', 'Reason']] + [
                process_reopt_residual_line(l)
                for l in completed.stdout.decode("utf-8").splitlines()
            ]
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
