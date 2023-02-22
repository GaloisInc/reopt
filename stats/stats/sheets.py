import os

from google.auth.transport.requests import Request
from google.oauth2.credentials import Credentials
from google_auth_oauthlib.flow import InstalledAppFlow
from typing import Tuple

from stats import sheets

# Spreadsheets, read and write
SCOPES = ["https://www.googleapis.com/auth/spreadsheets"]

SPREADSHEET_ID = "1j6Z8LEqqyV8K8TmIybClaJ9EqEI0-2hVKRRCsEBoiZQ"


def sheets_col(col: int) -> str:
    """
    Convert 1-relative column number to Excel-style column label.  Returns an
    empty string on 0.
    """
    if col == 0:
        return ""
    quot, rem = divmod(col - 1, 26)
    return sheets_col(quot) + chr(rem + ord("A"))


def get_range_contents(service, sheet: str, range: str) -> list[list[str]]:
    """
    Returns the contents of the given range for the sheet.  Range must be in A1
    format.  Returns a list of rows of a list of columns.
    """
    return (
        service.spreadsheets()
        .values()
        .get(
            spreadsheetId=SPREADSHEET_ID,
            range=f"{sheet}!{range}",
        )
        .execute()
        .get("values", [])
    )


def get_cell_for_coordinates(p: Tuple[int, int]) -> str:
    (row, col) = p
    return f"{sheets.sheets_col(col)}{row}"


LOCAL_PATH_FOR_CACHED_CREDENTIALS = "token.json"


def get_credentials():
    """
    Get credentials for the given sheet.  Saves them as a local token for future runs.
    """

    creds = None

    if os.path.exists(LOCAL_PATH_FOR_CACHED_CREDENTIALS):
        creds = Credentials.from_authorized_user_file("token.json", SCOPES)

    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file("credentials.json", SCOPES)
            creds = flow.run_local_server()
        # Save the credentials for the next run
        with open("token.json", "w") as token:
            token.write(creds.to_json())

    return creds
